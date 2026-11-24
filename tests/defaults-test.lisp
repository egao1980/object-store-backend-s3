;;;; Evidence for object-store-backend-s3#4: a default HTTP function and a
;;;; credential chain (boto3 shape: env, shared credentials file, profile).
;;;; Each test is the spec a fix must satisfy.

(in-package #:object-store-backend-s3/tests)

;;; Default http-fn: send through the bound http-protocol backend

(defclass %recording-http-backend (http-protocol:http-backend)
  ((requests :initform '() :accessor recorded-requests))
  (:default-initargs :name "recording"))

(defmethod http-protocol:send ((backend %recording-http-backend) client request &key)
  (declare (ignore client))
  (push request (recorded-requests backend))
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "etag" headers) "\"etag-1\"")
    (make-instance 'http-protocol:http-response
                   :status 200
                   :headers headers
                   :body (utf8-octets "hi")
                   :url (http-protocol:http-request-url request))))

(defun %request-header (request name)
  (let ((headers (http-protocol:http-request-headers request)))
    (if (hash-table-p headers)
        (gethash name headers)
        (cdr (assoc name headers :test #'string-equal)))))

(deftest default-http-fn-sends-via-bound-http-backend
  (testing "no :http-fn + a bound http-protocol:*http-backend* → requests go there"
    (let* ((recorder (make-instance '%recording-http-backend))
           (http-protocol:*http-backend* recorder)
           (s3 (make-s3-backend :endpoint "http://s3.test" :bucket "b"
                                :access-key "AK" :secret-key "SK"))
           (body (handler-case (object-store-protocol:get-object s3 "a.txt")
                   (error (e) e))))
      (ok (equalp (utf8-octets "hi") body) (format nil "get-object → ~A" body))
      (ok (= 1 (length (recorded-requests recorder)))
          "the request went through http-protocol:send")
      (let ((req (first (recorded-requests recorder))))
        (when req
          (ok (string= "http://s3.test/b/a.txt" (http-protocol:http-request-url req)))
          (ok (search "Credential=AK/" (or (%request-header req "authorization") ""))
              "SigV4 Authorization header is sent"))))))

;;; Credential chain

(defparameter +aws-env-vars+
  '("AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "AWS_SESSION_TOKEN"
    "AWS_PROFILE" "AWS_DEFAULT_PROFILE"
    "AWS_SHARED_CREDENTIALS_FILE" "AWS_CONFIG_FILE")
  "Environment the chain may read. Empty counts as unset (botocore).")

(defparameter +credentials-file+
  "[default]
aws_access_key_id = AKIDDEFAULT
aws_secret_access_key = SECRETDEFAULT

[dev]
aws_access_key_id = AKIDDEV
aws_secret_access_key = SECRETDEV
aws_session_token = TOKENDEV
")

(defun %temp-dir ()
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "s3-cred-test-~36R"
                                       (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %write-file (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string text s))
  path)

(defun %call-with-aws-env (bindings thunk)
  "THUNK with HOME at an empty temp dir, every +AWS-ENV-VARS+ cleared, then
   BINDINGS ((name . value) …) applied. THUNK gets the temp dir. Restores all."
  (let* ((names (cons "HOME" +aws-env-vars+))
         (saved (mapcar (lambda (n) (cons n (uiop:getenv n))) names))
         (home (%temp-dir)))
    (unwind-protect
         (progn
           (dolist (n +aws-env-vars+) (setf (uiop:getenv n) ""))
           (setf (uiop:getenv "HOME") (uiop:native-namestring home))
           (loop for (n . v) in bindings do (setf (uiop:getenv n) v))
           (funcall thunk home))
      (loop for (n . v) in saved do (setf (uiop:getenv n) (or v "")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(defmacro %with-aws-env ((home &rest bindings) &body body)
  `(%call-with-aws-env (list ,@(loop for (n v) in bindings collect `(cons ,n ,v)))
                       (lambda (,home) (declare (ignorable ,home)) ,@body)))

(defun %auth-of (backend)
  "Authorization header of a fixed-time signed GET, or the condition."
  (handler-case
      (cdr (assoc "authorization"
                  (s3-http-request-headers
                   (build-s3-request backend :key "a.txt"
                                             :amz-date "20130524T000000Z"
                                             :date-stamp "20130524"))
                  :test #'string-equal))
    (error (e) e)))

(defun %explicit-auth (access secret)
  (%auth-of (make-s3-backend :bucket "b" :access-key access :secret-key secret)))

(deftest credentials-from-environment
  (testing "AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY sign when no keys are passed"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
      (let ((got (%auth-of (make-s3-backend :bucket "b"))))
        (ok (equal (%explicit-auth "AKIDENV" "SECRETENV") got)
            (format nil "Authorization = ~A" got))))))

(deftest session-token-from-environment-is-signed
  (testing "AWS_SESSION_TOKEN → signed x-amz-security-token header"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV")
                         ("AWS_SESSION_TOKEN" "TOKENENV"))
      (let* ((req (handler-case (build-s3-request (make-s3-backend :bucket "b") :key "a.txt")
                    (error (e) e)))
             (headers (and (s3-http-request-p req) (s3-http-request-headers req)))
             (auth (cdr (assoc "authorization" headers :test #'string-equal))))
        (ok (equal "TOKENENV"
                   (cdr (assoc "x-amz-security-token" headers :test #'string-equal)))
            (format nil "x-amz-security-token in ~S" headers))
        (ok (and auth (search "x-amz-security-token" auth))
            "the token header is in SignedHeaders")))))

(deftest credentials-from-shared-file-and-profile
  (testing "AWS_SHARED_CREDENTIALS_FILE + AWS_PROFILE select a profile (with its token)"
    (%with-aws-env (home)
                   (let ((file (%write-file (merge-pathnames "creds.ini" home) +credentials-file+)))
                     (setf (uiop:getenv "AWS_SHARED_CREDENTIALS_FILE") (uiop:native-namestring file)
                           (uiop:getenv "AWS_PROFILE") "dev")
                     (let* ((req (handler-case (build-s3-request (make-s3-backend :bucket "b") :key "a.txt")
                                   (error (e) e)))
                            (headers (and (s3-http-request-p req) (s3-http-request-headers req)))
                            (auth (cdr (assoc "authorization" headers :test #'string-equal))))
                       (ok (and auth (search "Credential=AKIDDEV/" auth))
                           (format nil "Authorization = ~A" (or auth req)))
                       (ok (equal "TOKENDEV"
                                  (cdr (assoc "x-amz-security-token" headers :test #'string-equal)))
                           "the profile's aws_session_token is sent"))))))

(deftest credentials-from-home-aws-credentials
  (testing "~/.aws/credentials [default] when nothing else is set"
    (%with-aws-env (home)
      (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
      (let ((got (%auth-of (make-s3-backend :bucket "b"))))
        (ok (equal (%explicit-auth "AKIDDEFAULT" "SECRETDEFAULT") got)
            (format nil "Authorization = ~A" got))))))

(deftest explicit-keys-win-over-environment
  (testing ":access-key / :secret-key beat the environment"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
      (ok (equal (%explicit-auth "AKIDARG" "SECRETARG")
                 (%auth-of (make-s3-backend :bucket "b"
                                            :access-key "AKIDARG"
                                            :secret-key "SECRETARG")))))))

(deftest missing-credentials-signal
  (testing "no keys anywhere → a condition, not a request signed as Credential=NIL"
    (%with-aws-env (home)
      (let ((got (%auth-of (make-s3-backend :bucket "b"))))
        (ok (typep got 'object-store-protocol:object-store-error)
            (format nil "got ~A" got))))))
