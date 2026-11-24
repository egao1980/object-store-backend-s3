;;;; Edge cases for #4 (credential chain order, partial keys, profiles,
;;;; caching, extension, region, presigned URLs, default http-fn) and #5 (a
;;;; user :http-fn always gets an S3-HTTP-REQUEST). Uses the fixtures in
;;;; defaults-test.lisp.

(in-package #:object-store-backend-s3/tests)

(defun %access-key-of (backend)
  "Credential= access key of a fixed-time signed GET, or the condition."
  (let ((auth (%auth-of backend)))
    (if (stringp auth)
        (let* ((start (+ (search "Credential=" auth) (length "Credential=")))
               (end (position #\/ auth :start start)))
          (subseq auth start end))
        auth)))

;;; #5

(deftest user-http-fn-gets-an-s3-http-request
  (testing "http-protocol is loaded in this image; the injected fn still sees S3-HTTP-REQUEST"
    (let* ((seen nil)
           (s3 (make-s3-backend :endpoint "http://h" :bucket "b"
                                :access-key "A" :secret-key "S"
                                :http-fn (lambda (req) (push req seen) (list 200 #() nil)))))
      (object-store-protocol:get-object s3 "k")
      (ok (find-package :http-protocol))
      (ok (s3-http-request-p (first seen)) (format nil "got ~S" (type-of (first seen)))))))

(deftest plist-http-fn-response
  (let* ((headers (list (cons "etag" "\"p\"")))
         (s3 (make-s3-backend :endpoint "http://h" :bucket "b"
                              :access-key "A" :secret-key "S"
                              :http-fn (lambda (req) (declare (ignore req))
                                         (list :status 200 :body (utf8-octets "pl")
                                               :headers headers)))))
    (ok (equalp (utf8-octets "pl") (object-store-protocol:get-object s3 "k")))
    (ok (string= "\"p\"" (object-store-protocol:object-stat-etag
                          (object-store-protocol:head-object s3 "k"))))))

;;; #4 — default http-fn details

(deftest default-http-fn-keeps-stored-bytes
  (let* ((recorder (make-instance '%recording-http-backend))
         (http-protocol:*http-backend* recorder)
         (s3 (make-s3-backend :endpoint "http://s3.test" :bucket "b"
                              :access-key "AK" :secret-key "SK")))
    (object-store-protocol:put-object s3 "a.txt" "body")
    (let ((req (first (recorded-requests recorder))))
      (ok (eq :put (http-protocol:http-request-method req)))
      (ok (null (http-protocol:http-request-decompress req))
          "no Content-Encoding decoding: S3 returns objects as stored")
      (ok (equalp (utf8-octets "body") (http-protocol:http-request-content req))))))

(deftest default-http-fn-without-a-client-signals
  (if (find-package :cl-stack-http)
      (skip "cl-stack-http is loaded: the default falls back to its :auto backend")
      (let ((http-protocol:*http-backend* nil)
            (s3 (make-s3-backend :endpoint "http://s3.test" :bucket "b"
                                 :access-key "AK" :secret-key "SK")))
        (ok (signals (object-store-protocol:get-object s3 "a.txt")
                     'object-store-protocol:object-store-error)))))

;;; #4 — chain order and shapes

(deftest partial-environment-credentials-signal
  (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV"))
    (ok (typep (%auth-of (make-s3-backend :bucket "b"))
               'object-store-protocol:object-store-error)
        "a key id without a secret is an error, not a fall-through")))

(deftest environment-beats-shared-file
  (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                       ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
    (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
    (ok (equal "AKIDENV" (%access-key-of (make-s3-backend :bucket "b"))))))

(deftest explicit-profile-beats-aws-profile
  (%with-aws-env (home ("AWS_PROFILE" "default"))
    (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
    (ok (equal "AKIDDEV" (%access-key-of (make-s3-backend :bucket "b" :profile "dev"))))
    (ok (equal "AKIDDEFAULT" (%access-key-of (make-s3-backend :bucket "b"))))))

(deftest aws-default-profile-is-read
  (%with-aws-env (home ("AWS_DEFAULT_PROFILE" "dev"))
    (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
    (ok (equal "AKIDDEV" (%access-key-of (make-s3-backend :bucket "b"))))))

(deftest missing-profile-section-signals
  (%with-aws-env (home ("AWS_PROFILE" "nope"))
    (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
    (ok (typep (%auth-of (make-s3-backend :bucket "b"))
               'object-store-protocol:object-store-error))))

(deftest config-file-profile-section
  (testing "~/.aws/config: [profile NAME] after the credentials file"
    (%with-aws-env (home ("AWS_PROFILE" "ci"))
      (%write-file (merge-pathnames ".aws/config" home)
                   "[default]
region = eu-west-1

[profile ci]
aws_access_key_id = AKIDCI
aws_secret_access_key = SECRETCI
")
      (ok (equal "AKIDCI" (%access-key-of (make-s3-backend :bucket "b")))))))

(deftest aws-config-file-variable
  (%with-aws-env (home)
    (let ((file (%write-file (merge-pathnames "cfg.ini" home)
                             "[default]
aws_access_key_id = AKIDCFG
aws_secret_access_key = SECRETCFG
")))
      (setf (uiop:getenv "AWS_CONFIG_FILE") (uiop:native-namestring file))
      (ok (equal "AKIDCFG" (%access-key-of (make-s3-backend :bucket "b")))))))

(deftest resolved-credentials-are-kept-on-the-backend
  (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                       ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
    (let ((s3 (make-s3-backend :bucket "b")))
      (ok (equal "AKIDENV" (%access-key-of s3)))
      (ok (equal "AKIDENV" (s3-backend-access-key s3)) "filled in on first use")
      (setf (uiop:getenv "AWS_ACCESS_KEY_ID") "AKIDLATER")
      (ok (equal "AKIDENV" (%access-key-of s3)) "the chain is not re-read per request"))))

(deftest custom-provider-extends-the-chain
  (%with-aws-env (home)
    (let ((*s3-credential-providers*
            (append *s3-credential-providers*
                    (list (lambda (backend)
                            (declare (ignore backend))
                            (values "AKIDVAULT" "SECRETVAULT" "TOKENVAULT"))))))
      (let ((s3 (make-s3-backend :bucket "b")))
        (ok (equal "AKIDVAULT" (%access-key-of s3)))
        (ok (equal "TOKENVAULT" (s3-backend-session-token s3)))))))

(deftest explicit-session-token-is-signed
  (let* ((req (build-s3-request (make-s3-backend :bucket "b" :access-key "A" :secret-key "S"
                                                 :session-token "TOK")
                                :key "k"))
         (headers (s3-http-request-headers req)))
    (ok (equal "TOK" (cdr (assoc "x-amz-security-token" headers :test #'string-equal))))
    (ok (search "x-amz-security-token" (cdr (assoc "authorization" headers :test #'string-equal))))))

(deftest region-from-environment
  (let ((saved (mapcar (lambda (n) (cons n (uiop:getenv n)))
                       '("AWS_REGION" "AWS_DEFAULT_REGION"))))
    (unwind-protect
         (progn
           (setf (uiop:getenv "AWS_REGION") ""
                 (uiop:getenv "AWS_DEFAULT_REGION") "eu-central-1")
           (ok (equal "eu-central-1" (s3-backend-region (make-s3-backend))))
           (setf (uiop:getenv "AWS_REGION") "ap-south-1")
           (ok (equal "ap-south-1" (s3-backend-region (make-s3-backend))) "AWS_REGION first")
           (ok (equal "us-west-2" (s3-backend-region (make-s3-backend :region "us-west-2")))
               "explicit :region wins")
           (setf (uiop:getenv "AWS_REGION") "" (uiop:getenv "AWS_DEFAULT_REGION") "")
           (ok (equal "us-east-1" (s3-backend-region (make-s3-backend)))))
      (loop for (n . v) in saved do (setf (uiop:getenv n) (or v ""))))))

;;; Presigned URLs (AWS SigV4 query-string example) and temporary credentials

(deftest presign-url-aws-example
  (testing "AWS docs: GET examplebucket/test.txt, expires 86400, 20130524"
    (let* ((s3 (make-s3-backend :endpoint "https://examplebucket.s3.amazonaws.com"
                                :region "us-east-1"
                                :access-key "AKIAIOSFODNN7EXAMPLE"
                                :secret-key "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"))
           (url (object-store-protocol:presign-url s3 "test.txt" :expires 86400
                                                                 :amz-date "20130524T000000Z"
                                                                 :date-stamp "20130524")))
      (ok (search "X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404" url)
          url))))

(deftest presign-url-carries-the-session-token
  (let ((url (object-store-protocol:presign-url
              (make-s3-backend :endpoint "https://h" :bucket "b"
                               :access-key "A" :secret-key "S" :session-token "TOK/+=")
              "k")))
    (ok (search "X-Amz-Security-Token=TOK%2F%2B%3D" url) url)))
