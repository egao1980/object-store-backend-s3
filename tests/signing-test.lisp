(in-package #:object-store-backend-s3/tests)

;;; AWS S3 GET Object SigV4 example
;;; https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html

(defparameter *aws-access* "AKIAIOSFODNN7EXAMPLE")
(defparameter *aws-secret* "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
(defparameter *empty-hash*
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
(defparameter *aws-canon*
  (format nil "~{~a~%~}"
          '("GET"
            "/test.txt"
            ""
            "host:examplebucket.s3.amazonaws.com"
            "range:bytes=0-9"
            "x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            "x-amz-date:20130524T000000Z"
            ""
            "host;range;x-amz-content-sha256;x-amz-date"
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")))
(defparameter *aws-canon-hash*
  "7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972")
(defparameter *aws-signature*
  "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41")

(deftest sha256-empty
  (ok (string= *empty-hash* (octets-to-hex (sha256 #())))))

(deftest sha256-abc
  (ok (string= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
               (octets-to-hex (sha256 "abc")))))

(deftest hmac-rfc4231-2
  ;; RFC 4231 test case 2 (HMAC-SHA256)
  (ok (string= "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
               (octets-to-hex (hmac-sha256 "Jefe" "what do ya want for nothing?")))))

(deftest canonical-request-aws-get
  (let ((canon (canonical-request
                "GET" "/test.txt" nil
                '(("host" . "examplebucket.s3.amazonaws.com")
                  ("range" . "bytes=0-9")
                  ("x-amz-content-sha256" . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                  ("x-amz-date" . "20130524T000000Z"))
                *empty-hash*)))
    (ok (string= (string-right-trim '(#\Newline) *aws-canon*)
                 (string-right-trim '(#\Newline) canon)))
    (ok (string= *aws-canon-hash* (octets-to-hex (sha256 canon))))))

(deftest string-to-sign-and-signature-aws-get
  (let* ((headers '(("host" . "examplebucket.s3.amazonaws.com")
                    ("range" . "bytes=0-9")
                    ("x-amz-content-sha256" . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                    ("x-amz-date" . "20130524T000000Z")))
         (canon (canonical-request "GET" "/test.txt" nil headers *empty-hash*))
         (hashed (octets-to-hex (sha256 canon)))
         (sts (string-to-sign "20130524T000000Z"
                              "20130524/us-east-1/s3/aws4_request"
                              hashed))
         (sig (signature *aws-secret* "20130524" "us-east-1" sts)))
    (ok (string= *aws-canon-hash* hashed))
    (ok (search "AWS4-HMAC-SHA256" sts))
    (ok (string= *aws-signature* sig))))

(deftest sign-s3-request-authorization-shape
  (multiple-value-bind (auth headers payload)
      (sign-s3-request :method "GET" :path "/test.txt"
                       :headers '(("host" . "examplebucket.s3.amazonaws.com")
                                  ("range" . "bytes=0-9"))
                       :access-key *aws-access* :secret-key *aws-secret*
                       :region "us-east-1"
                       :amz-date "20130524T000000Z" :date-stamp "20130524"
                       :payload-hash *empty-hash*)
    (ok (search "AWS4-HMAC-SHA256" auth))
    (ok (search "Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request" auth))
    (ok (search (format nil "Signature=~a" *aws-signature*) auth))
    (ok (search "AWS4-HMAC-SHA256" (%assoc-val headers "authorization")))
    (ok (string= *empty-hash* payload))))

(defun %assoc-val (alist name)
  (cdr (assoc name alist :test #'string-equal)))

(defun %request-headers (req)
  "http-fn sees s3-http-request, or http-protocol:http-request when that package is loaded."
  (cond
    ((s3-http-request-p req)
     (s3-http-request-headers req))
    (t
     (let* ((pkg (find-package :http-protocol))
            (hdrs (and pkg (find-symbol "HTTP-REQUEST-HEADERS" pkg))))
       (if (and hdrs (fboundp hdrs))
           (funcall hdrs req)
           (error "unknown request type ~s" req))))))

(deftest signature-stable
  (let ((a (signature *aws-secret* "20130524" "us-east-1" "hello"))
        (b (signature *aws-secret* "20130524" "us-east-1" "hello")))
    (ok (string= a b))
    (ok (= 64 (length a)))))

(deftest build-s3-request-has-authorization
  (let* ((s3 (make-s3-backend :endpoint "https://examplebucket.s3.amazonaws.com"
                              :region "us-east-1"
                              :access-key *aws-access*
                              :secret-key *aws-secret*
                              :bucket nil))
         (req (build-s3-request s3 :method :get :key "test.txt"
                                :amz-date "20130524T000000Z"
                                :date-stamp "20130524"
                                :extra-headers
                                '(("range" . "bytes=0-9")
                                  ("host" . "examplebucket.s3.amazonaws.com")))))
    (ok (s3-http-request-p req))
    (ok (eq :get (s3-http-request-method req)))
    (let ((auth (%assoc-val (s3-http-request-headers req) "authorization")))
      (ok (search "AWS4-HMAC-SHA256" auth))
      (ok (search "Signature=" auth)))))

(deftest injected-http-fn-put-get
  (let* ((seen nil)
         (s3 (make-s3-backend
              :endpoint "http://127.0.0.1:9000"
              :region "us-east-1"
              :access-key "AKID"
              :secret-key "SECRET"
              :bucket "b"
              :http-fn (lambda (req)
                         (push req seen)
                         (list 200
                               (object-store-protocol:coerce-object-octets "hi")
                               '(("etag" . "\"abc\"")))))))
    (object-store-protocol:put-object s3 "k" "hi")
    (ok (equalp (object-store-protocol:coerce-object-octets "hi")
                (object-store-protocol:get-object s3 "k")))
    (ok (>= (length seen) 2))
    (ok (search "AWS4-HMAC-SHA256"
                (%assoc-val (%request-headers (first (last seen)))
                            "authorization")))))
