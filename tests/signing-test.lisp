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

(deftest header-value-alist-and-hash-table
  (ok (string= "text/plain"
               (object-store-backend-s3::%header-value
                '(("Content-Type" . "text/plain")) "content-type")))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "content-type" ht) "application/json"
          (gethash "etag" ht) "\"xyz\"")
    (ok (string= "application/json"
                 (object-store-backend-s3::%header-value ht "Content-Type")))
    (ok (string= "\"xyz\""
                 (object-store-backend-s3::%header-value ht "etag")))
    (ok (string= "\"xyz\""
                 (object-store-backend-s3::%etag-from-headers ht)))))

(deftest head-object-hash-table-headers
  "http-protocol RESPONSE-HEADERS is an EQUAL hash-table (dexador shape)."
  (let* ((headers (make-hash-table :test #'equal))
         (s3 (make-s3-backend
              :endpoint "http://127.0.0.1:9000"
              :region "us-east-1"
              :access-key "AKID"
              :secret-key "SECRET"
              :bucket "b"
              :http-fn (lambda (req)
                         (declare (ignore req))
                         (setf (gethash "etag" headers) "\"abc\""
                               (gethash "content-type" headers) "text/plain")
                         (list 200 #() headers)))))
    (let ((stat (object-store-protocol:head-object s3 "k")))
      (ok (object-store-protocol:object-stat-p stat))
      (ok (string= "\"abc\"" (object-store-protocol:object-stat-etag stat)))
      (ok (string= "text/plain"
                   (object-store-protocol:object-stat-content-type stat))))))

;;; More AWS SigV4 examples (same AWS page): PUT Object with a reserved
;;; character in the key, and GET Bucket (ListObjects) with a query.

(deftest sigv4-aws-put-object
  (let* ((body "Welcome to Amazon S3.")
         (payload (object-store-backend-s3::payload-hash body)))
    (ok (string= "44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072" payload))
    (multiple-value-bind (auth headers payload* canon sts sig)
        (sign-s3-request :method "PUT" :path "/test$file.text"
                         :headers '(("host" . "examplebucket.s3.amazonaws.com")
                                    ("date" . "Fri, 24 May 2013 00:00:00 GMT")
                                    ("x-amz-storage-class" . "REDUCED_REDUNDANCY"))
                         :body body
                         :access-key *aws-access* :secret-key *aws-secret*
                         :region "us-east-1"
                         :amz-date "20130524T000000Z" :date-stamp "20130524")
      (declare (ignore auth headers payload* sts))
      (ok (string= "/test%24file.text" (second (uiop:split-string canon :separator '(#\Newline)))))
      (ok (string= "98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd" sig)))))

(deftest sigv4-aws-list-objects
  (multiple-value-bind (auth headers payload canon sts sig)
      (sign-s3-request :method "GET" :path "/"
                       :query '(("max-keys" . "2") ("prefix" . "J"))
                       :headers '(("host" . "examplebucket.s3.amazonaws.com"))
                       :access-key *aws-access* :secret-key *aws-secret*
                       :region "us-east-1"
                       :amz-date "20130524T000000Z" :date-stamp "20130524"
                       :payload-hash *empty-hash*)
    (declare (ignore auth headers payload sts))
    (ok (string= "max-keys=2&prefix=J" (third (uiop:split-string canon :separator '(#\Newline)))))
    (ok (string= "34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7" sig))))

(deftest-parametrize sigv4-uri-encode-non-ascii
    ((input encode-slash expected)
     ("é" t "%C3%A9")
     ("ж" t "%D0%B6")
     ("日本/é.txt" nil "%E6%97%A5%E6%9C%AC/%C3%A9.txt")
     ("a b/c" t "a%20b%2Fc")
     ("a b/c" nil "a%20b/c")
     ("😀" t "%F0%9F%98%80")
     ("Ab0-._~" t "Ab0-._~"))
  (ok (string= expected
               (uri-encode input :encode-slash encode-slash))))

(deftest sigv4-non-ascii-key-request
  (let* ((s3 (make-s3-backend :endpoint "http://127.0.0.1:9000"
                              :access-key "AKID" :secret-key "SECRET" :bucket "b"))
         (req (build-s3-request s3 :method :get :key "日本/é ж.txt"
                                :amz-date "20130524T000000Z" :date-stamp "20130524")))
    (ok (string= "http://127.0.0.1:9000/b/%E6%97%A5%E6%9C%AC/%C3%A9%20%D0%B6.txt"
                 (s3-http-request-url req)))
    (multiple-value-bind (auth headers payload canon)
        (sign-s3-request :method "GET" :path "/b/日本/é ж.txt"
                         :headers '(("host" . "127.0.0.1:9000"))
                         :access-key "AKID" :secret-key "SECRET" :region "us-east-1"
                         :amz-date "20130524T000000Z" :date-stamp "20130524")
      (declare (ignore headers payload))
      (ok (string= "/b/%E6%97%A5%E6%9C%AC/%C3%A9%20%D0%B6.txt"
                   (second (uiop:split-string canon :separator '(#\Newline)))))
      (ok (string= auth (%assoc-val (s3-http-request-headers req) "authorization"))))))

(defun %octets (string) (encoding-protocol:encode string :encoding :utf-8))

(deftest list-objects-parses-listing
  (let* ((xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
  <Name>b</Name><Prefix>d/</Prefix><KeyCount>2</KeyCount><MaxKeys>2</MaxKeys>
  <IsTruncated>true</IsTruncated>
  <NextContinuationToken>1ueGcxLPRx1Tr/XYExHnhbYLgveDs2J/wm36Hy4vbOwM=</NextContinuationToken>
  <Contents><Key>d/é.txt</Key><LastModified>2026-11-22T10:00:00.000Z</LastModified>
    <ETag>&quot;abc&quot;</ETag><Size>12</Size><StorageClass>STANDARD</StorageClass></Contents>
  <Contents><Key>d/a&amp;b</Key><ETag>&quot;def&quot;</ETag><Size>0</Size></Contents>
</ListBucketResult>")
         (seen nil)
         (s3 (make-s3-backend :endpoint "http://127.0.0.1:9000"
                              :access-key "AKID" :secret-key "SECRET" :bucket "b"
                              :http-fn (lambda (req) (push req seen)
                                         (list 200 (%octets xml) nil))))
         (listing (object-store-protocol:list-objects s3 :prefix "d/" :max-keys 2))
         (objects (object-store-protocol:object-listing-objects listing)))
    (ok (= 2 (length objects)))
    (ok (string= "d/é.txt" (object-store-protocol:object-stat-key (first objects))))
    (ok (eql 12 (object-store-protocol:object-stat-size (first objects))))
    (ok (string= "\"abc\"" (object-store-protocol:object-stat-etag (first objects))))
    (ok (string= "d/a&b" (object-store-protocol:object-stat-key (second objects))))
    (ok (object-store-protocol:object-listing-truncated-p listing))
    (ok (string= "1ueGcxLPRx1Tr/XYExHnhbYLgveDs2J/wm36Hy4vbOwM="
                 (object-store-protocol:object-listing-continuation-token listing)))
    (ok (search "list-type=2" (s3-http-request-url (first seen))))))

(deftest list-objects-empty-and-malformed
  (flet ((store (body)
           (make-s3-backend :endpoint "http://h" :access-key "A" :secret-key "S" :bucket "b"
                            :http-fn (lambda (req) (declare (ignore req))
                                       (list 200 (%octets body) nil)))))
    (let ((listing (object-store-protocol:list-objects
                    (store "<ListBucketResult><IsTruncated>false</IsTruncated></ListBucketResult>"))))
      (ok (null (object-store-protocol:object-listing-objects listing)))
      (ng (object-store-protocol:object-listing-truncated-p listing)))
    (ok (signals (object-store-protocol:list-objects (store "<ListBucketResult><Contents>"))
                 'object-store-protocol:object-store-error))))

(deftest multipart-xml-roundtrip
  (let* ((bodies nil)
         (s3 (make-s3-backend
              :endpoint "http://h" :access-key "A" :secret-key "S" :bucket "b"
              :http-fn (lambda (req)
                         (push (s3-http-request-body req) bodies)
                         (list 200 (%octets "<InitiateMultipartUploadResult><Bucket>b</Bucket><Key>k</Key><UploadId>VXBsb2FkIElE&amp;x</UploadId></InitiateMultipartUploadResult>")
                               '(("etag" . "\"e\"")))))))
    (ok (string= "VXBsb2FkIElE&x" (object-store-protocol:create-multipart-upload s3 "k")))
    (object-store-protocol:complete-multipart-upload s3 "k" "id" '((1 . "\"a<b\"") (2 . "\"c\"")))
    (let ((xml (encoding-protocol:decode (first bodies))))
      (ok (search "<PartNumber>1</PartNumber>" xml))
      (ok (search "&lt;" xml))
      (ng (search "a<b" xml)))))
