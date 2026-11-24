;;;; #10: head-object read only ETag and Content-Type, so its size was always
;;;; make-object-stat's default 0 and Last-Modified was lost. Found by the
;;;; MinIO demo (cl-stack-demos harness/s3-minio): a 26-octet object had
;;;; size 0.

(in-package #:object-store-backend-s3/tests)

(defun %head-with-headers (headers)
  "HEAD-OBJECT on a backend whose http-fn answers 200 with HEADERS."
  (object-store-protocol:head-object
   (make-s3-backend :endpoint "http://127.0.0.1:9000" :region "us-east-1"
                    :access-key "AKID" :secret-key "SECRET" :bucket "b"
                    :http-fn (lambda (req)
                               (declare (ignore req))
                               (list 200 #() headers)))
   "k"))

(deftest head-object-size-and-last-modified
  (testing "hash-table headers (dexador): Content-Length 26 and Last-Modified"
    (let ((headers (make-hash-table :test #'equal)))
      (setf (gethash "content-length" headers) "26"
            (gethash "etag" headers) "\"d2b656fc5d3ed7f80ba49ab734de50e9\""
            (gethash "content-type" headers) "text/plain"
            (gethash "last-modified" headers) "Tue, 24 Nov 2026 05:41:58 GMT")
      (let ((stat (%head-with-headers headers)))
        (ok (eql 26 (object-store-protocol:object-stat-size stat))
            (format nil "size ~s" (object-store-protocol:object-stat-size stat)))
        (ok (equal "Tue, 24 Nov 2026 05:41:58 GMT"
                   (object-store-protocol:object-stat-last-modified stat)))
        (ok (equal "\"d2b656fc5d3ed7f80ba49ab734de50e9\""
                   (object-store-protocol:object-stat-etag stat)))
        (ok (equal "text/plain" (object-store-protocol:object-stat-content-type stat))))))
  (testing "alist headers in any case, and an integer Content-Length"
    (ok (eql 6291579
             (object-store-protocol:object-stat-size
              (%head-with-headers '(("Content-Length" . "6291579"))))))
    (ok (eql 5 (object-store-protocol:object-stat-size
                (%head-with-headers '(("content-length" . 5)))))))
  (testing "no Content-Length: size 0, Last-Modified NIL"
    (let ((stat (%head-with-headers '(("etag" . "\"x\"")))))
      (ok (eql 0 (object-store-protocol:object-stat-size stat)))
      (ok (null (object-store-protocol:object-stat-last-modified stat))))))
