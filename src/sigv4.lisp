(in-package #:object-store-backend-s3)

;;; AWS Signature Version 4 (header-based). Testable primitives:
;;; CANONICAL-REQUEST, STRING-TO-SIGN, SIGNATURE, AUTHORIZATION-HEADER.

(defun uri-encode (string &key (encode-slash t))
  "RFC 3986 percent-encoding used by SigV4. Unreserved: A-Z a-z 0-9 - . _ ~."
  (with-output-to-string (out)
    (loop for c across (string string)
          for code = (char-code c)
          do (cond
               ((or (alphanumericp c) (find c "-._~"))
                (write-char c out))
               ((and (char= c #\/) (not encode-slash))
                (write-char c out))
               (t (format out "%~2,'0X" code))))))

(defun uri-encode-path (path)
  (if (or (null path) (string= path ""))
      "/"
      (uri-encode path :encode-slash nil)))

(defun %normalize-headers (headers)
  "HEADERS is an alist (name . value). Names lowercased, values trimmed, sorted."
  (let ((pairs (mapcar (lambda (h)
                         (cons (string-downcase
                                (etypecase (car h)
                                  (string (car h))
                                  (symbol (symbol-name (car h)))))
                               (string-trim '(#\Space #\Tab) (princ-to-string (cdr h)))))
                       headers)))
    (sort (copy-list pairs) #'string< :key #'car)))

(defun canonical-query (query)
  "QUERY is an alist. Sorted, URI-encoded keys and values."
  (if (null query)
      ""
      (format nil "~{~a~^&~}"
              (mapcar (lambda (pair)
                        (format nil "~a=~a"
                                (uri-encode (string (car pair)))
                                (uri-encode (if (cdr pair)
                                                (princ-to-string (cdr pair))
                                                ""))))
                      (sort (copy-list query) #'string<
                            :key (lambda (p) (string (car p))))))))

(defun canonical-request (method path query headers payload-hash)
  "SigV4 canonical request string.
   METHOD is a string (GET/PUT/…). PATH is the URI path.
   QUERY is an alist or a preformatted string.
   HEADERS is an alist. PAYLOAD-HASH is lowercase hex SHA-256."
  (let* ((method (string-upcase (string method)))
         (path (uri-encode-path path))
         (qs (if (stringp query) query (canonical-query query)))
         (norm (%normalize-headers headers))
         (canonical-headers
          (format nil "~{~a:~a~%~}"
                  (loop for (n . v) in norm
                        collect n collect v)))
         (signed (format nil "~{~a~^;~}" (mapcar #'car norm))))
    (format nil "~a~%~a~%~a~%~a~%~a~%~a"
            method path qs canonical-headers signed payload-hash)))

(defun string-to-sign (amz-date credential-scope hashed-canonical-request)
  (format nil "AWS4-HMAC-SHA256~%~a~%~a~%~a"
          amz-date credential-scope hashed-canonical-request))

(defun credential-scope (date-stamp region service)
  (format nil "~a/~a/~a/aws4_request" date-stamp region service))

(defun signing-key (secret-key date-stamp region &optional (service "s3"))
  "Derive kSigning = HMAC(HMAC(HMAC(HMAC(AWS4||secret, date), region), service), aws4_request)."
  (let* ((k-date (hmac-sha256 (concatenate 'string "AWS4" secret-key) date-stamp))
         (k-region (hmac-sha256 k-date region))
         (k-service (hmac-sha256 k-region service)))
    (hmac-sha256 k-service "aws4_request")))

(defun signature (secret-key date-stamp region string-to-sign
                  &optional (service "s3"))
  "Hex HMAC-SHA256 of STRING-TO-SIGN under the derived signing key."
  (octets-to-hex
   (hmac-sha256 (signing-key secret-key date-stamp region service)
                string-to-sign)))

(defun authorization-header (access-key date-stamp region signed-headers
                             signature-hex &optional (service "s3"))
  (format nil "AWS4-HMAC-SHA256 Credential=~a/~a,SignedHeaders=~a,Signature=~a"
          access-key
          (credential-scope date-stamp region service)
          signed-headers
          signature-hex))

(defun %signed-headers-string (headers)
  (format nil "~{~a~^;~}" (mapcar #'car (%normalize-headers headers))))

(defun empty-payload-hash ()
  (octets-to-hex (sha256 #())))

(defun payload-hash (body)
  (octets-to-hex
   (sha256 (if (or (null body) (and (vectorp body) (zerop (length body))))
               (make-array 0 :element-type '(unsigned-byte 8))
               (utf8-octets body)))))

(defun sign-s3-request (&key method path query headers body
                          access-key secret-key region
                          amz-date date-stamp
                          (service "s3")
                          (payload-hash nil payload-hash-p))
  "Return (values authorization-header signed-headers-alist payload-hash).
   AMZ-DATE is like 20130524T000000Z; DATE-STAMP is 20130524."
  (let* ((payload (if payload-hash-p
                      payload-hash
                      (payload-hash body)))
         (hdrs (copy-list headers))
         (hdrs (acons "x-amz-date" amz-date
                      (remove "x-amz-date" hdrs :key #'car :test #'string-equal)))
         (hdrs (acons "x-amz-content-sha256" payload
                      (remove "x-amz-content-sha256" hdrs
                              :key #'car :test #'string-equal)))
         (canon (canonical-request method path query hdrs payload))
         (hashed (octets-to-hex (sha256 canon)))
         (scope (credential-scope date-stamp region service))
         (sts (string-to-sign amz-date scope hashed))
         (sig (signature secret-key date-stamp region sts service))
         (signed (%signed-headers-string hdrs))
         (auth (authorization-header access-key date-stamp region signed sig service)))
    (values auth
            (acons "authorization" auth hdrs)
            payload
            canon
            sts
            sig)))
