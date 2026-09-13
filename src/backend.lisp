(in-package #:object-store-backend-s3)

(defstruct s3-http-request
  method
  url
  headers
  body
  query)

(defclass s3-backend (object-store)
  ((endpoint :initarg :endpoint :accessor s3-backend-endpoint
             :initform "https://s3.amazonaws.com")
   (region :initarg :region :accessor s3-backend-region :initform "us-east-1")
   (access-key :initarg :access-key :accessor s3-backend-access-key)
   (secret-key :initarg :secret-key :accessor s3-backend-secret-key)
   (bucket :initarg :bucket :accessor s3-backend-bucket :initform nil)
   (http-fn :initarg :http-fn :accessor s3-backend-http-fn :initform nil)
   (service :initarg :service :accessor s3-backend-service :initform "s3")))

(defun make-s3-backend (&key endpoint region access-key secret-key bucket http-fn)
  (make-instance 's3-backend
                 :endpoint (or endpoint "https://s3.amazonaws.com")
                 :region (or region "us-east-1")
                 :access-key access-key
                 :secret-key secret-key
                 :bucket bucket
                 :http-fn http-fn))

(defun use-s3-backend (&rest args &key &allow-other-keys)
  (setf object-store-protocol:*object-store* (apply #'make-s3-backend args)))

(defun %strip-scheme-host (endpoint)
  (let* ((s (string-right-trim "/" endpoint))
         (scheme-end (search "://" s))
         (rest (if scheme-end (subseq s (+ scheme-end 3)) s))
         (slash (position #\/ rest)))
    (values (if slash (subseq rest 0 slash) rest)
            (if slash (subseq rest slash) ""))))

(defun %object-path (backend key)
  (let ((bucket (s3-backend-bucket backend)))
    (if bucket
        (format nil "/~a/~a" bucket (string-left-trim "/" key))
        (format nil "/~a" (string-left-trim "/" key)))))

(defun %host (backend)
  (nth-value 0 (%strip-scheme-host (s3-backend-endpoint backend))))

(defun %amz-now ()
  "UTC timestamp pair (amz-date date-stamp). Uses GET-UNIVERSAL-TIME."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (values (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
                    year month date hour min sec)
            (format nil "~4,'0d~2,'0d~2,'0d" year month date))))

(defun build-s3-request (backend &key (method :get) key query body
                                 extra-headers
                                 amz-date date-stamp
                                 content-type)
  "Build a signed S3-HTTP-REQUEST. Does not send it."
  (multiple-value-bind (auto-amz auto-date) (%amz-now)
    (let* ((amz-date (or amz-date auto-amz))
           (date-stamp (or date-stamp auto-date))
           (path (%object-path backend (or key "")))
           (host (%host backend))
           (headers (copy-list extra-headers)))
      (unless (assoc "host" headers :test #'string-equal)
        (push (cons "host" host) headers))
      (when content-type
        (push (cons "content-type" content-type) headers))
      (multiple-value-bind (auth signed-headers payload)
          (sign-s3-request :method method :path path :query query
                           :headers headers :body body
                           :access-key (s3-backend-access-key backend)
                           :secret-key (s3-backend-secret-key backend)
                           :region (s3-backend-region backend)
                           :amz-date amz-date :date-stamp date-stamp
                           :service (s3-backend-service backend))
        (declare (ignore auth payload))
        (let* ((qs (canonical-query query))
               (base (string-right-trim "/" (s3-backend-endpoint backend)))
               (url (if (plusp (length qs))
                        (format nil "~a~a?~a" base path qs)
                        (format nil "~a~a" base path))))
          (make-s3-http-request :method (intern (string-upcase (string method)) :keyword)
                                :url url
                                :headers signed-headers
                                :body body
                                :query query))))))

(defun %header-alist (req)
  (s3-http-request-headers req))

(defun %header-value (headers name)
  "Look up NAME in an alist or EQUAL hash-table (http-protocol lowercase keys)."
  (let ((want (string-downcase (string name))))
    (cond
      ((null headers) nil)
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash want headers)
           (loop for k being the hash-keys of headers using (hash-value v)
                 when (and (or (stringp k) (symbolp k))
                           (string-equal (string k) want))
                   return v)))
      ((consp headers)
       (cdr (assoc name headers :test #'string-equal)))
      (t nil))))

(defun %as-http-request (req)
  "Soft-use http-protocol:MAKE-HTTP-REQUEST when that system is loaded."
  (let* ((pkg (find-package :http-protocol))
         (make (and pkg (find-symbol "MAKE-HTTP-REQUEST" pkg))))
    (if (and make (fboundp make))
        (funcall make
                 :method (s3-http-request-method req)
                 :url (s3-http-request-url req)
                 :headers (s3-http-request-headers req)
                 :content (s3-http-request-body req))
        req)))

(defun %invoke-http (backend req)
  (let ((fn (s3-backend-http-fn backend)))
    (unless fn
      (error 'object-store-error
             :message "s3-backend has no http-fn — inject one or load an HTTP client"))
    (funcall fn (%as-http-request req))))

(defun %response-status (response)
  (cond
    ((integerp response) response)
    ((and (consp response) (integerp (car response))) (car response))
    (t
     (let* ((pkg (find-package :http-protocol))
            (st (and pkg (find-symbol "RESPONSE-STATUS" pkg))))
       (if (and st (fboundp st))
           (funcall st response)
           (getf (if (listp response) response nil) :status 200))))))

(defun %response-body (response)
  (cond
    ((vectorp response) response)
    ((stringp response) (utf8-octets response))
    ((and (consp response) (cdr response)) (cadr response))
    (t
     (let* ((pkg (find-package :http-protocol))
            (body (and pkg (find-symbol "RESPONSE-BODY" pkg))))
       (if (and body (fboundp body))
           (let ((b (funcall body response)))
             (if (stringp b) (utf8-octets b) b))
           (getf (if (listp response) response nil) :body #()))))))

(defun %response-headers (response)
  (cond
    ((and (consp response) (cddr response)) (caddr response))
    (t
     (let* ((pkg (find-package :http-protocol))
            (hdrs (and pkg (find-symbol "RESPONSE-HEADERS" pkg))))
       (if (and hdrs (fboundp hdrs))
           (funcall hdrs response)
           (getf (if (listp response) response nil) :headers nil))))))

(defun %etag-from-headers (headers)
  (%header-value headers "etag"))

(defun %raise-http (backend key status body)
  (cond
    ((= status 404)
     (error 'object-not-found :key key :store backend
            :message (format nil "S3 404 ~a" key)))
    ((= status 412)
     (error 'object-precondition-failed :key key :store backend
            :message "S3 412 precondition failed"))
    ((>= status 400)
     (error 'object-store-error :key key :store backend
            :message (format nil "S3 HTTP ~a~@[ ~a~]" status
                             (if (vectorp body)
                                 (map 'string #'code-char body)
                                 body))))
    (t nil)))

(defmethod put-object ((store s3-backend) key data &key content-type metadata
                       if-match if-none-match)
  (call-with-object-store-retry
   (lambda ()
     (let* ((bytes (coerce-object-octets data))
            (extra (copy-list metadata)))
       (when if-match
         (push (cons "if-match" if-match) extra))
       (when if-none-match
         (push (cons "if-none-match" (if (eq if-none-match t) "*" if-none-match))
               extra))
       (let* ((req (build-s3-request store :method :put :key key :body bytes
                                     :content-type content-type
                                     :extra-headers extra))
              (resp (%invoke-http store req))
              (status (%response-status resp)))
         (%raise-http store key status (%response-body resp))
         (make-object-stat :key key
                           :size (length bytes)
                           :etag (%etag-from-headers (%response-headers resp))
                           :content-type (or content-type "application/octet-stream")))))))

(defmethod get-object ((store s3-backend) key &key stream)
  (call-with-object-store-retry
   (lambda ()
     (let* ((req (build-s3-request store :method :get :key key))
            (resp (%invoke-http store req))
            (status (%response-status resp))
            (body (%response-body resp)))
       (when (= status 404)
         (restart-case
             (error 'object-not-found :key key :store store
                    :message (format nil "S3 404 ~a" key))
           (use-value (value)
             :report "Use a supplied object body"
             (return-from get-object (emit-object-body value :stream stream)))))
       (%raise-http store key status body)
       (emit-object-body (or body #()) :stream stream)))))

(defmethod delete-object ((store s3-backend) key &key)
  (call-with-object-store-retry
   (lambda ()
     (let* ((req (build-s3-request store :method :delete :key key))
            (resp (%invoke-http store req))
            (status (%response-status resp)))
       (when (= status 404)
         (restart-case
             (error 'object-not-found :key key :store store)
           (use-value (value)
             :report "Treat as deleted"
             (return-from delete-object value))))
       (%raise-http store key status (%response-body resp))
       t))))

(defmethod head-object ((store s3-backend) key &key)
  (call-with-object-store-retry
   (lambda ()
     (let* ((req (build-s3-request store :method :head :key key))
            (resp (%invoke-http store req))
            (status (%response-status resp)))
       (when (= status 404)
         (restart-case
             (error 'object-not-found :key key :store store)
           (use-value (value)
             :report "Use a supplied object-stat"
             (return-from head-object value))))
       (%raise-http store key status (%response-body resp))
       (let ((headers (%response-headers resp)))
         (make-object-stat
          :key key
          :etag (%etag-from-headers headers)
          :content-type (or (%header-value headers "content-type")
                            "application/octet-stream")))))))

(defmethod list-objects ((store s3-backend) &key prefix continuation-token max-keys)
  (let* ((query (append (when prefix (list (cons "prefix" prefix)))
                        (when continuation-token
                          (list (cons "continuation-token" continuation-token)))
                        (when max-keys
                          (list (cons "max-keys" (princ-to-string max-keys))))
                        (list (cons "list-type" "2"))))
         (req (build-s3-request store :method :get :key "" :query query))
         (resp (%invoke-http store req))
         (status (%response-status resp)))
    (%raise-http store nil status (%response-body resp))
    (make-object-listing :objects nil :continuation-token nil :truncated-p nil)))

(defmethod presign-url ((store s3-backend) key &key (method :get) (expires 3600))
  (multiple-value-bind (amz-date date-stamp) (%amz-now)
    (let* ((path (%object-path store key))
           (host (%host store))
           (scope (credential-scope date-stamp (s3-backend-region store) "s3"))
           (query (list (cons "X-Amz-Algorithm" "AWS4-HMAC-SHA256")
                        (cons "X-Amz-Credential"
                              (format nil "~a/~a"
                                      (s3-backend-access-key store) scope))
                        (cons "X-Amz-Date" amz-date)
                        (cons "X-Amz-Expires" (princ-to-string expires))
                        (cons "X-Amz-SignedHeaders" "host")))
           (headers (list (cons "host" host)))
           (canon (canonical-request method path query headers (empty-payload-hash)))
           (hashed (octets-to-hex (sha256 canon)))
           (sts (string-to-sign amz-date scope hashed))
           (sig (signature (s3-backend-secret-key store) date-stamp
                           (s3-backend-region store) sts))
           (qs (canonical-query (acons "X-Amz-Signature" sig query)))
           (base (string-right-trim "/" (s3-backend-endpoint store))))
      (format nil "~a~a?~a" base path qs))))

(defmethod create-multipart-upload ((store s3-backend) key &key content-type metadata)
  (let* ((query (list (cons "uploads" "")))
         (req (build-s3-request store :method :post :key key :query query
                                :content-type content-type
                                :extra-headers metadata))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    (let ((body (map 'string #'code-char (%response-body resp))))
      (let ((start (search "<UploadId>" body))
            (end (search "</UploadId>" body)))
        (if (and start end)
            (subseq body (+ start 10) end)
            body)))))

(defmethod upload-part ((store s3-backend) key upload-id part-number data &key)
  (let* ((bytes (coerce-object-octets data))
         (query (list (cons "partNumber" (princ-to-string part-number))
                      (cons "uploadId" upload-id)))
         (req (build-s3-request store :method :put :key key :query query :body bytes))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    (or (%etag-from-headers (%response-headers resp)) "")))

(defmethod complete-multipart-upload ((store s3-backend) key upload-id parts &key)
  (let* ((xml (with-output-to-string (o)
                (write-string "<CompleteMultipartUpload>" o)
                (dolist (p parts)
                  (let ((n (car p))
                        (etag (if (consp (cdr p)) (cadr p) (cdr p))))
                    (format o "<Part><PartNumber>~a</PartNumber><ETag>~a</ETag></Part>"
                            n etag)))
                (write-string "</CompleteMultipartUpload>" o)))
         (query (list (cons "uploadId" upload-id)))
         (req (build-s3-request store :method :post :key key :query query
                                :body (utf8-octets xml)
                                :content-type "application/xml"))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    (make-object-stat :key key :etag (%etag-from-headers (%response-headers resp)))))

(defmethod abort-multipart-upload ((store s3-backend) key upload-id &key)
  (let* ((query (list (cons "uploadId" upload-id)))
         (req (build-s3-request store :method :delete :key key :query query))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    t))
