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
   (access-key :initarg :access-key :accessor s3-backend-access-key :initform nil
               :documentation "Explicit, or filled in from the credential chain on first use.")
   (secret-key :initarg :secret-key :accessor s3-backend-secret-key :initform nil)
   (session-token :initarg :session-token :accessor s3-backend-session-token
                  :initform nil
                  :documentation "Temporary-credential token, sent as x-amz-security-token.")
   (profile :initarg :profile :accessor s3-backend-profile :initform nil
            :documentation "Shared-file profile for the credential chain (NIL: AWS_PROFILE, AWS_DEFAULT_PROFILE, \"default\").")
   (bucket :initarg :bucket :accessor s3-backend-bucket :initform nil)
   (http-fn :initarg :http-fn :accessor s3-backend-http-fn :initform nil
            :documentation "Function of one S3-HTTP-REQUEST → response; NIL sends through http-protocol (SEND-WITH-HTTP-PROTOCOL).")
   (service :initarg :service :accessor s3-backend-service :initform "s3")))

(defun make-s3-backend (&key endpoint region access-key secret-key session-token
                             profile bucket http-fn)
  "An S3-BACKEND (boto3 client shape).

   ACCESS-KEY / SECRET-KEY (+ SESSION-TOKEN): explicit credentials. Without
   them the first request resolves *S3-CREDENTIAL-PROVIDERS*: AWS_ACCESS_KEY_ID
   / AWS_SECRET_ACCESS_KEY / AWS_SESSION_TOKEN, then PROFILE (else AWS_PROFILE,
   AWS_DEFAULT_PROFILE, \"default\") in the shared credentials file
   (AWS_SHARED_CREDENTIALS_FILE, else ~/.aws/credentials), then the config
   file (AWS_CONFIG_FILE, else ~/.aws/config). See S3-BACKEND-CREDENTIALS.
   REGION: explicit, else AWS_REGION, AWS_DEFAULT_REGION, \"us-east-1\".
   HTTP-FN: function of one S3-HTTP-REQUEST → response (a list
   (status body headers) or an http-protocol:http-response); NIL sends
   through http-protocol (SEND-WITH-HTTP-PROTOCOL)."
  (make-instance 's3-backend
                 :endpoint (or endpoint "https://s3.amazonaws.com")
                 :region (or region (%env "AWS_REGION") (%env "AWS_DEFAULT_REGION")
                             "us-east-1")
                 :access-key access-key
                 :secret-key secret-key
                 :session-token session-token
                 :profile profile
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
  "Build a signed S3-HTTP-REQUEST. Does not send it. Credentials come from
   S3-BACKEND-CREDENTIALS (explicit, else the chain); a session token is
   sent as the signed x-amz-security-token header."
  (multiple-value-bind (access-key secret-key session-token)
      (s3-backend-credentials backend)
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
        (when session-token
          (setf headers (acons "x-amz-security-token" session-token
                               (remove "x-amz-security-token" headers
                                       :key #'car :test #'string-equal))))
        (multiple-value-bind (auth signed-headers payload)
            (sign-s3-request :method method :path path :query query
                             :headers headers :body body
                             :access-key access-key
                             :secret-key secret-key
                             :region (s3-backend-region backend)
                             :amz-date amz-date :date-stamp date-stamp
                             :service (s3-backend-service backend))
          (declare (ignore auth payload))
          (let* ((qs (canonical-query query))
                 (base (string-right-trim "/" (s3-backend-endpoint backend)))
                 (url (if (plusp (length qs))
                          (format nil "~a~a?~a" base (uri-encode-path path) qs)
                          (format nil "~a~a" base (uri-encode-path path)))))
            (make-s3-http-request :method (intern (string-upcase (string method)) :keyword)
                                  :url url
                                  :headers signed-headers
                                  :body body
                                  :query query)))))))

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

(defun %default-http-backend ()
  "The http-protocol backend the default http-fn sends with: the bound
   http-protocol:*http-backend*, else cl-stack-http's ENSURE-HTTP-BACKEND
   (its :auto order) when that facade is loaded."
  (or http-protocol:*http-backend*
      (let* ((pkg (find-package :cl-stack-http))
             (ensure (and pkg (find-symbol "ENSURE-HTTP-BACKEND" pkg))))
        (and ensure (fboundp ensure) (funcall ensure)))
      (error 'object-store-error
             :message "S3: no HTTP client. Bind http-protocol:*http-backend* (for example (setf http-protocol:*http-backend* (cl-stack-http:ensure-http-backend))), load cl-stack-http, or pass :http-fn")))

(defun send-with-http-protocol (request)
  "The default http-fn: send the S3-HTTP-REQUEST through http-protocol (the
   bound *HTTP-BACKEND* / *HTTP-CLIENT*, else cl-stack-http's :auto backend).
   → http-protocol:http-response. The body is kept as S3 stored it (no
   Content-Encoding decoding); 4xx/5xx come back as responses."
  (check-type request s3-http-request)
  (http:request (s3-http-request-method request)
                (s3-http-request-url request)
                :backend (%default-http-backend)
                :headers (s3-http-request-headers request)
                :content (s3-http-request-body request)
                :decompress nil
                :raise-for-status nil))

(defun %invoke-http (backend req)
  "Send REQ (an S3-HTTP-REQUEST) with BACKEND's http-fn, else
   SEND-WITH-HTTP-PROTOCOL. A user http-fn always gets the S3-HTTP-REQUEST,
   whatever else the image loaded (#5)."
  (funcall (or (s3-backend-http-fn backend) #'send-with-http-protocol) req))

(defun %response-status (response)
  "Status of an http-fn RESPONSE: an http-protocol:http-response, an integer,
   (status body headers), or a plist with :STATUS (default 200)."
  (cond
   ((typep response 'http-protocol:http-response)
    (http-protocol:response-status response))
   ((integerp response) response)
   ((and (consp response) (integerp (car response))) (car response))
   (t (getf (if (listp response) response nil) :status 200))))

(defun %response-body (response)
  (cond
   ((typep response 'http-protocol:http-response)
    (let ((b (http-protocol:response-body response)))
      (if (stringp b) (utf8-octets b) b)))
   ((stringp response) (utf8-octets response))
   ((vectorp response) response)
   ((and (consp response) (integerp (car response))) (cadr response))
   (t (getf (if (listp response) response nil) :body #()))))

(defun %response-headers (response)
  (cond
   ((typep response 'http-protocol:http-response)
    (http-protocol:response-headers response))
   ((and (consp response) (integerp (car response))) (caddr response))
   (t (getf (if (listp response) response nil) :headers nil))))

(defun %etag-from-headers (headers)
  (%header-value headers "etag"))

(defun %body-text (body)
  "Response BODY as text: UTF-8 (encoding-protocol), or NIL when it is not
   valid UTF-8 (error messages must not fail on a binary body)."
  (cond ((stringp body) body)
        ((and (vectorp body) (plusp (length body)))
         (handler-case (encoding-protocol:decode (utf8-octets body) :encoding :utf-8)
           (encoding-protocol:encoding-error () nil)))
        (t nil)))

(defun %parse-xml (store key body)
  "Parse an S3 XML response BODY (octets) with xml-protocol → root element."
  (handler-case (xml-protocol:document-root-element
                 (xml-protocol:decode-octets (utf8-octets body)))
    (xml-protocol:xml-error (e)
      (error 'object-store-error :key key :store store
             :message (format nil "S3: malformed XML response: ~A" e)))))

(defun %child-text (element name)
  (let ((child (and element (xml-protocol:xml-child element name))))
    (and child (xml-protocol:xml-element-text child))))

(defun parse-list-objects-v2 (root)
  "ListBucketResult element → OBJECT-LISTING (Contents, IsTruncated,
   NextContinuationToken)."
  (make-object-listing
   :objects (loop for c in (xml-protocol:xml-children-named root "Contents")
                  collect (let ((size (%child-text c "Size")))
                            (make-object-stat
                             :key (%child-text c "Key")
                             :size (and size (parse-integer size :junk-allowed t))
                             :etag (%child-text c "ETag")
                             :last-modified (%child-text c "LastModified"))))
   :continuation-token (%child-text root "NextContinuationToken")
   :truncated-p (let ((s (%child-text root "IsTruncated")))
                  (and s (string-equal (string-trim " " s) "true") t))))

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
                             (%body-text body))))
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
       ;; An empty object: no body, or an empty vector of any element type
       ;; (#() is not octets for COERCE-OBJECT-OCTETS).
       (emit-object-body (if (and body (plusp (length body)))
                             body
                             (make-array 0 :element-type '(unsigned-byte 8)))
                         :stream stream)))))

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
    (parse-list-objects-v2 (%parse-xml store nil (%response-body resp)))))

(defmethod presign-url ((store s3-backend) key &key (method :get) (expires 3600)
                        amz-date date-stamp)
  "SigV4 query-string URL for KEY. The payload hash is UNSIGNED-PAYLOAD (a
   presigned URL cannot know the body); temporary credentials add
   X-Amz-Security-Token. AMZ-DATE / DATE-STAMP fix the signing time (tests)."
  (multiple-value-bind (access-key secret-key session-token)
      (s3-backend-credentials store)
    (multiple-value-bind (auto-amz auto-date) (%amz-now)
      (let* ((amz-date (or amz-date auto-amz))
             (date-stamp (or date-stamp auto-date))
             (path (%object-path store key))
             (host (%host store))
             (scope (credential-scope date-stamp (s3-backend-region store) "s3"))
             (query (append
                     (list (cons "X-Amz-Algorithm" "AWS4-HMAC-SHA256")
                           (cons "X-Amz-Credential"
                                 (format nil "~a/~a" access-key scope))
                           (cons "X-Amz-Date" amz-date)
                           (cons "X-Amz-Expires" (princ-to-string expires)))
                     (when session-token
                       (list (cons "X-Amz-Security-Token" session-token)))
                     (list (cons "X-Amz-SignedHeaders" "host"))))
             (headers (list (cons "host" host)))
             (canon (canonical-request method path query headers "UNSIGNED-PAYLOAD"))
             (hashed (octets-to-hex (sha256 canon)))
             (sts (string-to-sign amz-date scope hashed))
             (sig (signature secret-key date-stamp (s3-backend-region store) sts))
             (qs (canonical-query (acons "X-Amz-Signature" sig query)))
             (base (string-right-trim "/" (s3-backend-endpoint store))))
        (format nil "~a~a?~a" base (uri-encode-path path) qs)))))

(defmethod create-multipart-upload ((store s3-backend) key &key content-type metadata)
  (let* ((query (list (cons "uploads" "")))
         (req (build-s3-request store :method :post :key key :query query
                                :content-type content-type
                                :extra-headers metadata))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    (let ((upload-id (%child-text (%parse-xml store key (%response-body resp)) "UploadId")))
      (unless (and upload-id (plusp (length upload-id)))
        (error 'object-store-error :key key :store store
               :message "S3: InitiateMultipartUploadResult has no UploadId"))
      upload-id)))

(defmethod upload-part ((store s3-backend) key upload-id part-number data &key)
  (let* ((bytes (coerce-object-octets data))
         (query (list (cons "partNumber" (princ-to-string part-number))
                      (cons "uploadId" upload-id)))
         (req (build-s3-request store :method :put :key key :query query :body bytes))
         (resp (%invoke-http store req)))
    (%raise-http store key (%response-status resp) (%response-body resp))
    (or (%etag-from-headers (%response-headers resp)) "")))

(defmethod complete-multipart-upload ((store s3-backend) key upload-id parts &key)
  (let* ((xml (xml-protocol:encode-to-octets
               (apply #'xml-protocol:make-xml-element "CompleteMultipartUpload" nil
                      (loop for p in parts
                            collect (xml-protocol:make-xml-element
                                     "Part" nil
                                     (xml-protocol:make-xml-element
                                      "PartNumber" nil (princ-to-string (car p)))
                                     (xml-protocol:make-xml-element
                                      "ETag" nil (princ-to-string
                                                  (if (consp (cdr p)) (cadr p) (cdr p)))))))
               :pretty nil))
         (query (list (cons "uploadId" upload-id)))
         (req (build-s3-request store :method :post :key key :query query
                                :body xml
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
