(defpackage #:object-store-backend-s3
  (:use #:cl #:object-store-protocol)
  (:export #:s3-backend
           #:make-s3-backend
           #:use-s3-backend
           #:s3-backend-endpoint
           #:s3-backend-region
           #:s3-backend-access-key
           #:s3-backend-secret-key
           #:s3-backend-bucket
           #:s3-backend-http-fn

           #:octets-to-hex
           #:utf8-octets
           #:sha256
           #:hmac-sha256
           #:uri-encode

           #:canonical-request
           #:string-to-sign
           #:signing-key
           #:signature
           #:authorization-header
           #:sign-s3-request
           #:build-s3-request
           #:s3-http-request
           #:s3-http-request-p
           #:s3-http-request-method
           #:s3-http-request-url
           #:s3-http-request-headers
           #:s3-http-request-body
           #:s3-http-request-query))

(in-package #:object-store-backend-s3)
