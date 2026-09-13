(defsystem "object-store-backend-s3"
  :version "0.1.1"
  :description "S3 / MinIO backend for object-store-protocol (SigV4; injectable HTTP)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("object-store-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "hmac")
               (:file "sigv4")
               (:file "backend"))
  :in-order-to ((test-op (test-op "object-store-backend-s3/tests"))))

(defsystem "object-store-backend-s3/tests"
  :depends-on ("object-store-backend-s3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "signing-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
