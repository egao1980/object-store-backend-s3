(defsystem "object-store-backend-s3"
  :version "0.2.0"
  :description "S3 / MinIO backend for object-store-protocol (SigV4; injectable HTTP)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("object-store-protocol"
               (:version "encoding-protocol" "0.3.0")
               "crypto-protocol"
               "crypto-backend-ironclad"
               "xml-protocol"
               "xml-backend-native"
               ;; the default http-fn (send-with-http-protocol)
               "http-protocol"
               ;; ~/.aws/credentials and ~/.aws/config (parse-ini)
               "cl-stack-config")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "hmac")
               (:file "sigv4")
               (:file "backend")
               (:file "credentials"))
  :in-order-to ((test-op (test-op "object-store-backend-s3/tests"))))

(defsystem "object-store-backend-s3/tests"
  :depends-on ("object-store-backend-s3" "http-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "signing-test")
               (:file "defaults-test")
               (:file "credentials-edge-test")
               (:file "profile-precedence-test")
               (:file "ini-compat-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
