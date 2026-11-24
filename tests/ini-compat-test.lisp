;;;; Prosecution evidence (round 3) for #4: the shared-file providers read
;;;; ~/.aws/credentials with cl-stack-config:parse-ini, which cuts every line
;;;; at the first # or ; (an inline comment anywhere on the line) and matches
;;;; key names case-sensitively. botocore reads the same file with
;;;; configparser.RawConfigParser: comments only on their own line, option
;;;; names lower-cased. Against these two files botocore gives
;;;; ('AKIDHASH', 'minio#Secret;2026') and ('AKIDUPPER', 'SECRETUPPER').
;;;; A secret holding # or ; (MinIO, Ceph and R2 secrets are user-chosen) is
;;;; signed cut short, and S3 answers SignatureDoesNotMatch.

(in-package #:object-store-backend-s3/tests)

(defparameter +hash-secret-file+
  "[default]
aws_access_key_id = AKIDHASH
aws_secret_access_key = minio#Secret;2026
")

(defparameter +upper-case-keys-file+
  "[default]
AWS_ACCESS_KEY_ID = AKIDUPPER
AWS_SECRET_ACCESS_KEY = SECRETUPPER
")

(deftest shared-file-secret-keeps-hash-and-semicolon
  (testing "aws_secret_access_key = minio#Secret;2026 signs with the whole secret, as botocore reads it"
    (%with-aws-env (home)
      (%write-file (merge-pathnames ".aws/credentials" home) +hash-secret-file+)
      (let ((got (%auth-of (make-s3-backend :bucket "b"))))
        (ok (equal (%explicit-auth "AKIDHASH" "minio#Secret;2026") got)
            (format nil "Authorization = ~A" got))
        (ok (not (equal (%explicit-auth "AKIDHASH" "minio") got))
            "not signed with the secret cut at #")))))

(deftest shared-file-key-names-ignore-case
  (testing "AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY in ~/.aws/credentials are read, as botocore reads them"
    (%with-aws-env (home)
      (%write-file (merge-pathnames ".aws/credentials" home) +upper-case-keys-file+)
      (let ((got (%auth-of (make-s3-backend :bucket "b"))))
        (ok (equal (%explicit-auth "AKIDUPPER" "SECRETUPPER") got)
            (format nil "Authorization = ~A" got))))))
