;;;; Prosecution evidence for #4: an explicit :profile must win over
;;;; credentials in the environment, as boto3.Session(profile_name=...) and
;;;; `aws --profile` do. botocore's create_credential_resolver removes the
;;;; EnvProvider when a profile is passed explicitly; AWS_PROFILE from the
;;;; environment does not remove it.

(in-package #:object-store-backend-s3/tests)

(deftest explicit-profile-beats-environment-keys
  (testing "(make-s3-backend :profile \"dev\") with AWS_ACCESS_KEY_ID set signs with [dev]"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
      (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
      (let ((got (%access-key-of (make-s3-backend :bucket "b" :profile "dev"))))
        (ok (equal "AKIDDEV" got)
            (format nil "signed with ~A, want AKIDDEV (the profile passed)" got)))))
  (testing "AWS_PROFILE from the environment does not displace the environment keys (botocore)"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV")
                         ("AWS_PROFILE" "dev"))
      (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
      (ok (equal "AKIDENV" (%access-key-of (make-s3-backend :bucket "b")))))))
