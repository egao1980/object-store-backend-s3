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

(deftest explicit-profile-edges
  (testing "an explicit profile missing from the files signals; it never falls back to the environment keys"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
                   (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
                   (let ((got (%access-key-of (make-s3-backend :bucket "b" :profile "nosuch"))))
                     (ok (typep got 'object-store-protocol:object-store-error)
                         (format nil "got ~A" got))
                     (ok (and (typep got 'condition)
                              (search "[nosuch]" (princ-to-string got))
                              (not (search "AWS_ACCESS_KEY_ID" (princ-to-string got))))
                         "the message names the profile and does not suggest the environment keys"))))
  (testing "an explicit profile from the config file still wins over the environment"
    (%with-aws-env (home ("AWS_ACCESS_KEY_ID" "AKIDENV")
                         ("AWS_SECRET_ACCESS_KEY" "SECRETENV"))
                   (%write-file (merge-pathnames ".aws/config" home)
                                (format nil "[profile cfg]~%aws_access_key_id = AKIDCFG~%aws_secret_access_key = S~%"))
                   (ok (equal "AKIDCFG" (%access-key-of (make-s3-backend :bucket "b" :profile "cfg"))))))
  (testing "explicit :access-key / :secret-key still beat an explicit profile"
    (%with-aws-env (home)
                   (%write-file (merge-pathnames ".aws/credentials" home) +credentials-file+)
                   (ok (equal "AKIDARG" (%access-key-of (make-s3-backend :bucket "b" :profile "dev"
                                                                         :access-key "AKIDARG"
                                                                         :secret-key "S")))))))
