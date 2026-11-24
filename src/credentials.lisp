(in-package #:object-store-backend-s3)

;;; AWS credential chain (botocore CredentialResolver shape): an ordered list
;;; of providers, the first that yields credentials wins. Explicit keys on
;;; the backend come before the list; the result is cached on the backend.
;;; Not here yet: container / instance metadata, SSO, assume-role (#6).

(defun %env (name)
  "Environment variable NAME, or NIL when unset or empty (botocore)."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun %home-file (relative)
  "RELATIVE under the user's home: $HOME when set, else USER-HOMEDIR-PATHNAME."
  (merge-pathnames relative
                   (let ((home (%env "HOME")))
                     (if home
                         (uiop:ensure-directory-pathname (uiop:parse-native-namestring home))
                         (user-homedir-pathname)))))

(defun %credentials-or-partial (source access-key secret-key session-token)
  "(values ACCESS-KEY SECRET-KEY SESSION-TOKEN) when both keys are present,
   NIL when neither is, and an OBJECT-STORE-ERROR for one without the other."
  (cond ((and access-key secret-key)
         (values access-key secret-key session-token))
        ((or access-key secret-key)
         (error 'object-store-error
                :message (format nil "S3: partial credentials in ~A (~:[aws_access_key_id~;aws_secret_access_key~] missing)"
                                 source access-key)))
        (t nil)))

(defun %profile-name (backend)
  (or (s3-backend-profile backend)
      (%env "AWS_PROFILE")
      (%env "AWS_DEFAULT_PROFILE")
      "default"))

(defun %ini-section (path section)
  "SECTION of the INI file at PATH (cl-stack-config:parse-ini) as an EQUAL
   hash-table, or NIL when the file or the section is missing."
  (when (probe-file path)
    (let ((table (gethash section
                          (cl-stack-config:parse-ini (uiop:read-file-string path)
                                                     path))))
      (and (hash-table-p table) table))))

(defun %section-credentials (source path section)
  (let ((table (%ini-section path section)))
    (when table
      (flet ((value (key)
               (let ((v (gethash key table)))
                 (and v (plusp (length (princ-to-string v))) (princ-to-string v)))))
        (%credentials-or-partial
         (format nil "~A [~A]" source section)
         (value "aws_access_key_id")
         (value "aws_secret_access_key")
         (or (value "aws_session_token") (value "aws_security_token")))))))

(defun environment-credentials (backend)
  "AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY (+ AWS_SESSION_TOKEN, or the
   legacy AWS_SECURITY_TOKEN). Passes when BACKEND names a :profile: an
   explicit profile drops the environment provider, as botocore's
   create_credential_resolver does for Session(profile_name=...) and
   `aws --profile`. A profile taken from AWS_PROFILE leaves it first."
  (unless (s3-backend-profile backend)
    (%credentials-or-partial "the environment"
                             (%env "AWS_ACCESS_KEY_ID")
                             (%env "AWS_SECRET_ACCESS_KEY")
                             (or (%env "AWS_SESSION_TOKEN") (%env "AWS_SECURITY_TOKEN")))))

(defun shared-credentials-file-credentials (backend)
  "The profile's section of AWS_SHARED_CREDENTIALS_FILE, else ~/.aws/credentials."
  (let ((path (let ((file (%env "AWS_SHARED_CREDENTIALS_FILE")))
                (if file
                    (uiop:parse-native-namestring file)
                    (%home-file ".aws/credentials")))))
    (%section-credentials (uiop:native-namestring path) path (%profile-name backend))))

(defun config-file-credentials (backend)
  "Keys in the profile's section of AWS_CONFIG_FILE, else ~/.aws/config:
   [default] or [profile NAME]."
  (let* ((path (let ((file (%env "AWS_CONFIG_FILE")))
                 (if file
                     (uiop:parse-native-namestring file)
                     (%home-file ".aws/config"))))
         (profile (%profile-name backend))
         (section (if (string= profile "default")
                      "default"
                      (format nil "profile ~A" profile))))
    (%section-credentials (uiop:native-namestring path) path section)))

(defvar *s3-credential-providers*
  '(environment-credentials
    shared-credentials-file-credentials
    config-file-credentials)
  "Credential providers tried in order when a backend has no explicit keys.
   Each is a function of the backend → (values access-key secret-key
   session-token), or NIL to pass. Push your own (instance metadata, a
   vault) to extend the chain.")

(defun s3-backend-credentials (backend)
  "(values access-key secret-key session-token) for BACKEND: its explicit
   keys, else the first of *S3-CREDENTIAL-PROVIDERS* that yields some (then
   kept on BACKEND). Signals OBJECT-STORE-ERROR when there are none, rather
   than signing as Credential=NIL."
  (let ((access-key (s3-backend-access-key backend))
        (secret-key (s3-backend-secret-key backend)))
    (if (or access-key secret-key)
        (%credentials-or-partial ":access-key / :secret-key" access-key secret-key
                                 (s3-backend-session-token backend))
        (dolist (provider *s3-credential-providers*
                          (error 'object-store-error
                                 :store backend
                                 :message (format nil "S3: no AWS credentials. Pass :access-key / :secret-key, ~:[set AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY, ~;~]or add [~A] to ~~/.aws/credentials"
                                                  (s3-backend-profile backend)
                                                  (%profile-name backend))))
          (multiple-value-bind (key secret token) (funcall provider backend)
            (when key
              (setf (s3-backend-access-key backend) key
                    (s3-backend-secret-key backend) secret
                    (s3-backend-session-token backend) token)
              (return (values key secret token))))))))
