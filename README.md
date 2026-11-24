# object-store-backend-s3

S3 / MinIO backend for [`object-store-protocol`](https://github.com/egao1980/object-store-protocol). It builds signed HTTP requests (AWS SigV4) and sends them through [`http-protocol`](https://github.com/egao1980/http-protocol) by default, or through an `http-fn` you inject. The unit tests need no live MinIO. **OCI: 0.2.0.**

SHA-256 and HMAC-SHA256 come from `crypto-protocol` (`crypto-backend-ironclad`). UTF-8, hex and RFC 3986 percent-encoding come from `encoding-protocol`, so non-ASCII keys sign and address correctly. S3 XML (ListObjectsV2, multipart) is parsed and written with `xml-protocol`. `~/.aws/credentials` and `~/.aws/config` are read with `cl-stack-config`'s `parse-ini` (0.3.1 or later), which reads them as botocore does (`configparser.RawConfigParser`): comments are whole lines only, so a secret may hold `#` and `;`, and option names are lowercased.

```lisp
(asdf:load-system "object-store-backend-s3")
(asdf:load-system "cl-stack-http")          ; or bind http-protocol:*http-backend* yourself

;; boto3.client("s3") shape: credentials from the environment / ~/.aws
(let ((s3 (object-store-backend-s3:make-s3-backend
           :endpoint "http://127.0.0.1:9000"
           :bucket "my-bucket")))
  (stack-object-store:put-object s3 "a.txt" "hi")
  (stack-object-store:get-object s3 "a.txt")
  (stack-object-store:presign-url s3 "a.txt" :expires 600))
```

## Sending requests

| `:http-fn` | What happens |
|------------|--------------|
| NIL (default) | `send-with-http-protocol` calls `http:request` with the bound `http-protocol:*http-backend*` / `*http-client*`. When no backend is bound and `cl-stack-http` is loaded, it uses `(cl-stack-http:ensure-http-backend)`, its `:auto` order. Without either it signals `object-store-error`. Bodies are kept as S3 stored them (`:decompress nil`), and 4xx/5xx come back as responses that map to `object-not-found`, `object-precondition-failed` or `object-store-error`. |
| a function | Called with the `s3-http-request` (`s3-http-request-method` / `-url` / `-headers` / `-body`), always, whatever else the image loaded. It returns an `http-protocol:http-response`, a list `(status body headers)`, or a plist `(:status … :body … :headers …)`. |

## Credentials (botocore chain)

Explicit `:access-key` / `:secret-key` (+ `:session-token`) win. Otherwise the first request walks `*s3-credential-providers*` and keeps the result on the backend:

1. `environment-credentials`: `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` (+ `AWS_SESSION_TOKEN`, or the legacy `AWS_SECURITY_TOKEN`). Skipped when the backend has an explicit `:profile`, as `boto3.Session(profile_name=...)` and `aws --profile` skip it. A profile that comes from `AWS_PROFILE` keeps the environment keys first.
2. `shared-credentials-file-credentials`: `AWS_SHARED_CREDENTIALS_FILE`, else `~/.aws/credentials`, in section `[PROFILE]`.
3. `config-file-credentials`: `AWS_CONFIG_FILE`, else `~/.aws/config`, in `[default]` / `[profile PROFILE]`.

PROFILE is `:profile`, else `AWS_PROFILE`, else `AWS_DEFAULT_PROFILE`, else `default`. An empty variable counts as unset. A key id without a secret, or the reverse, is an `object-store-error`, and so is finding no credentials at all. Nothing is ever signed as `Credential=NIL`. A session token is sent as the signed `x-amz-security-token` header, or as `X-Amz-Security-Token` in a presigned URL. A provider is a function of the backend that returns `(values access-key secret-key session-token)` or NIL; push your own to extend the chain. Container / instance-metadata credentials, SSO and assume-role are tracked in [#6](https://github.com/egao1980/object-store-backend-s3/issues/6).

The region comes from `:region`, else `AWS_REGION`, else `AWS_DEFAULT_REGION`, else `us-east-1`.

## Presigned URLs

`presign-url` signs the query string with `UNSIGNED-PAYLOAD`, as SigV4 requires, and matches the AWS documentation example in the test suite.

## API

Testable signing primitives: `canonical-request`, `string-to-sign`, `signature`, `authorization-header`, `sign-s3-request`, `build-s3-request`. The suite has the AWS GET / PUT / ListObjects / presigned-URL vectors.

`s3-backend` slots: `endpoint` `region` `access-key` `secret-key` `session-token` `profile` `bucket` `http-fn`.

## Test

```bash
cl-repo test
```

## License

MIT — see [LICENSE](LICENSE).
