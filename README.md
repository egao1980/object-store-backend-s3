# object-store-backend-s3

S3 / MinIO backend for [`object-store-protocol`](https://github.com/egao1980/object-store-protocol). Builds signed HTTP requests (AWS SigV4). Does **not** require a live MinIO for unit tests — inject `http-fn`.

HMAC-SHA256 is **soft-use**: `crypto-protocol:hmac` when `*crypto-backend*` is bound, else Ironclad, else the documented internal SHA-256 + HMAC in `src/hmac.lisp`.

```lisp
(asdf:load-system "object-store-backend-s3")

(let ((s3 (object-store-backend-s3:make-s3-backend
           :endpoint "http://127.0.0.1:9000"
           :region "us-east-1"
           :access-key "AKIA…"
           :secret-key "…"
           :bucket "my-bucket"
           :http-fn (lambda (req) …))))
  (stack-object-store:put-object s3 "a.txt" "hi"))
```

Testable signing primitives: `canonical-request`, `string-to-sign`, `signature`, `authorization-header`. Known AWS GET-object fixture in the test suite.

`s3-backend` slots: `endpoint` `region` `access-key` `secret-key` `bucket` `http-fn`.

## License

MIT — see [LICENSE](LICENSE).
