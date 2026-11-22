# object-store-backend-s3

S3 / MinIO backend for [`object-store-protocol`](https://github.com/egao1980/object-store-protocol). Builds signed HTTP requests (AWS SigV4). Does **not** require a live MinIO for unit tests — inject `http-fn`.

SHA-256 and HMAC-SHA256 come from `crypto-protocol` (`crypto-backend-ironclad`), UTF-8, hex and RFC 3986 percent-encoding (non-ASCII keys sign and address correctly) from `encoding-protocol`, and S3 XML (ListObjectsV2, multipart) is parsed and written with `xml-protocol`.

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
