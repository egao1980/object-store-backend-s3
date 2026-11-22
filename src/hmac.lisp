(in-package #:object-store-backend-s3)

;;; SigV4 primitives over the stack: crypto-protocol (SHA-256, HMAC-SHA256 via
;;; crypto-backend-ironclad) and encoding-protocol (UTF-8, hex). Kept as the
;;; exported names; no private hash or codec code.

(defun utf8-octets (x)
  "X as a simple octet vector: octet vectors are copied as is, strings are
   encoded as UTF-8 (encoding-protocol)."
  (etypecase x
    (string (coerce (encoding-protocol:encode x :encoding :utf-8)
                    '(simple-array (unsigned-byte 8) (*))))
    (vector (coerce x '(simple-array (unsigned-byte 8) (*))))))

(defun octets-to-hex (octets)
  "Lower-case hex (SigV4 canonical form)."
  (string-downcase (encoding-protocol:encode (utf8-octets octets) :encoding :base16)))

(defun sha256 (data)
  "SHA-256 of DATA (string → UTF-8 octets) as octets."
  (crypto-protocol:digest (utf8-octets data) :algorithm :sha256))

(defun hmac-sha256 (key data)
  "HMAC-SHA256 of DATA under KEY (strings → UTF-8 octets) as octets."
  (crypto-protocol:hmac (utf8-octets key) (utf8-octets data) :algorithm :sha256))
