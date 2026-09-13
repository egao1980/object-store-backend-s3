(in-package #:object-store-backend-s3)

;;; HMAC-SHA256: prefer crypto-protocol (when a backend is bound), else Ironclad,
;;; else this file's SHA-256 + HMAC (documented internal fallback).

(defun utf8-octets (x)
  (etypecase x
    ((vector (unsigned-byte 8))
     (coerce x '(simple-array (unsigned-byte 8) (*))))
    ((and vector (not string))
     (if (zerop (length x))
         (make-array 0 :element-type '(unsigned-byte 8))
         (coerce x '(simple-array (unsigned-byte 8) (*)))))
    (string
     (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
       (loop for c across x
             for code = (char-code c)
             do (cond
                  ((< code #x80)
                   (vector-push-extend code out))
                  ((< code #x800)
                   (vector-push-extend (logior #xc0 (ash code -6)) out)
                   (vector-push-extend (logior #x80 (logand code #x3f)) out))
                  ((< code #x10000)
                   (vector-push-extend (logior #xe0 (ash code -12)) out)
                   (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                   (vector-push-extend (logior #x80 (logand code #x3f)) out))
                  (t
                   (vector-push-extend (logior #xf0 (ash code -18)) out)
                   (vector-push-extend (logior #x80 (logand (ash code -12) #x3f)) out)
                   (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                   (vector-push-extend (logior #x80 (logand code #x3f)) out))))
       (coerce out '(simple-array (unsigned-byte 8) (*)))))))

(defun octets-to-hex (octets)
  "Lowercase hex (AWS SigV4 requires lowercase)."
  (string-downcase
   (with-output-to-string (s)
     (loop for b across octets
           do (format s "~2,'0x" b)))))

(defun %u32 (n)
  (logand n #xffffffff))

(defun %rotr32 (x n)
  (let ((x (%u32 x)))
    (%u32 (logior (ash x (- n)) (ash x (- 32 n))))))

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
    #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
    #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
    #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
    #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
    #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
    #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %sha256-pad (octets)
  (let* ((len (length octets))
         (bitlen (* len 8))
         (mod (mod (+ len 1) 64))
         (zeros (if (<= mod 56) (- 56 mod) (+ 56 (- 64 mod))))
         (out (make-array (+ len 1 zeros 8) :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (replace out octets)
    (setf (aref out len) #x80)
    (loop for i from 0 below 8
          do (setf (aref out (- (length out) 1 i))
                   (ldb (byte 8 (* i 8)) bitlen)))
    out))

(defun %sha256-compress (h block)
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 0 below 16
          for off = (* i 4)
          do (setf (aref w i)
                   (logior (ash (aref block off) 24)
                           (ash (aref block (+ off 1)) 16)
                           (ash (aref block (+ off 2)) 8)
                           (aref block (+ off 3)))))
    (loop for i from 16 below 64
          for s0 = (logxor (%rotr32 (aref w (- i 15)) 7)
                           (%rotr32 (aref w (- i 15)) 18)
                           (ash (aref w (- i 15)) -3))
          for s1 = (logxor (%rotr32 (aref w (- i 2)) 17)
                           (%rotr32 (aref w (- i 2)) 19)
                           (ash (aref w (- i 2)) -10))
          do (setf (aref w i)
                   (%u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
    (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
      (loop for i from 0 below 64
            for s1 = (logxor (%rotr32 e 6) (%rotr32 e 11) (%rotr32 e 25))
            for ch = (logxor (logand e f) (logand (lognot e) g))
            for temp1 = (%u32 (+ hh s1 ch (aref +sha256-k+ i) (aref w i)))
            for s0 = (logxor (%rotr32 a 2) (%rotr32 a 13) (%rotr32 a 22))
            for maj = (logxor (logand a b) (logand a c) (logand b c))
            for temp2 = (%u32 (+ s0 maj))
            do (setf hh g g f f e e (%u32 (+ d temp1))
                     d c c b b a a (%u32 (+ temp1 temp2))))
      (setf (aref h 0) (%u32 (+ (aref h 0) a))
            (aref h 1) (%u32 (+ (aref h 1) b))
            (aref h 2) (%u32 (+ (aref h 2) c))
            (aref h 3) (%u32 (+ (aref h 3) d))
            (aref h 4) (%u32 (+ (aref h 4) e))
            (aref h 5) (%u32 (+ (aref h 5) f))
            (aref h 6) (%u32 (+ (aref h 6) g))
            (aref h 7) (%u32 (+ (aref h 7) hh))))
    h))

(defun %sha256-internal (data)
  (let* ((octets (utf8-octets data))
         (padded (%sha256-pad octets))
         (h (make-array 8 :element-type '(unsigned-byte 32)
                        :initial-contents
                        '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                          #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))))
    (loop for off from 0 below (length padded) by 64
          do (%sha256-compress h (subseq padded off (+ off 64))))
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 8
            for word = (aref h i)
            for base = (* i 4)
            do (setf (aref out base) (ldb (byte 8 24) word)
                     (aref out (+ base 1)) (ldb (byte 8 16) word)
                     (aref out (+ base 2)) (ldb (byte 8 8) word)
                     (aref out (+ base 3)) (ldb (byte 8 0) word)))
      out)))

(defun %hmac-sha256-internal (key data)
  (let* ((key (utf8-octets key))
         (data (utf8-octets data))
         (block-key (if (> (length key) 64)
                        (%sha256-internal key)
                        key))
         (ipad (make-array 64 :element-type '(unsigned-byte 8) :initial-element #x36))
         (opad (make-array 64 :element-type '(unsigned-byte 8) :initial-element #x5c)))
    (loop for i from 0 below (length block-key)
          do (setf (aref ipad i) (logxor (aref ipad i) (aref block-key i))
                   (aref opad i) (logxor (aref opad i) (aref block-key i))))
    (%sha256-internal
     (concatenate '(simple-array (unsigned-byte 8) (*))
                  opad
                  (%sha256-internal
                   (concatenate '(simple-array (unsigned-byte 8) (*)) ipad data))))))

(defun sha256 (data)
  "SHA-256 of DATA (string or octets). Soft-uses crypto-protocol / ironclad."
  (let* ((octets (utf8-octets data))
         (cp (find-package :crypto-protocol))
         (digest (and cp (find-symbol "DIGEST" cp)))
         (backend (and cp (find-symbol "*CRYPTO-BACKEND*" cp)))
         (ip (find-package :ironclad))
         (idigest (and ip (find-symbol "DIGEST-SEQUENCE" ip))))
    (cond
      ((and digest backend (fboundp digest) (symbol-value backend))
       (funcall digest octets :algorithm :sha256))
      ((and idigest (fboundp idigest))
       (funcall idigest :sha256 octets))
      (t (%sha256-internal octets)))))

(defun hmac-sha256 (key data)
  "HMAC-SHA256. Soft-uses crypto-protocol:HMAC when *crypto-backend* is bound,
   else ironclad:HMAC-DIGEST, else the internal SHA-256 in this file."
  (let* ((key (utf8-octets key))
         (data (utf8-octets data))
         (cp (find-package :crypto-protocol))
         (hmac (and cp (find-symbol "HMAC" cp)))
         (backend (and cp (find-symbol "*CRYPTO-BACKEND*" cp)))
         (ip (find-package :ironclad))
         (make (and ip (find-symbol "MAKE-HMAC" ip)))
         (update (and ip (find-symbol "UPDATE-HMAC" ip)))
         (final (and ip (find-symbol "HMAC-DIGEST" ip))))
    (cond
      ((and hmac backend (fboundp hmac) (symbol-value backend))
       (funcall hmac key data :algorithm :sha256))
      ((and make update final (fboundp make) (fboundp update) (fboundp final))
       (let ((ctx (funcall make key :sha256)))
         (funcall update ctx data)
         (funcall final ctx)))
      (t (%hmac-sha256-internal key data)))))
