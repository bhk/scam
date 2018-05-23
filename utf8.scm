;;----------------------------------------------------------------
;; utf8: Convert between UTF-8-encodings and character values.
;;----------------------------------------------------------------

(require "core")
(require "num")


;; Emit a sequence of continuation bytes and a prefix byte.
;;
(define (emit-utf value offsets)
  (if offsets
      (let ((dr (div-rem value 64))
            (ro (rest offsets))
            (o1 (word 1 offsets)))
        (append (emit-utf (word 1 dr) ro)
                (+ o1 (word 2 dr))))))


;; Convert Unicode character indices to UTF-8 encoded bytes.
;; CHARS = vector of Unicode character indices.
;;
(define (chars-to-bytes chars)
  &public
  (foreach ch chars
           (cond
            ((< ch 128) ch)
            ((< ch 2048) (emit-utf ch [128 192]))
            ((< ch 65536) (emit-utf ch [128 128 224]))
            ((< ch 1114112) (emit-utf ch [128 128 128 240]))
            (else "?"))))


;; byte-to-bin: A quick decimal-to-binary for values 1...255

(define (plex2 strings)
  (foreach a strings
           (foreach b strings
                    (concat a b))))

(define byte-to-bin-a (rest (plex2 (plex2 "00 01 10 11"))))

(define (byte-to-bin n)
  (if (subst 0 "" n)
      (word n byte-to-bin-a)))


;; bin-to-dec: Convert an arbitrarily large binary integer to decimal.

(define (b2d-loop bits)
  (if (word 2 bits)
      (+ (lastword bits) (* 2 (b2d-loop (butlast bits))))
      bits))

(define (bin-to-dec str)
  (b2d-loop (subst 1 "1 " 0 "0 " str)))


;; Convert UTF-8 encoded bytes to Unicode character indices.
;; BYTES = vector of numeric byte values
;;
(define (bytes-to-chars bytes)
  &public

  ;; chop-1s : Remove consecutive leading 1 bits
  (define `(chop-1s n)
    (or (subst " " "" (rest (subst 0 "0 " n)))
        0))

  ;; Get ASCII values in decimal; high bytes in binary.
  (define `nums
    (foreach dec bytes
             (or (filter "1%" (concat (byte-to-bin dec) "b"))
                 dec)))

  ;; Combine continuing bytes with preceding high bytes.
  (define `grouped (subst "b 10" "" nums))

  ;; convert remaining binary to decimal
  (foreach num grouped
           (if (filter "%b" num)
               (bin-to-dec (chop-1s (subst "b" "" num)))
               num)))
