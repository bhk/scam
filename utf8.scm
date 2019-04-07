;; # utf8: UTF-8 Coding

(require "core.scm")
(require "math.scm")


;; Construct a multi-byte UTF-8 sequence.
;;
(define (utf-seq code offsets)
  (if offsets
      (let ((q (// code 64))
            (ro (rest offsets))
            (o1 (word 1 offsets)))
        (define `rem (- code (* q 64)))
        (append (utf-seq q ro)
                (+ o1 rem)))))


;; Convert Unicode character indices to UTF-8 encoded bytes.
;;
;; CODES = vector of Unicode code points\
;; Result = vector of code points
;;
;; Example:
;;
;;     > (utf8-encode [960 178])
;;     [207 128 194 178]
;;     > (string-from-bytecodes [207 128 194 178])
;;     "π²"
;;
(define (utf8-encode codes)
  &public
  (foreach c codes
           (cond
            ((< c 128) c)
            ((< c 2048) (utf-seq c [128 192]))
            ((< c 65536) (utf-seq c [128 128 224]))
            ((< c 1114112) (utf-seq c [128 128 128 240]))
            (else "?"))))


;; byte-to-bin: A quick decimal-to-binary for numbers 1...255

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

(define `(bin-to-dec str)
  (b2d-loop (subst 1 "1 " 0 "0 " str)))


;; Convert UTF-8 encoded bytes to Unicode code points.
;;
;; BYTES = vector of numeric byte values\
;; Result = vector of code points
;;
;; Example:
;;
;;     > (string-to-bytecodes "π²")
;;     [207 128 194 178]
;;     > (utf8-decode [207 128 194 178])
;;     [960 178]
;;
(define (utf8-decode bytes)
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
