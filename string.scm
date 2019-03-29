;; # string: String Manipulation
;;
;; The `string` library provides some common string manipulation functions
;; that are not available as builtins.

(require "core.scm")

;; This is a vector of all bytes 1...255.
(define all-bytes
  (concat
        "\x01 \x02 \x03 \x04 \x05 \x06 \x07 "
   "\x08   !+ \x0A \x0B \x0C \x0D \x0E \x0F "
   "\x10 \x11 \x12 \x13 \x14 \x15 \x16 \x17 "
   "\x18 \x19 \x1A \x1B \x1C \x1D \x1E \x1F "
   "  !0   !1 \x22 \x23 \x24 \x25 \x26 \x27 "
   "\x28 \x29 \x2A \x2B \x2C \x2D \x2E \x2F "
   "\x30 \x31 \x32 \x33 \x34 \x35 \x36 \x37 "
   "\x38 \x39 \x3A \x3B \x3C \x3D \x3E \x3F "
   "\x40 \x41 \x42 \x43 \x44 \x45 \x46 \x47 "
   "\x48 \x49 \x4A \x4B \x4C \x4D \x4E \x4F "
   "\x50 \x51 \x52 \x53 \x54 \x55 \x56 \x57 "
   "\x58 \x59 \x5A \x5B \x5C \x5D \x5E \x5F "
   "\x60 \x61 \x62 \x63 \x64 \x65 \x66 \x67 "
   "\x68 \x69 \x6A \x6B \x6C \x6D \x6E \x6F "
   "\x70 \x71 \x72 \x73 \x74 \x75 \x76 \x77 "
   "\x78 \x79 \x7A \x7B \x7C \x7D \x7E \x7F "
   "\x80 \x81 \x82 \x83 \x84 \x85 \x86 \x87 "
   "\x88 \x89 \x8A \x8B \x8C \x8D \x8E \x8F "
   "\x90 \x91 \x92 \x93 \x94 \x95 \x96 \x97 "
   "\x98 \x99 \x9A \x9B \x9C \x9D \x9E \x9F "
   "\xA0 \xA1 \xA2 \xA3 \xA4 \xA5 \xA6 \xA7 "
   "\xA8 \xA9 \xAA \xAB \xAC \xAD \xAE \xAF "
   "\xB0 \xB1 \xB2 \xB3 \xB4 \xB5 \xB6 \xB7 "
   "\xB8 \xB9 \xBA \xBB \xBC \xBD \xBE \xBF "
   "\xC0 \xC1 \xC2 \xC3 \xC4 \xC5 \xC6 \xC7 "
   "\xC8 \xC9 \xCA \xCB \xCC \xCD \xCE \xCF "
   "\xD0 \xD1 \xD2 \xD3 \xD4 \xD5 \xD6 \xD7 "
   "\xD8 \xD9 \xDA \xDB \xDC \xDD \xDE \xDF "
   "\xE0 \xE1 \xE2 \xE3 \xE4 \xE5 \xE6 \xE7 "
   "\xE8 \xE9 \xEA \xEB \xEC \xED \xEE \xEF "
   "\xF0 \xF1 \xF2 \xF3 \xF4 \xF5 \xF6 \xF7 "
   "\xF8 \xF9 \xFA \xFB \xFC \xFD \xFE \xFF"))


;; Construct a function that performs a number of substitutions.  The
;; returned function accepts one argument and returns the result of the
;; substitutions.
;;
;; FROMS = a vector of substrings to be replaced.\
;; TOS = a vector of corresponding replacements.\
;; INPUT = if non-nil, a lambda expression that will transform the
;;   input string before the substitutions.
;;
(define (gen-polysub froms tos ?input)
  &public
  (define `arg
    (or input (lambda (s) s)))

  (define `(enc s)
    (subst "$" "$$" "(" "$[" ")" "$]" "," "$&" s))

  (define `(gen-subst from to expr)
    (concat "$(subst " (enc from) "," (enc to) "," expr ")"))

  (if froms
      (gen-polysub (rest froms) (rest tos)
                   (gen-subst (first froms) (first tos) arg))
      arg))


;; Demote all elements in a vector.  This is faster than `(for i vec [i])`.
(define (vdemote vec)
  (subst [" "] " " [vec]))


(define (gen-split chars to-pattern)
  (let& ((froms (vdemote chars)))
    (gen-polysub froms (patsubst "%" to-pattern froms))))

(define `ascii (wordlist 1 127 all-bytes))
(define `utf8-cont (wordlist 128 191 all-bytes))   ;; continuation bytes
(define `utf8-esc (wordlist 194 244 all-bytes))    ;; initial bytes of a sequence

;; Note: These 'split' functions operate on *demoted* strings.
(declare (split-ascii str))
(declare (split-utf8-esc str))
(declare (split-utf8-cont str))

(set split-ascii (gen-split ascii ["% "]))
(set split-utf8-esc (gen-split utf8-esc ["% "]))
(set split-utf8-cont (gen-split utf8-cont ["!.% "]))


;; Group UTF-8 continuation characters with UTF-8 initial bytes *if* the
;; string contains any initial bytes.  Just in case we have non-UTF8,
;; don't leave any "!." strings behind.
;;
(define (utf8-group-if s)
  (if (word 2 s)
      (subst " !." "" "!." " " (split-utf8-cont s))
      s))


;; Get all characters in STR.  The result is a vector of strings, each
;; containing one character. `concat-vec` reverses this operation.
;;
;; UTF-8 encoding of STR is assumed.  If STR is not a well-formed UTF8
;; string, the result will contain all *bytes* in STR (so `concat-vec` will
;; still reverse the operation) but the bytes may not grouped at character
;; boundaries.
;;
(define (string-to-chars s)
  &public
  (define `u8
    (utf8-group-if (split-utf8-esc (subst "!" "!1" " " "!0" "\t" "!+" s))))
  (filter "%" (split-ascii u8)))


;; Return the number of *characters* in S.  UTF-8 encoding is assumed.
;;
(define (string-len s)
  &public
  (words (string-to-chars s)))


;; Extract a substring, given start and end character indices.  The range is
;; inclusive and 1-based.  When LAST is less than FIRST, an empty string is
;; returned.
;;
;; FIRST = index of first character to include (1 or greater).\
;; LAST = index of last character to include (0 or greater).
;;
(define (string-slice first last str)
  &public
  (promote (subst " " "" (wordlist first last (string-to-chars str)))))


;; Convert letters to upper case.  Only ASCII letters are supported.
;;
(declare (string-upper str) &public)


;; Convert letters to lower case.  Only ASCII letters are supported.
;;
(declare (string-lower str) &public)


(begin
  (define `uppers (wordlist 65 90 all-bytes))
  (define `lowers (wordlist 97 122 all-bytes))
  (set string-upper (gen-polysub lowers uppers))
  (set string-lower (gen-polysub uppers lowers)))


;; Get the numeric indices of all *bytes* in STR.  The result is a vector of
;; numbers from 1 to 255.
;;
(define (string-to-bytes str)
  &public

  ;; initialize on first use
  (declare (s2b-sub str))
  (declare (num-enc str))
  (if (not s2b-sub)
      (begin
        (set num-enc
             ;; Prefix all digits in a vector.  Fix up bang-escaped values.
             (gen-polysub ["/" 0 1 2 3 4 5 6 7 8 9 "!/1" "!/0"]
                          ["/@" "/0" "/1" "/2" "/3" "/4" "/5" "/6" "/7" "/8" "/9" "!1" "!0"]))

        (set s2b-sub (gen-polysub (vdemote (num-enc all-bytes))
                                  (addsuffix [" "] (indices all-bytes))))))

  (filter "%" (s2b-sub (num-enc [str]))))


;; Convert byte values into single-byte strings.
;;
;; BYTES = a vector of byte values (numbers from 0 to 255).
;;
;; The result is a vector the same length as bytes.  Each zero value in
;; BYTES will result in a corresponding empty string in the result.
;;
(define (strings-from-bytes bytes)
  &public
  (foreach n bytes
           (if (filter 0 n)
               [""]
               (word n all-bytes))))


;; Construct a string from a vector of byte values.  This reverses
;; `string-to-bytes`.
;;
(define (string-from-bytes bytes)
  &public
  (concat-vec (strings-from-bytes bytes)))


(define (nwords num str)
  (if (word num str)
      (wordlist 1 num str)
      (nwords num (concat str " " str " " str))))


;; Return a string that consists of NUM copies of STR concatenated.
;;
(define (string-repeat str num)
  &public
  (subst " " "" "." str (if (filter-out "-%" (subst 0 "" num)) (nwords num ". . ."))))
