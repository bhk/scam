(require "core")
(require "utf8" &private)

;; byte-to-bin

(expect 255 (words byte-to-bin-a))
(expect "00000001" (byte-to-bin 1))
(expect "10110010" (byte-to-bin 178))
(expect "11111111" (byte-to-bin 255))

;; bin-to-dec

(expect 193 (bin-to-dec 11000001))

;; bytes-to-chars

(expect [32 127] (bytes-to-chars [32 127]))
;; the result of (string-to-bytes "a¢€￦€¢a")
(expect [97 162 8364 65510 8364 162 97]
        (bytes-to-chars [97 194 162 226 130 172 239 191 166 226 130 172 194 162 97]))
(expect [66376]
        (bytes-to-chars [240 144 141 136]))

;; chars-to-bytes

(expect [32 127] (chars-to-bytes [32 127]))
(expect [97 194 162 226 130 172 239 191 166 226 130 172 194 162 97]
        (chars-to-bytes [97 162 8364 65510 8364 162 97]))
(expect [240 144 141 136]
        (chars-to-bytes [66376]))
