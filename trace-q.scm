(require "core")
(require "trace" &private)

(expect "........0" (trace-digits ""))
(expect "........1" (trace-digits "o"))
(expect "......121" (trace-digits "oiooio"))
(expect ".......10" (trace-digits "oooooooooo"))
