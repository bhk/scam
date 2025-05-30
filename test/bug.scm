;; "Private: ..." comment line ends in "\", eating up $(call ^R,core)...  add a space!

(require "core")
(define `D "\\")
(expect 1 2)
