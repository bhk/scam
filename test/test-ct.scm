(require "core")
(require "parse")

(defmacro (ut1 form)
  (if (eq 3 (words form))
      ["Q" (string-value (nth 3 form))]
      ["Q" "error"]))
