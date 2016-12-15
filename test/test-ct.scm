(require "core")
(require "parse")


(defmacro (ut1 args)
  (or (case (first args)
        ((PSymbol pos name) (PString pos name)))
      (PString 0 "error")))
