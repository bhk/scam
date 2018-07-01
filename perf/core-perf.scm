(require "'core")
(require "num")
(require "clocker")


(define (main argv)
  (define e (current-env))
  (clk-show "dict-keys" (dict-keys e))
  (clk-show "dict-find" (dict-find "main" e))
  (clk-show "dict-get (a)" (dict-get "main" e))
  (clk-show "dict-get (b)" (dict-get "main" e "x"))
  nil)
