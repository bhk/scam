(require "core.scm")
(require "getopts.scm" &private)

(expect (getopts ["a" "b c"]
                 nil)
        { *: ["a" "b c"] })

(expect (getopts ["-x" "-g"]
                 "-g=")
        { !: [(BadOption "-x")
              (MissingArg "-g")] })

(expect (getopts ["a" "-f" "-g" "g 1!" "-f" "--" "-f" "x y"]
                 "-f -g=")
        { *: ["a" "-f" "x y"],
          f: [1 1],
          g: ["g 1!"] })
