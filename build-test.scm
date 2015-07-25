(require "core")
(require "build")

(print (exe-rules "out/scam" "scam.scm" 1))
("$(call link,.v1/S,.v1/scam.min .v1/core.min .v1/repl.min .v1/build.min .v1/trace.min .v1/runtime.min .v1/io.min .v1/parse.min .v1/compile.min .v1/num.min .v1/gen.min .v1/gen0.min .v1/gen1.min .v1/macros.min .v1/escape.min,scam,1)")
