# File name escaping
#
# Special characters:
#   ` `, `#` is escaped with `\`.
#   The first `\` preceding ` ` or `#` is escaped with `\`.
#   `|` is escaped with `\` on RHS (not on LHS!) with odd rules:
#        `|` ->     `\|`
#       `\|` ->   `\\\|`
#      `\\|` -> `\\\\\|`
#   `=` and `$` can be the result of an expansion.
#
# Unusable characters:
#   `*` and `?` are always expanded.
#   `~` will be expanded when at the start  (`./~` does not fix it.)
#   TAB converts to space when escaped with `\` on RHS (not on LHS).
#   `%` identifies a pattern.
#   `;` terminates LHS, cannot be escaped at all on RHS.
#   `:` can be escaped with `\`, but when on RHS the `\` remains.

E = =

all: @\#\ $$$E\\\|\^&()[]{}-_+'`"',.<>
	@: $(info < = $<)

@\#\ $$$E\|\^&()[]{}-_+'`"',.<>:
	@: $(info @ = $@)
