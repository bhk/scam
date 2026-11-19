# SCAM Math Implementation Details

For almost all operations, numbers are converted to an internal unary format
for manipulation, and then back to ASCII decimal.  The unary format is still
in base 10, but each digit begins with a "0" character followed by 0 to 9
"1" characters.

There are a few variations on the unary formats:

* "U" encoding: straightforward replacement of ASCII with unary digits.

      (d2u ascii) -> unary
      (u2d unary) -> decimal

  Notably, for non-empty strings, `d2u` preserves all non-digit values
  unmolested, and `u2d` strictly reverses `d2u`.  As a result, validation
  and parsing of numbers is easier in the unary form, since only `0` and `1`
  digits appear in plain integers.  Math operations therefore convert to
  unary even before parsing and validation.

  Artihmetic operations on U values assume they contain no non-digit
  characters except a leading `-` in the case of negative values.  The
  rightmost digit is in the 1's position.

  For example `u-mul` and `u-add` accept two U-encoded integers and return a
  U-encoded integer.

  Invalid U values are represented by `NaN`, which survives the `u2d`
  conversion back to decimal unchanged.

* "UF" and "UV" encoding: These are both vector forms, with a space
  character between each unary digit, and unsigned.  They differ in how the
  numeric value is interpreted.

  "UF" means the vector represents a fraction x, 0 <= x < 1.  The digits
  fall just to the right of the decimal point, in big-endian format: the
  leftmost (first) digit is the 0.1 position.

  "UV" implies a right-aligned, integer representation.  The `uv-` prefix is
  also used for functions that are agnostic to the right- or left-alignment.

  Some examples:

  * `uf-add` differs from `u+` in that the UF digits are left-aligned,
    whereas the digits are right-aligned in U encoding.  Overflow (into the
    1's position) is dropped.

  * `uv-complement` returns the 10s complement of a UV integer (modulo the
    digit position to the left of the first digit), or of a UF fraction
    modulo 1.

  * `uf-mul` accepts to UF values and returns a value also between 0 and 1.
    Mechanically it is much like `u-mul`, with the difference being that
    leading zeros are significant in `uf-mul`.

* "FP" encoding is a list: [EXP SIGN ...FRAC]

  This represents the value SIGN * FRAC * 10^EXP.

  FRAC is UF-encoded, and typically normalized to 0.1 <= FRAC < 1, except
  when FRAC = 0.

  SIGN is either "+" or "-", representing 1 or -1.

  EXP is U-encoded.

  Invalid FP values are represented by nil, which will be converted to `NaN`
  by `fp2u` or `fp2d`.
