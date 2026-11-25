# SCAM Math Implementation Details

## Encodings

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

  FRAC is UF-encoded, and normalized to 0.1 <= FRAC < 1, except when FRAC =
  0.

  SIGN is either "+" or "-", representing 1 or -1.

  EXP is U-encoded.

  Invalid FP values are represented by nil, which will be converted to `NaN`
  by `fp2u` or `fp2d`.



## Precision and Accuracy

Functions accepting precision arguments have to meet accuracy guarantees,
which must consider the following sources of error:

1. Rounding error: Representing an arbitrary value with a finite number of
   digits will produce a result off by ≤ 0.5U (where U = units of the
   final digit).

2. Approximation error: Transcendental algorithms (log, exp, etc.) terminate
   when the computation error drops below some threshold.  Others limit this
   error by choosing the size of the values they manipulate (*~).  Rarely,
   there is no approzimation error (`round`).

These errors might point in the same direction, so in general algorithms can
approach but cannot reach a guarantee of ≤ 0.5U.  Instead we generally
guarantee < 1U.

When algorithms allow for it, such as `round`, we *guarantee* ≤ 0.5U, and
document that guarantee.

When our implementation of an algorithm yields errors worse than ≤ 0.5U but
quantifiably better than < 1U, we do not specify stricter guarantees in user
docs because it is implementation specific and because it would complicate
the specifications.  However, we can make use of it internally when
composing that function with other operations.


## Composing Operations

When composing functions, each of which provide accuracy guarantees, we have
to consider how those error might compound.

1. Addition: Here we can sum the absolute error of the arguments (`+` itself
   is exact):

       (+ (round x -3) (round y -3)) is off by ≤ 1 * 10^-3  [not "<"!]

   Relative error will be dominated by the larger of x or y.

       (+ (round x 3) (round y 3)) is off by ≤ 1 in 3rd digit.

2. Multiplication: Summing the relative errors yields a close approximation.

       (* (round x 3) (round y 3)) is off by "about 1U".

   Absolute error is complicated:

       (* (round x -3) (round y -3)) error is not limited by a constant.

   We can compute significant digits for x and y separately, and then
   translate that back to decimal position using the result.

3. Chaining:  h(x) = f(g(x))

   Here, `f` will introduce its own error, while magnifying or shrinking the
   error in the intermediate value.

   If `f` itself only guarantees <1U, we must invoke it with a higher
   precision than what was specified for `h`:

      (f' x p) -> (round (f x (prec+1 p)) p)

   The error of `f'` is < 0.6U, leaving a magin of 0.4U.  Using even higher
   precision for `f` increases the margin only slightly, since rounding
   introduced 0.5U.

   If `f` itself has a quantifiable margin, we might use the same precision
   specified for `h`, and avoid an extra call to `round`.


## Intermediate Values

When an approximate intermediate value is in play, we need to know how much
precision to specify when computing it.  We need the contributed error to
remain safely below the margin of `f` or `f'` -- usually less than 0.4U.
For improved flexibility in implementations, and for simplicity, we aim for
0.1U.  How that translates to the degree of accuracy required for `g(x)` can
be approximated using the derivative of `f`:

For simplicity, we compute what is required to achieve 0.1U:

    Δᵢ = max approximation error in input to `f`.
         Specifiying Pᵢ for `g(x)`, we can *expect* < 1U at Pᵢ:
           Δᵢ = 10^Nᵢ                  when Pᵢ names decimal position Nᵢ
           Δᵢ = |f(x)| * 10^(1-Dᵢ)     when Pᵢ names # of digits Dᵢ

    Δₒ = max allowable error contributed to the result by Δᵢ.
        Given Pₒ, the precision of the result, we must *ensure* <0.1 U:
          Δₒ = 10^(Nₒ-1)               when Pₒ names decimal position Nₒ
          Δₒ = |f(x)| * 10^(-Dₒ-1)     when Pₒ names # of digits Dₒ

    As a function of Δₒ:
        f(x ± Δᵢ) ≈ f(x) ± Δᵢ*f'(x)
        Δᵢ = Δₒ / |f'(x)|
        Nᵢ = log₁₀( Δᵢ )
           = log₁₀( Δₒ / |f'(x)| )

   Note also N <-> D relationships:
       Nₒ = log₁₀|f(x)| + 1 - Dₒ
       Dₒ = log₁₀|f(x)| + 1 - Nₒ
       Nᵢ = log₁₀|x| + 1 - Dᵢ
       Dᵢ = log₁₀|x| + 1 - Nᵢ

When Pₒ is Nₒ:

           Nᵢ = log₁₀( Δₒ / |f'(x)| )
              = log₁₀( 10^(Nₒ-1) / |f'(x)|)
              = Nₒ - 1 - log₁₀|f'(x)|

          Dᵢ = log₁₀|x| + 1 - Nᵢ
             = log₁₀|x| + 1 - (Nₒ - 1 - log₁₀|f'(x)|)
             = -Nₒ + 2 + log₁₀|x| + log₁₀|f'(x)|

When Pₒ is Dₒ:

           Nᵢ = log₁₀( Δₒ / |f'(x)| )
              = log₁₀( f(x) * 10^(-Dₒ-1) ) - log10|f'(x)|
              = log₁₀|f(x)| - Dₒ - 1 + log10|f'(x)|
              = -Dₒ - 1 + log₁₀|f(x)| - log₁₀|f'(x)|

           Dᵢ = log₁₀|x| + 1 - Nᵢ
              = log₁₀|x| + 1 - ( -Dₒ - 1 + log₁₀|f(x)| - log₁₀|f'(x)| )
              = Dₒ + 2 + log₁₀|x| - log₁₀|f(x)| + log₁₀|f'(x)|


### f(x) = log x

    f'(x) = 1/x

    Nᵢ = Nₒ - 1 - log₁₀|f'(x)|
       = Nₒ - 1 - log₁₀|1/x|
       = Nₒ - 1 + log₁₀|x|

    Dᵢ = log₁₀|x| + 1 - Nᵢ
       = log₁₀|x| + 1 - (Nₒ - 1 + log₁₀|x|)
       = -Nₒ + 2

    Nᵢ = -Dₒ - 1 + log₁₀|f(x)| - log₁₀|f'(x)|
       = -Dₒ - 1 + log₁₀|log x| - log₁₀|1/x|
       = -Dₒ - 1 + log₁₀|log x| + log₁₀|x|

    Dᵢ = Dₒ + 2 + log₁₀|x| - log₁₀|f(x)| + log₁₀|f'(x)|
       = Dₒ + 2 + log₁₀|x| - log₁₀|log x| + log₁₀|1/x|
       = Dₒ + 1 - log₁₀|log x|

       Let DD = floor( log₁₀|log x| )
          DD = 0  :  0.0000454 < x < 0.368  or  2.178 < x < 22026
          DD = -1 :      0.369 < x < 0.904  or  1.106 < x < 2.718
          DD = -2 :      0.905 < x < 0.990  or  1.010 < x < 1.105


### f(x) = log(x+1)

    f'(x) = 1/(1+x)

    Dᵢ = Dₒ + 2 + log₁₀|x| - log₁₀|log(x+1)| + log₁₀|1/(1+x)|
       = Dₒ + 1 + log₁₀|x| - log₁₀|log(x+1)| - log₁₀|1+x|

    Consider log₁₀|1+x|
      x in (-0.9, 0) : -1
      x in (0, 10)   :  0
      x in (10, 100) :  1

    Let DD = log₁₀|x| - log₁₀|log(x+1)| - log₁₀|1+x|
      -1 < DD < 0   :        0 < x < 22016
       0 < DD < 0.1 :  -0.3585 < x < 0
       0 < DD < 1   :   -0.973 < x < 0

     Dᵢ = -Nₒ + 2 + log₁₀|x| + log₁₀|f'(x)|
        = -Nₒ + 2 + log₁₀|x| + log₁₀|1/(x+1)|
        = -Nₒ + 2 + log₁₀|x/(x+1)|



### f(x) = eˣ

    f'(x) = f(x) = eˣ

    Nᵢ = Nₒ - 2 - log₁₀|f'(x)|
       = Nₒ - 2 - floor(log₁₀(eˣ))
       ≈ Nₒ - 2 - floor(log(eˣ) * 0.4343)
       ≈ Nₒ - 2 - floor(x * 0.4343)

    Nᵢ ≤ Nₒ - 1 - log₁₀|f'(x)|
       ≤ Nₒ - 1 - log₁₀|x|
    Nᵢ = Nₒ - 2 - log₁₀|x|

    Dᵢ > Dₒ + 1 + log₁₀|x| - log₁₀|f(x)| + log₁₀|f'(x)|
       > Dₒ + 1 + log₁₀|x| - log₁₀(eˣ) + log₁₀|eˣ|
    Dᵢ = Dₒ + 2 + log₁₀|x|


## Explosions

When asking for significant digits from transcendental functions whose
ranges cross 0, we run into some pathological (or at least surprising)
cases.  There is a huge mismatch between the "significant digits" in the
input and the output.  As the result approaches zero, many significant
digits *of accuracy* in the input correspond to very few significant digits
of accuracy in the output.

    > (sin (get-pi))
    2.384626433832795e-16
    > (sin (get-pi 100))
    -1.785191348671769e-101
    > (sin (get-pi 200))
    -3.557118902433407e-200

The `sin` function has no way of knowing how many of the digits of the input
are actually accurate ("significant").  Does "3" mean exactly 3, or a
single-digit approximation of something else (like, say, π)?.

Therefore, our "precision" argument requests a number of "significant" digits
assuming the inputs are *exact*.

Perhaps significant digits are not a valid way of requesting precision from
real-valued trigonometric functions.  But it remains broadly useful for
`log` across a wide range, except where `log` approaches zero.


## Complex Numbers

A question arises when applying *precision* arguments to complex values,
which are represented as a sum of real and imaginary parts.

When precision is specified as a decimal position, it can be easily applied
to the real and imaginary components independently without unexpected results.

Significant digits, however, can lead to odd results when applied
independently.  After all, 1+0.00000000001i is a funny result for (exp (* 2
(get-pi)) 1).  Geometrically, significant digits of individual components
yields a strangely shaped error region, and from that perspective, rounding
base on the magnitude -- |a+bi| -- would remedy that.

However, scaling based on max(|a|,|b|) offers some advantages:

 1. It still results in rectangular error regions.  These constrain the
    magnitude of the error within a constant factor of that of the
    magnitude-based approach: between 1/sqrt(2) and 1.

 2. Results are less confusing, since the larger component will have the
    requested number of digits (versus one fewer, which could happend with
    magnitude-based rounding).

 3. Implementation is perhaps more efficient: max(|a|,|b|) vs. a^2 + b^2.


## Algorithms

### Powers

When integer exponents are used, powers can be computed efficiently using
this approach:

     x^n = (x^2)^(n/2)          -- easily computed when n is even
         = x * (x^2)^((n-1)/2)  -- easily computed when n is odd

This translates easily to a recursive implementation:

    (define (raise-r x n)
      (cond
       ((= 1 n) x)
       ((odd? n) (* x (raise-r (* x x) (// n 2))))
       (else (raise-r (* x x) (// n 2)))))
