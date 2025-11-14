# P-Spline Prior

Use a p-spline (penalised spline) to model main effects or interactions.
Typically used with age, but can be used with any variable where
outcomes are expected to vary smoothly from one element to the next.

## Usage

``` r
Sp(
  n_comp = NULL,
  s = 1,
  sd = 1,
  sd_slope = 1,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- n_comp:

  Number of spline basis functions (components) to use.

- s:

  Scale for the prior for the innovations. Default is `1`.

- sd:

  Standard deviation in prior for first element of random walk.

- sd_slope:

  Standard deviation in prior for initial slope of random walk. Default
  is `1`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_spline"`.

## Details

If `Sp()` is used with an interaction, separate splines are used for the
'along' variable within each combination of the 'by' variables.

## Mathematical details

When `Sp()` is used with a main effect,

\$\$\pmb{\beta} = \pmb{X} \pmb{\alpha}\$\$

and when it is used with an interaction,

\$\$\pmb{\beta}\_u = \pmb{X} \pmb{\alpha}\_u\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction, with \\J\\
  elements;

- \\\pmb{\beta}\_u\\ is a subvector of \\\pmb{\beta}\\ holding values
  for the \\u\\th combination of the 'by' variables;

- \\J\\ is the number of elements of \\\pmb{\beta}\\;

- \\U\\ is the number of elements of \\\pmb{\beta}\_u\\;

- \\X\\ is a \\J \times n\\ or \\V \times n\\ matrix of spline basis
  functions; and

- \\n\\ is `n_comp`.

The elements of \\\pmb{\alpha}\\ or \\\pmb{\alpha}\_u\\ are assumed to
follow a [second-order random
walk](https://bayesiandemography.github.io/bage/reference/RW2.md).

## Constraints

With some combinations of terms and priors, the values of the intercept,
main effects, and interactions are are only weakly identified. For
instance, it may be possible to increase the value of the intercept and
reduce the value of the remaining terms in the model with no effect on
predicted rates and only a tiny effect on prior probabilities. This weak
identifiability is typically harmless. However, in some applications,
such as when trying to obtain interpretable values for main effects and
interactions, it can be helpful to increase identifiability through the
use of constraints, specified through the `con` argument.

Current options for `con` are:

- `"none"` No constraints. The default.

- `"by"` Only used in interaction terms that include 'along' and 'by'
  dimensions. Within each value of the 'along' dimension, terms across
  each 'by' dimension are constrained to sum to 0.

## References

- Eilers, P.H.C. and Marx B. (1996). "Flexible smoothing with B-splines
  and penalties". Statistical Science. 11 (2): 89–121.

## See also

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Smoothing via random walk

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Smoothing via second-order random walk

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  Smoothing of age via singular value decomposition

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [`splines::bs()`](https://rdrr.io/r/splines/bs.html) Function used by
  bage to construct spline basis functions

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
Sp()
#>   Sp() 
#>     n_comp: NULL
#>          s: 1
#>         sd: 1
#>   sd_slope: 1
#>      along: NULL
#>        con: none
Sp(n_comp = 10)
#>   Sp(n_comp=10) 
#>     n_comp: 10
#>          s: 1
#>         sd: 1
#>   sd_slope: 1
#>      along: NULL
#>        con: none
```
