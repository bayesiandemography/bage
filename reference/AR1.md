# Autoregressive Prior of Order 1

Use an autoregressive process of order 1 to model a main effect, or use
multiple AR1 processes to model an interaction. Typically used with time
effects or with interactions that involve time.

## Usage

``` r
AR1(
  s = 1,
  shape1 = 5,
  shape2 = 5,
  min = 0.8,
  max = 0.98,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- s:

  Scale for the prior for the innovations. Default is `1`.

- shape1, shape2:

  Parameters for beta-distribution prior for coefficients. Defaults are
  `5` and `5`.

- min, max:

  Minimum and maximum values for autocorrelation coefficient. Defaults
  are `0.8` and `0.98`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_ar"`.

## Details

If [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
is used with an interaction, separate AR processes are constructed along
the 'along' variable, within each combination of the 'by' variables.

Arguments `min` and `max` can be used to specify the permissible range
for autocorrelation.

Argument `s` controls the size of innovations. Smaller values for `s`
tend to give smoother estimates.

## Mathematical details

When `AR1()` is used with a main effect,

\$\$\beta_j = \phi \beta\_{j-1} + \epsilon_j\$\$ \$\$\epsilon_j \sim
\text{N}(0, \omega^2),\$\$

and when it is used with an interaction,

\$\$\beta\_{u,v} = \phi \beta\_{u,v-1} + \epsilon\_{u,v}\$\$
\$\$\epsilon\_{u,v} \sim \text{N}(0, \omega^2),\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Internally, `AR1()` derives a value for \\\omega\\ that gives every
element of \\\beta\\ a marginal variance of \\\tau^2\\. Parameter
\\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2),\$\$ where `s` is provided by the user.

Coefficient \\\phi\\ is constrained to lie between `min` and `max`. Its
prior distribution is

\$\$\phi = (\mathtt{max} - \mathtt{min}) \phi' - \mathtt{min}\$\$

where

\$\$\phi' \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).\$\$

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

- `AR1()` is based on the TMB function
  [AR1](http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.html#details)

- The defaults for `min` and `max` are based on the defaults for
  `forecast::ets()`.

## See also

- [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  Generalization of `AR1()`

- [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md),
  [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)
  Line with AR errors

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
AR1()
#>   AR1() 
#>        min: 0.8
#>        max: 0.98
#>          s: 1
#>      along: NULL
#>        con: none
AR1(min = 0, max = 1, s = 2.4)
#>   AR1(s=2.4,min=0,max=1) 
#>        min: 0
#>        max: 1
#>          s: 2.4
#>      along: NULL
#>        con: none
AR1(along = "cohort")
#>   AR1(along="cohort") 
#>        min: 0.8
#>        max: 0.98
#>          s: 1
#>      along: cohort
#>        con: none
```
