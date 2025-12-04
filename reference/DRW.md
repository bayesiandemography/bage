# Damped Random Walk Prior

Use a damped random walk as a model for a main effect, or use multiple
damped random walks as a model for an interaction. Typically used with
terms that involve time, particularly in forecasts. Damping often
improves forecast accuracy.

## Usage

``` r
DRW(
  s = 1,
  sd = 1,
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

- sd:

  Standard deviation of initial value. Default is `1`. Can be `0`.

- shape1, shape2:

  Parameters for beta-distribution prior for damping coefficient.
  Defaults are `5` and `5`.

- min, max:

  Minimum and maximum values for damping coefficient. Defaults are `0.8`
  and `0.98`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_drwrandom"` or `"bage_prior_drwzero"`.

## Details

If `DRW()` is used with an interaction, a separate damped random walk is
constructed within each combination of the 'by' variables.

Arguments `min` and `max` can be used to control the amount of damping
that occurs.

Argument `s` controls the size of innovations. Smaller values for `s`
tend to produce smoother series.

Argument `sd` controls variance in initial values. Setting `sd` to `0`
fixes initial values at 0.

## Mathematical details

When `DRW()` is used with a main effect,

\$\$\beta_1 \sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\beta_j \sim
\text{N}(\phi \beta\_{j-1}, \tau^2), \quad j \> 1\$\$

and when it is used with an interaction,

\$\$\beta\_{u,1} \sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\beta\_{u,v}
\sim \text{N}(\phi \beta\_{u,v-1}, \tau^2), \quad v \> 1\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\\phi\\ is the damping coefficient;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Coefficient \\\phi\\ is constrained to lie between `min` and `max`. Its
prior distribution is

\$\$\phi = (\mathtt{max} - \mathtt{min}) \phi' - \mathtt{min}\$\$

where

\$\$\phi' \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).\$\$

Standard deviation \\\tau\\ has a half-normal prior \$\$\tau \sim
\text{N}^+(0, \mathtt{s}^2),\$\$ where `s` is provided by the user.

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

## See also

- [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md)
  Damped second-order random walk

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Random walk, without damping

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Second-order random walk, without damping

- [`RW_Seas()`](https://bayesiandemography.github.io/bage/reference/RW_Seas.md)
  Random walk with seasonal effect

- [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  Autoregressive with order k

- [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)
  Autoregressive with order 1

- [`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)
  Smoothing via splines

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  Smoothing over age using singular value decomposition

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
DRW()
#>   DRW() 
#>          s: 1
#>         sd: 1
#>     shape1: 5
#>     shape2: 5
#>        min: 0.8
#>        max: 0.98
#>      along: NULL
#>        con: none
DRW(min = 0, max = 1)
#>   DRW(min=0,max=1) 
#>          s: 1
#>         sd: 1
#>     shape1: 5
#>     shape2: 5
#>        min: 0
#>        max: 1
#>      along: NULL
#>        con: none
DRW(sd = 0)
#>   DRW(sd=0) 
#>          s: 1
#>         sd: 0
#>     shape1: 5
#>     shape2: 5
#>        min: 0.8
#>        max: 0.98
#>      along: NULL
#>        con: none
```
