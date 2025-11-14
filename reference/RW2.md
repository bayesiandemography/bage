# Second-Order Random Walk Prior

Use a second-order random walk as a model for a main effect, or use
multiple second-order random walks as a model for an interaction. A
second-order random walk is a random walk with drift where the drift
term varies. It is typically used with terms that involve age or time,
where there are sustained trends upward or downward.

## Usage

``` r
RW2(s = 1, sd = 1, sd_slope = 1, along = NULL, con = c("none", "by"))
```

## Arguments

- s:

  Scale for the prior for the innovations. Default is `1`.

- sd:

  Standard deviation of initial value. Default is `1`. Can be `0`.

- sd_slope:

  Standard deviation of initial slope. Default is `1`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_rw2random"` or `"bage_prior_rw2zero"`.

## Details

If `RW2()` is used with an interaction, a separate random walk is
constructed within each combination of the 'by' variables.

Argument `s` controls the size of innovations. Smaller values for `s`
tend to give smoother series.

Argument `sd` controls variance in initial values. Setting `sd` to `0`
fixes initial values at `0`.

Argument `sd_slope` controls variance in the initial slope.

## Mathematical details

When `RW2()` is used with a main effect,

\$\$\beta_1 \sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\beta_2 \sim
\text{N}(\beta_1, \mathtt{sd\\slope}^2)\$\$ \$\$\beta_j \sim \text{N}(2
\beta\_{j-1} - \beta\_{j-2}, \tau^2), \quad j = 2, \cdots, J\$\$

and when it is used with an interaction,

\$\$\beta\_{u,1} \sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\beta\_{u,2}
\sim \text{N}(\beta\_{u,1}, \mathtt{sd\\slope}^2)\$\$ \$\$\beta\_{u,v}
\sim \text{N}(2\beta\_{u,v-1} - \beta\_{u,v-2}, \tau^2), \quad v = 3,
\cdots, V\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2)\$\$.

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

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Random walk

- [`RW2_Seas()`](https://bayesiandemography.github.io/bage/reference/RW2_Seas.md)
  Second order random walk with seasonal effect

- [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  Autoregressive with order k

- [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)
  Autoregressive with order 1

- [`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)
  Smoothing via splines

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  Smoothing over age via singular value decomposition

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
RW2()
#>   RW2() 
#>          s: 1
#>         sd: 1
#>   sd_slope: 1
#>      along: NULL
#>        con: none
RW2(s = 0.5)
#>   RW2(s=0.5) 
#>          s: 0.5
#>         sd: 1
#>   sd_slope: 1
#>      along: NULL
#>        con: none
```
