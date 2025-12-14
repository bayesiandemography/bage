# Second-Order Random Walk Prior with 'Infant' Indicator

Use a second-order random walk to model variation over age, with an
indicator variable for the first age group. Designed for use in models
of mortality rates.

## Usage

``` r
RW2_Infant(s = 1, sd_slope = 1, con = c("none", "by"))
```

## Arguments

- s:

  Scale for the prior for the innovations. Default is `1`.

- sd_slope:

  Standard deviation for initial slope of random walk. Default is `1`.

- con:

  **\[experimental\]** Constraints on parameters. Current choices are
  `"none"` and `"by"`. Default is `"none"`. See below for details.

## Value

Object of class `"bage_prior_rw2infant"`.

## Details

A second-order random walk prior
[`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
works well for smoothing mortality rates over age, except at age 0,
where there is a sudden jump in rates, reflecting the special risks of
infancy. The `RW2_Infant()` extends the
[`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
prior by adding an indicator variable for the first age group.

If `RW2_Infant()` is used in an interaction, the 'along' dimension is
always age, implying that there is a separate random walk along age
within each combination of the 'by' variables.

Argument `s` controls the size of innovations in the random walk.
Smaller values for `s` tend to give smoother series.

Argument `sd` controls the sl size of innovations in the random walk.
Smaller values for `s` tend to give smoother series.

## Mathematical details

When `RW2_Infant()` is used with a main effect,

\$\$\beta_1 \sim \text{N}(0, 1)\$\$ \$\$\beta_2 \sim \text{N}(0,
\mathtt{sd\\slope}^2)\$\$ \$\$\beta_3 \sim \text{N}(2 \beta_2,
\tau^2)\$\$ \$\$\beta_j \sim \text{N}(2 \beta\_{j-1} - \beta\_{j-2},
\tau^2), \quad j = 3, \cdots, J\$\$

and when it is used with an interaction,

\$\$\beta\_{u,1} \sim \text{N}(0, 1)\$\$ \$\$\beta\_{u,2} \sim
\text{N}(0, \mathtt{sd\\slope}^2)\$\$ \$\$\beta\_{u,3} \sim \text{N}(2
\beta\_{u,2}, \tau^2)\$\$ \$\$\beta\_{u,v} \sim \text{N}(2
\beta\_{u,v-1} - \beta\_{u,v-2}, \tau^2), \quad v = 3, \cdots, V\$\$

where

- \\\pmb{\beta}\\ is a main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2)\$\$.

## Constraints

**\[experimental\]** The specification of constraints is likely to
change in future versions of bage.

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

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Second-order random walk, without infant indicator

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
RW2_Infant()
#>   RW2_Infant() 
#>          s: 1
#>   sd_slope: 1
#>        con: none
RW2_Infant(s = 0.1)
#>   RW2_Infant(s=0.1) 
#>          s: 0.1
#>   sd_slope: 1
#>        con: none
```
