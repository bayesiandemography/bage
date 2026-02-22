# Random Walk Prior with Seasonal Effect

Use a random walk with seasonal effects as a model for a main effect, or
use multiple random walks, each with their own seasonal effects, as a
model for an interaction. Typically used with terms that involve time.

## Usage

``` r
RW_Seas(
  n_seas,
  s_seas,
  s = 1,
  sd = 1,
  sd_seas = 1,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- n_seas:

  Number of seasons

- s_seas:

  Scale for innovations in seasonal effects. Can be `0`. When greater
  than 0, seasonal effects vary from year to year.

- s:

  Scale for prior for innovations in random walk. Default is `1`.

- sd:

  Standard deviation of initial value. Default is `1`. Can be `0`.

- sd_seas:

  Standard deviation for initial values of seasonal effects. Default is
  `1`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  **\[experimental\]** Constraints on parameters. Current choices are
  `"none"` and `"by"`. Default is `"none"`. See below for details.

## Value

Object of class `"bage_prior_rwrandomseasvary"`,
`"bage_prior_rwrandomseasfix"`, `"bage_prior_rwzeroseasvary"`, or
`"bage_prior_rwzeroseasfix"`.

## Details

If `RW_Seas()` is used with an interaction, a separate series is
constructed within each combination of the 'by' variables.

Argument `s` controls the size of innovations in the random walk.
Smaller values for `s` tend to produce smoother series.

Argument `sd` controls variance in initial values of the random walk.
`sd` can be `0`.

Argument `n_seas` controls the number of seasons. When using quarterly
data, for instance, `n_seas` should be `4`.

Setting `s_seas` to `0` produces seasonal effects that are the same each
year. Setting `s_seas` to a value greater than `0` produces seasonal
effects that evolve over time.

## Mathematical details

When `RW_Seas()` is used with a main effect,

\$\$\beta_j = \alpha_j + \lambda_j, \quad j = 1, \cdots, J\$\$
\$\$\alpha_1 \sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\alpha_j \sim
\text{N}(\alpha\_{j-1}, \tau^2), \quad j = 2, \cdots, J\$\$
\$\$\lambda_j \sim \text{N}(0, \mathtt{sd\\seas}^2), \quad j = 1,
\cdots, \mathtt{n\\seas} - 1\$\$ \$\$\lambda_j =
-\sum\_{s=1}^{\mathtt{n\\seas} - 1} \lambda\_{j - s}, \quad j =
\mathtt{n\\seas}, 2 \mathtt{n\\seas}, \cdots\$\$ \$\$\lambda_j \sim
\text{N}(\lambda\_{j-\mathtt{n\\seas}}, \omega^2), \quad
\text{otherwise},\$\$

and when it is used with an interaction,

\$\$\beta\_{u,v} = \alpha\_{u,v} + \lambda\_{u,v}, \quad v = 1, \cdots,
V\$\$ \$\$\alpha\_{u,1} \sim \text{N}(0, \mathtt{sd}^2)\$\$
\$\$\alpha\_{u,v} \sim \text{N}(\alpha\_{u,v-1}, \tau^2), \quad v = 2,
\cdots, V\$\$ \$\$\lambda\_{u,v} \sim \text{N}(0, \mathtt{sd\\seas}^2),
\quad v = 1, \cdots, \mathtt{n\\seas} - 1\$\$ \$\$\lambda\_{u,v} =
-\sum\_{s=1}^{\mathtt{n\\seas} - 1} \lambda\_{u,v - s}, \quad v =
\mathtt{n\\seas}, 2 \mathtt{n\\seas}, \cdots\$\$ \$\$\lambda\_{u,v} \sim
\text{N}(\lambda\_{u,v-\mathtt{n\\seas}}, \omega^2), \quad
\text{otherwise},\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\\alpha_j\\ or \\\alpha\_{u,v}\\ is an element of the random walk;

- \\\lambda_j\\ or \\\lambda\_{u,v}\\ is an element of the seasonal
  effect;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Parameter \\\omega\\ has a half-normal prior \$\$\omega \sim
\text{N}^+(0, \mathtt{s\\seas}^2).\$\$ If `s_seas` is set to 0, then
\\\omega\\ is 0, and seasonal effects are time-invariant.

Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2).\$\$

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

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Random walk without seasonal effect

- [`RW2_Seas()`](https://bayesiandemography.github.io/bage/reference/RW2_Seas.md)
  Second-order random walk with seasonal effect

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## seasonal effects fixed
RW_Seas(n_seas = 4, s_seas = 0)
#>   RW_Seas(n_seas=4,s_seas=0) 
#>     n_seas: 4
#>     s_seas: 0
#>          s: 1
#>         sd: 1
#>    sd_seas: 1
#>      along: NULL
#>        con: none

## seasonal effects evolve
RW_Seas(n_seas = 4, s_seas = 1)
#>   RW_Seas(n_seas=4,s_seas=1) 
#>     n_seas: 4
#>     s_seas: 1
#>          s: 1
#>         sd: 1
#>    sd_seas: 1
#>      along: NULL
#>        con: none

## first term in random walk fixed at 0
RW_Seas(n_seas = 4, s_seas = 1, sd = 0)       
#>   RW_Seas(n_seas=4,s_seas=1,sd=0) 
#>     n_seas: 4
#>     s_seas: 1
#>          s: 1
#>         sd: 0
#>    sd_seas: 1
#>      along: NULL
#>        con: none
```
