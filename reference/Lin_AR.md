# Linear Prior with Autoregressive Errors

Use a line or lines with autoregressive errors to model a main effect or
interaction. Typically used with time.

## Usage

``` r
Lin_AR(
  n_coef = 2,
  s = 1,
  shape1 = 5,
  shape2 = 5,
  mean_slope = 0,
  sd_slope = 1,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- n_coef:

  Number of lagged terms in the model, ie the order of the model.
  Default is `2`.

- s:

  Scale for the innovations in the AR process. Default is `1`.

- shape1, shape2:

  Parameters for beta-distribution prior for coefficients. Defaults are
  `5` and `5`.

- mean_slope:

  Mean in prior for slope of line. Default is 0.

- sd_slope:

  Standard deviation in the prior for the slope of the line. Larger
  values imply steeper slopes. Default is 1.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  **\[experimental\]** Constraints on parameters. Current choices are
  `"none"` and `"by"`. Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_linar"`.

## Details

If `Lin_AR()` is used with an interaction, separate lines are
constructed along the 'along' variable, within each combination of the
'by' variables.

The order of the autoregressive errors is controlled by the `n_coef`
argument. The default is 2.

Argument `s` controls the size of the innovations. Smaller values tend
to give smoother estimates.

Argument `sd_slope` controls the slopes of the lines. Larger values can
give more steeply sloped lines.

## Mathematical details

When `Lin_AR()` is used with a main effect,

\$\$\beta_1 = \alpha + \epsilon_1\$\$ \$\$\beta_j = \alpha + (j - 1)
\eta + \epsilon_j, \quad j \> 1\$\$ \$\$\alpha \sim \text{N}(0, 1)\$\$
\$\$\epsilon_j = \phi_1 \epsilon\_{j-1} + \cdots +
\phi\_{\mathtt{n\\coef}} \epsilon\_{j-\mathtt{n\\coef}} +
\varepsilon_j\$\$ \$\$\varepsilon_j \sim \text{N}(0, \omega^2),\$\$

and when it is used with an interaction,

\$\$\beta\_{u,1} = \alpha_u + \epsilon\_{u,1}\$\$ \$\$\beta\_{u,v} =
\eta (v - 1) + \epsilon\_{u,v}, \quad v = 2, \cdots, V\$\$ \$\$\alpha_u
\sim \text{N}(0, 1)\$\$ \$\$\epsilon\_{u,v} = \phi_1 \epsilon\_{u,v-1} +
\cdots + \phi\_{\mathtt{n\\coef}} \epsilon\_{u,v-\mathtt{n\\coef}} +
\varepsilon\_{u,v},\$\$ \$\$\varepsilon\_{u,v} \sim \text{N}(0,
\omega^2).\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\u\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

The slopes have priors \$\$\eta \sim \text{N}(\mathtt{mean\\slope},
\mathtt{sd\\slope}^2)\$\$ and \$\$\eta_u \sim
\text{N}(\mathtt{mean\\slope}, \mathtt{sd\\slope}^2).\$\$

Internally, `Lin_AR()` derives a value for \\\omega\\ that gives
\\\epsilon_j\\ or \\\epsilon\_{u,v}\\ a marginal variance of \\\tau^2\\.
Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2).\$\$

The correlation coefficients \\\phi_1, \cdots,
\phi\_{\mathtt{n\\coef}}\\ each have prior

\$\$0.5 \phi_k - 0.5 \sim \text{Beta}(\mathtt{shape1},
\mathtt{shape2}).\$\$

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

- [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)
  Special case of `Lin_AR()`

- [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)
  Line with independent normal errors

- [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md) AR
  process with no line

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
Lin_AR()
#>   Lin_AR() 
#>     n_coef: 2
#>          s: 1
#> mean_slope: 0
#>   sd_slope: 1
#>        min: -1
#>        max: 1
#>      along: NULL
#>        con: none
Lin_AR(n_coef = 3, s = 0.5, sd_slope = 2)
#>   Lin_AR(n_coef=3,s=0.5,sd_slope=2) 
#>     n_coef: 3
#>          s: 0.5
#> mean_slope: 0
#>   sd_slope: 2
#>        min: -1
#>        max: 1
#>      along: NULL
#>        con: none
```
