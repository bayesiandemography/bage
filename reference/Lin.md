# Linear Prior with Independent Normal Errors

Use a line or lines with independent normal errors to model a main
effect or interaction. Typically used with time.

## Usage

``` r
Lin(s = 1, mean_slope = 0, sd_slope = 1, along = NULL, con = c("none", "by"))
```

## Arguments

- s:

  Scale for the prior for the errors. Default is `1`. Can be `0`.

- mean_slope:

  Mean in prior for slope of line. Default is 0.

- sd_slope:

  Standard deviation in prior for slope of line. Default is 1.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  **\[experimental\]** Constraints on parameters. Current choices are
  `"none"` and `"by"`. Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_lin"`.

## Details

If `Lin()` is used with an interaction, then separate lines are
constructed along the 'along' variable, within each combination of the
'by' variables.

Argument `s` controls the size of the errors. Smaller values give
smoother estimates. `s` can be zero, in which case errors are zero, and
all values lie exactly on straight lines. This is clearly a
simplification, but it allows the prior to be used with very large
interactions.

Argument `sd_slope` controls the size of the slopes of the lines. Larger
values can give more steeply sloped lines.

## Mathematical details

When `Lin()` is used with a main effect,

\$\$\beta_j = (j - (J+1)/2) \eta + \epsilon_j\$\$ \$\$\eta \sim
\text{N}(\mathtt{mean\\slope}, \mathtt{sd\\slope}^2)\$\$ \$\$\epsilon_j
\sim \text{N}(0, \tau^2),\$\$

and when it is used with an interaction,

\$\$\beta\_{u,v} = (v - (V + 1)/2) \eta_u + \epsilon\_{u,v}\$\$
\$\$\eta_u \sim \text{N}(\mathtt{mean\\slope}, \mathtt{sd\\slope}^2)\$\$
\$\$\epsilon\_{u,v} \sim \text{N}(0, \tau^2),\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2).\$\$

When \\\mathtt{s} = 0\\, the model reduces to

\$\$\beta_j = (j - (J+1)/2) \eta\$\$ \$\$\eta \sim
\text{N}(\mathtt{mean\\slope}, \mathtt{sd\\slope}^2)\$\$

or

\$\$\beta\_{u,v} = (v = (V + 1)/2) \eta_u\$\$ \$\$\eta_u \sim
\text{N}(\mathtt{mean\\slope}, \mathtt{sd\\slope}^2)\$\$.

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

- [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)
  Linear with AR errors

- [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)
  Linear with AR1 errors

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Second-order random walk

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
Lin()
#>   Lin() 
#>          s: 1
#> mean_slope: 0
#>   sd_slope: 1
#>      along: NULL
#>        con: none
Lin(s = 0.5, sd_slope = 2)
#>   Lin(s=0.5,sd_slope=2) 
#>          s: 0.5
#> mean_slope: 0
#>   sd_slope: 2
#>      along: NULL
#>        con: none
Lin(s = 0)
#>   Lin(s=0) 
#>          s: 0
#> mean_slope: 0
#>   sd_slope: 1
#>      along: NULL
#>        con: none
Lin(along = "cohort")
#>   Lin(along="cohort") 
#>          s: 1
#> mean_slope: 0
#>   sd_slope: 1
#>      along: cohort
#>        con: none
```
