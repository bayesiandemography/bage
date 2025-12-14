# Autoregressive Prior

Use an autoregressive process to model a main effect, or use multiple
autoregressive processes to model an interaction. Typically used with
time effects or with interactions that involve time.

## Usage

``` r
AR(
  n_coef = 2,
  s = 1,
  shape1 = 5,
  shape2 = 5,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- n_coef:

  Number of lagged terms in the model, ie the order of the model.
  Default is `2`.

- s:

  Scale for the prior for the innovations. Default is `1`.

- shape1, shape2:

  Parameters for beta-distribution prior for coefficients. Defaults are
  `5` and `5`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  **\[experimental\]** Constraints on parameters. Current choices are
  `"none"` and `"by"`. Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_ar"`.

## Details

If `AR()` is used with an interaction, then separate AR processes are
constructed along the 'along' variable, within each combination of the
'by' variables.

By default, the autoregressive processes have order 2. Alternative
choices can be specified through the `n_coef` argument.

Argument `s` controls the size of innovations. Smaller values for `s`
tend to give smoother estimates.

## Mathematical details

When `AR()` is used with a main effect,

\$\$\beta_j = \phi_1 \beta\_{j-1} + \cdots + \phi\_{\mathtt{n\\coef}}
\beta\_{j-\mathtt{n\\coef}} + \epsilon_j\$\$ \$\$\epsilon_j \sim
\text{N}(0, \omega^2),\$\$

and when it is used with an interaction,

\$\$\beta\_{u,v} = \phi_1 \beta\_{u,v-1} + \cdots +
\phi\_{\mathtt{n\\coef}} \beta\_{u,v-\mathtt{n\\coef}} +
\epsilon\_{u,v}\$\$ \$\$\epsilon\_{u,v} \sim \text{N}(0, \omega^2),\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\v\\ denotes position within the 'along' variable of the interaction;
  and

- \\u\\ denotes position within the 'by' variable(s) of the interaction.

Internally, `AR()` derives a value for \\\omega\\ that gives every
element of \\\beta\\ a marginal variance of \\\tau^2\\. Parameter
\\\tau\\ has a half-normal prior

\$\$\tau \sim \text{N}^+(0, \mathtt{s}^2).\$\$

The correlation coefficients \\\phi_1, \cdots,
\phi\_{\mathtt{n\\coef}}\\ each have prior

\$\$\phi_k \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).\$\$

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

## References

- `AR()` is based on the TMB function
  [ARk](http://kaskr.github.io/adcomp/classdensity_1_1ARk__t.html#details)

## See also

- [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)
  Special case of `AR()`. Can be more numerically stable than
  higher-order models.

- [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md),
  [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)
  Straight line with AR errors

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
AR(n_coef = 3)
#>   AR(n_coef=3) 
#>     n_coef: 3
#>        min: -1
#>        max: 1
#>          s: 1
#>      along: NULL
#>        con: none
AR(n_coef = 3, s = 2.4)
#>   AR(n_coef=3,s=2.4) 
#>     n_coef: 3
#>        min: -1
#>        max: 1
#>          s: 2.4
#>      along: NULL
#>        con: none
AR(along = "cohort")
#>   AR(along="cohort") 
#>     n_coef: 2
#>        min: -1
#>        max: 1
#>          s: 1
#>      along: cohort
#>        con: none
```
