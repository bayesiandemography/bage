# Second-Order Random Walk Prior with First Order Autoregressive Errors

Use one or more second-order random walks, combined with an AR1 error
term, to model a main effect or an interaction. Typically used with
time.

## Usage

``` r
RW2_AR1(
  s_rw = 1,
  sd = 1,
  sd_slope = 1,
  s_ar = 1,
  shape1 = 5,
  shape2 = 5,
  min = 0.8,
  max = 0.98,
  along = NULL,
  con = c("none", "by")
)
```

## Arguments

- s_rw:

  Scale for the innovations in the RW2 process. Default is `1`.

- sd:

  Standard deviation for initial term in RW2 process. Default is `1`.
  Can be `0`.

- sd_slope:

  Standard deviation in the prior for the initial slope of RW2 process.
  Larger values imply steeper slopes. Default is 1.

- s_ar:

  Scale for the innovations in the AR1 process. Default is `1`.

- shape1, shape2:

  Parameters for beta-distribution prior for coefficients. Defaults are
  `5` and `5`.

- min, max:

  Minimum and maximum values for autocorrelation coefficient in AR1
  process. Defaults are `0.8` and `0.98`.

- along:

  Name of the variable to be used as the 'along' variable. Only used
  with interactions.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

## Value

An object of class `"bage_prior_rw2randomar"` or
`"bage_prior_rw2zeroar"`.

## Details

If `RW2_AR1()` is used with an interaction, separate random walks are
constructed along the 'along' variable, within each combination of the
'by' variables.

Parameters controlling the RW2 process:

- `s_rw`

- `sd`

- `sd_slope`

Parameters controlling the AR1 process:

- `s_ar`

- `shape1`

- `shape2`

- `min`

- `max`

## Mathematical details

When `RW2_AR1()` is used with a main effect,

\$\$\beta_j = \alpha_j + \epsilon_j\$\$ \$\$\alpha_1 \sim \text{N}(0,
\mathtt{sd}^2)\$\$ \$\$\alpha_2 \sim \text{N}(\alpha_1,
\mathtt{sd\\slope}^2)\$\$ \$\$\alpha_j \sim \text{N}(2\alpha\_{j-1} -
\alpha\_{j-2}, \tau^2), \quad j = 3, \cdots, J\$\$ \$\$\epsilon_j = \phi
\epsilon\_{j-1} + \varepsilon_j\$\$ \$\$\varepsilon_j \sim \text{N}(0,
\omega^2),\$\$

and when it is used with an interaction,

\$\$\beta\_{u,v} = \alpha\_{u,v} + \epsilon\_{u,v}\$\$ \$\$\alpha\_{u,1}
\sim \text{N}(0, \mathtt{sd}^2)\$\$ \$\$\alpha\_{u,2} \sim
\text{N}(\alpha\_{u,1}, \mathtt{sd\\slope}^2)\$\$ \$\$\alpha\_{u,v} \sim
\text{N}(2\alpha\_{u,v-1} - \alpha\_{u,v-2}, \tau^2), \quad v = 3,
\cdots, V\$\$ \$\$\epsilon\_{u,v} = \phi \epsilon\_{u,v-1} +
\varepsilon\_{u,v}\$\$ \$\$\varepsilon\_{u,v} \sim \text{N}(0,
\omega^2),\$\$

where

- \\\pmb{\beta}\\ is the main effect or interaction;

- \\j\\ denotes position within the main effect;

- \\u\\ denotes position within the 'by' variable(s) of the interaction;
  and

- \\v\\ denotes position within the 'along' variable of the interaction.

The \\\tau\\ parameter in the random walk has prior \$\$\tau \sim
\text{N}^+(0, \mathtt{s\\rw}^2)\$\$

Internally,
[`RW2_AR()`](https://bayesiandemography.github.io/bage/reference/RW2_AR.md)
derives a value for \\\omega\\ that gives \\\epsilon_j\\ or
\\\epsilon\_{u,v}\\ a marginal variance of \\\nu^2\\. Parameter \\\nu\\
has a half-normal prior \$\$\nu \sim \text{N}^+(0,
\mathtt{s\\ar}^2).\$\$

Coefficient \\\phi\\ is constrained to lie between `min` and `max`. Its
prior distribution is

\$\$\phi = (\mathtt{max} - \mathtt{min}) \phi' - \mathtt{min}\$\$

where

\$\$\phi' \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).\$\$

## Constraints

With some combinations of terms and priors, the values of the intercept,
main effects, and interactions are only weakly identified. This weak
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

- [`RW2_AR()`](https://bayesiandemography.github.io/bage/reference/RW2_AR.md)
  Generalization of `RW2_AR1()`

- [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)
  Sepcial case of `RW2_AR1()`

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Second-order random walk

- [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)
  AR1 process

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
RW2_AR1()
#>   RW2_AR1() 
#>       s_rw: 1
#>         sd: 1
#>   sd_slope: 1
#>       s_ar: 1
#>     shape1: 5
#>     shape2: 5
#>        min: 0.8
#>        max: 0.98
#>      along: NULL
#>        con: none
RW2_AR1(sd_slope = 2, s_ar = 0.5)
#>   RW2_AR1(sd_slope=2,s_ar=0.5) 
#>       s_rw: 1
#>         sd: 1
#>   sd_slope: 2
#>       s_ar: 0.5
#>     shape1: 5
#>     shape2: 5
#>        min: 0.8
#>        max: 0.98
#>      along: NULL
#>        con: none
```
