# Dynamic SVD-Based Priors for Age Profiles or Age-Sex Profiles

Use components from a Singular Value Decomposition (SVD) to model an
interaction involving age and time, or age, sex/gender and time, where
the coefficients evolve over time.

## Usage

``` r
SVD_AR(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  n_coef = 2,
  s = 1,
  shape1 = 5,
  shape2 = 5,
  con = c("none", "by")
)

SVD_AR1(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  min = 0.8,
  max = 0.98,
  s = 1,
  shape1 = 5,
  shape2 = 5,
  con = c("none", "by")
)

SVD_DRW(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  s = 1,
  sd = 1,
  min = 0.8,
  max = 0.98,
  shape1 = 5,
  shape2 = 5,
  con = c("none", "by")
)

SVD_DRW2(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  s = 1,
  sd = 1,
  sd_slope = 1,
  min = 0.8,
  max = 0.98,
  shape1 = 5,
  shape2 = 5,
  con = c("none", "by")
)

SVD_RW(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  s = 1,
  sd = 1,
  con = c("none", "by")
)

SVD_RW2(
  ssvd,
  v = NULL,
  n_comp = NULL,
  indep = TRUE,
  s = 1,
  sd = 1,
  sd_slope = 1,
  con = c("none", "by")
)
```

## Arguments

- ssvd:

  Object of class `"bage_ssvd"` holding a scaled SVD. See below for
  scaled SVDs of databases currently available in bage.

- v:

  Version of scaled SVD components to use. If no value is suppled, the
  most recent version is used.

- n_comp:

  Number of components from scaled SVD to use in modelling. The default
  is half the number of components of `ssvd`.

- indep:

  Whether to use separate or combined SVDs in terms involving sex or
  gender. Default is `TRUE`. See below for details.

- n_coef:

  Number of AR coefficients in `SVD_RW()`.

- s:

  Scale for standard deviations terms.

- shape1, shape2:

  Parameters for prior for coefficients in `SVD_AR()`, `SVD_DRW()`, and
  `SVD_DRW2()`. Defaults are `5` and `5`.

- con:

  Constraints on parameters. Current choices are `"none"` and `"by"`.
  Default is `"none"`. See below for details.

- min, max:

  Minimum and maximum values for autocorrelation coefficient in
  `SVD_AR1()`, `SVD_DRW()`, and `SVD_DRW2()`. Defaults are `0.8` and
  `0.98`.

- sd:

  Standard deviation of initial value for random walks. Default is `1`.
  Can be `0`.

- sd_slope:

  Standard deviation in prior for initial slope. Default is `1`.

## Value

An object inheriting from class `"bage_prior"`.

## Details

`SVD_AR()`, `SVD_AR1()`, `SVD_RW()`, `SVD_RW2()`, `SVD_DRW()`, and
`SVD_RW2()`, priors assume that, in any given period, the age profiles
or age-sex profiles for the quantity being modelled looks like they were
drawn at random from an external demographic database. For instance, the
`SVD_AR()` prior obtained via

    SVD_AR(HMD)

assumes that profiles look like they were obtained from the [Human
Mortality Database](https://www.mortality.org).

## Mathematical details

When the interaction being modelled only involves age and time, or age,
sex/gender, and time

\$\$\pmb{\beta}\_t = \pmb{F} \pmb{\alpha}\_t + \pmb{g},\$\$

and when it involves other variables besides age, sex/gender, and time,

\$\$\pmb{\beta}\_{u,t} = \pmb{F} \pmb{\alpha}\_{u,t} + \pmb{g},\$\$

where

- \\\pmb{\beta}\\ is an interaction involving age, time, possibly
  sex/gender, and possibly other variables;

- \\\pmb{\beta}\_t\\ is a subvector of \\\pmb{\beta}\\ holding values
  for period \\t\\;

- \\\pmb{\beta}\_{u,t}\\ is a subvector of \\\pmb{\beta}\_t\\ holding
  values for the \\u\\th combination of the non-age, non-time,
  non-sex/gender variables for period \\t\\;

- \\\pmb{F}\\ is a known matrix; and

- \\\pmb{g}\\ is a known vector.

\\\pmb{F}\\ and \\\pmb{g}\\ are constructed from a large database of
age-specific demographic estimates by applying the singular value
decomposition, and then standardizing.

With `SVD_AR()`, the prior for the \\k\\th element of
\\\pmb{\alpha}\_t\\ or \\\pmb{\alpha}\_{u,t}\\ is

\$\$\alpha\_{k,t} = \phi_1 \alpha\_{k,t-1} + \cdots + \phi_n
\beta\_{k,t-n} + \epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = \phi_1 \alpha\_{k,u,t-1} + \cdots + \phi_n
\beta\_{k,u,t-n} + \epsilon\_{k,u,t};\$\$

with `SVD_AR1()`, it is

\$\$\alpha\_{k,t} = \phi \alpha\_{k,t-1} + \epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = \phi \alpha\_{k,u,t-1} + \epsilon\_{k,u,t};\$\$

with `SVD_RW()`, it is

\$\$\alpha\_{k,t} = \alpha\_{k,t-1} + \epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = \alpha\_{k,u,t-1} + \epsilon\_{k,u,t};\$\$

and with `SVD_RW2()`, it is

\$\$\alpha\_{k,t} = 2 \alpha\_{k,t-1} - \alpha\_{k,t-2} +
\epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = 2 \alpha\_{k,u,t-1} - \alpha\_{k,u,t-2} +
\epsilon\_{k,u,t};\$\$

with `SVD_DRW()`, it is

\$\$\alpha\_{k,t} = \phi \alpha\_{k,t-1} + \epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = \phi \alpha\_{k,u,t-1} + \epsilon\_{k,u,t};\$\$

and with `SVD_DRW2()`, it is

\$\$\alpha\_{k,t} = \alpha\_{k,t-1} + \phi (\alpha\_{k,t-1} -
\alpha\_{k,t-2}) + \epsilon\_{k,t}\$\$

or

\$\$\alpha\_{k,u,t} = \alpha\_{k,u,t-1} + \phi (\alpha\_{k,u,t-1} -
\alpha\_{k,u,t-2}) + \epsilon\_{k,u,t}.\$\$

`SVD_AR1()` and `SVD_DRW()` are almost but not quite identical. In
`SVD_AR1()`, the variance of \\\epsilon_t\\ is chosen so that
\\\alpha_t\\ has marginal variance \\\tau^2\\, while in `SVD_DRW()`,
\\\epsilon_t\\ has variance \\\tau^2\\.

For details on the time series models, see
[`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md),
[`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md),
[`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
[`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md),
[`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md),
and
[`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md).

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

## Scaled SVDs of demographic databases in bage

- [`HMD`](https://bayesiandemography.github.io/bage/reference/HMD.md)
  Mortality rates from the [Human Mortality
  Database](https://www.mortality.org).

- [`HFD`](https://bayesiandemography.github.io/bage/reference/HFD.md)
  Fertility rates from the [Human Fertility
  Database](https://www.humanfertility.org).

- [`LFP`](https://bayesiandemography.github.io/bage/reference/LFP.md)
  Labor forcce participation rates from the
  [OECD](https://data-explorer.oecd.org).

## References

- For details of the construction of scaled SVDS see the [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## See also

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  SVD prior for non-time-varying terms

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Smoothing via random walk

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Smoothing via second-order random walk

- [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md)
  Smoothing via damped random walk

- [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md)
  Smoothing via damped second-order random walk

- [`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)
  Smoothing via splines

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [`set_var_sexgender()`](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md)
  Identify sex or gender variable in data

## Examples

``` r
SVD_AR1(HMD)
#>   SVD_AR1(HMD) 
#>       ssvd: HMD
#>     n_comp: 3
#>        min: 0.8
#>        max: 0.98
#>          s: 1
#>      along: NULL
#>        con: none
SVD_RW(HMD, n_comp = 3)
#>   SVD_RW(HMD) 
#>       ssvd: HMD
#>     n_comp: 3
#>          s: 1
#>         sd: 1
#>      along: NULL
#>        con: none
SVD_RW2(HMD, indep = FALSE)
#>   SVD_RW2(HMD,indep=FALSE) 
#>       ssvd: HMD
#>     n_comp: 3
#>      indep: FALSE
#>          s: 1
#>         sd: 1
#>   sd_slope: 1
#>      along: NULL
#>        con: none
```
