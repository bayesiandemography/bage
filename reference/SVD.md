# SVD-Based Prior for Age or Age-Sex Profiles

Use components from a Singular Value Decomposition (SVD) to model a main
effect or interaction involving age.

## Usage

``` r
SVD(ssvd, v = NULL, n_comp = 3, indep = TRUE)
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
  is `3`.

- indep:

  Whether to use separate or combined SVDs in terms involving sex or
  gender. Default is `TRUE`. See below for details.

## Value

An object of class `"bage_prior_svd"`.

## Details

A `SVD()` prior assumes that the age, age-sex, or age-gender profiles
for the quantity being modelled looks like they were drawn at random
from an external demographic database. For instance, the prior obtained
via

    SVD(HMD)

assumes that age or age-sex profiles look like they were drawn from the
[Human Mortality Database](https://www.mortality.org).

If `SVD()` is used with an interaction involving variables other than
age and sex/gender, separate profiles are constructed within each
combination of other variables.

bage chooses the appropriate age-specific or age-sex-specific SVD values
internally. The choice depends on the model term that the `SVD()` prior
is applied to, and on the age labels used in `data` argument to
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
or
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).
bage makes its choice when
[`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
is called.

## Joint or independent SVDs

Two possible ways of extracting patterns from age-sex-specific data are

1.  carry out separate SVDs on separate datasets for each sex/gender; or

2.  carry out a single SVD on dataset that has separate entries for each
    sex/gender.

Option 1 is more flexible. Option 2 is more robust to sampling or
measurement errors. Option 1 is obtained by setting the `joint` argument
to `FALSE`. Option 1 is obtained by setting the `indep` argument to
`TRUE`. The default is `TRUE`.

## Mathematical details

**Case 1: Term involving age and no other variables**

When `SVD()` is used with an age main effect,

\$\$\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},\$\$

where

- \\\pmb{\beta}\\ is a main effect or interaction involving age;

- \\J\\ is the number of elements of \\\pmb{\beta}\\;

- \\n\\ is the number of components from the SVD;

- \\\pmb{F}\\ is a known matrix with dimension \\J \times n\\; and

- \\\pmb{g}\\ is a known vector with \\J\\ elements.

\\\pmb{F}\\ and \\\pmb{g}\\ are constructed from a large database of
age-specific demographic estimates by performing an SVD and
standardizing.

The elements of \\\pmb{\alpha}\\ have prior \$\$\alpha_k \sim
\text{N}(0, 1), \quad k = 1, \cdots, K.\$\$

**Case 2: Term involving age and non-sex, non-gender variable(s)**

When `SVD()` is used with an interaction that involves age but that does
not involve sex or gender,

\$\$\pmb{\beta}\_u = \pmb{F} \pmb{\alpha}\_u + \pmb{g},\$\$

where

- \\\pmb{\beta}\_u\\ is a subvector of \\\pmb{\beta}\\ holding values
  for the \\u\\th combination of the non-age variables;

- \\V\\ is the number of elements of \\\pmb{\beta}\_u\\;

- \\n\\ is the number of components from the SVD;

- \\\pmb{F}\\ is a known matrix with dimension \\V \times n\\; and

- \\\pmb{g}\\ is a known vector with \\V\\ elements.

**Case 3: Term involving age, sex/gender, and no other variables**

When `SVD()` is used with an interaction that involves age and sex or
gender, there are two sub-cases, depending on the value of `indep`.

When `indep` is `TRUE`,

\$\$\pmb{\beta}\_{s} = \pmb{F}\_s \pmb{\alpha}\_{s} + \pmb{g}\_s,\$\$

and when `indep` is `FALSE`,

\$\$\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},\$\$

where

- \\\pmb{\beta}\\ is an interaction involving age and sex/gender;

- \\\pmb{\beta}\_{s}\\ is a subvector of \\\pmb{\beta}\\, holding values
  for sex/gender \\s\\;

- \\J\\ is the number of elements in \\\pmb{\beta}\\;

- \\S\\ is the number of sexes/genders;

- \\n\\ is the number of components from the SVD;

- \\\pmb{F}\_s\\ is a known \\(J/S) \times n\\ matrix, specific to
  sex/gender \\s\\;

- \\\pmb{g}\_s\\ is a known vector with \\J/S\\ elements, specific to
  sex/gender \\s\\;

- \\\pmb{F}\\ is a known \\J \times n\\ matrix, with values for all
  sexes/genders; and

- \\\pmb{g}\\ is a known vector with \\J\\ elements, with values for all
  sexes/genders.

The elements of \\\pmb{\alpha}\_s\\ and \\\pmb{\alpha}\\ have prior
\$\$\alpha_k \sim \text{N}(0, 1).\$\$

**Case 4: Term involving age, sex/gender, and other variable(s)**

When `SVD()` is used with an interaction that involves age, sex or
gender, and other variables, there are two sub-cases, depending on the
value of `indep`.

When `indep` is `TRUE`,

\$\$\pmb{\beta}\_{u,s} = \pmb{F}\_s \pmb{\alpha}\_{u,s} +
\pmb{g}\_s,\$\$

and when `indep` is `FALSE`,

\$\$\pmb{\beta}\_u = \pmb{F} \pmb{\alpha}\_u + \pmb{g},\$\$

where

- \\\pmb{\beta}\\ is an interaction involving sex/gender;

- \\\pmb{\beta}\_{u,s}\\ is a subvector of \\\pmb{\beta}\\, holding
  values for sex/gender \\s\\ for the \\u\\th combination of the other
  variables;

- \\V\\ is the number of elements in \\\pmb{\beta}\_u\\;

- \\S\\ is the number of sexes/genders;

- \\n\\ is the number of components from the SVD;

- \\\pmb{F}\_s\\ is a known \\(V/S) \times n\\ matrix, specific to
  sex/gender \\s\\;

- \\\pmb{g}\_s\\ is a known vector with \\V/S\\ elements, specific to
  sex/gender \\s\\;

- \\\pmb{F}\\ is a known \\V \times n\\ matrix, with values for all
  sexes/genders; and

- \\\pmb{g}\\ is a known vector with \\V\\ elements, with values for all
  sexes/genders.

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

- [`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
  SVD priors for for time-varying age profiles;

- [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  Smoothing via random walk

- [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  Smoothing via second-order random walk

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
SVD(HMD) 
#>   SVD(HMD) 
#>       ssvd: HMD
#>     n_comp: 3
SVD(HMD, n_comp = 2)
#>   SVD(HMD,n_comp=2) 
#>       ssvd: HMD
#>     n_comp: 2
```
