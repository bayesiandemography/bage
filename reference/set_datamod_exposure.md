# Specify Exposure Data Model

Specify a data model for the exposure variable in a Poisson model. The
data model assumes that, within each cell, observed exposure is drawn
from an Inverse-Gamma distribution. In this model,

E\[ expected exposure \| true exposure \] = true exposure

and

sd\[ expected exposure \| true exposure \] = `cv` \\\times\\ true
exposure

where `cv` is a coefficient of variation parameter.

## Usage

``` r
set_datamod_exposure(mod, cv)
```

## Arguments

- mod:

  An object of class `"bage_mod_pois"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

- cv:

  Coefficient of variation for measurement errors in exposure. A single
  number, or a data frame with a variable called `"cv"` and one or more
  'by' variables.

## Value

A revised version of `mod`.

## Details

In the exposure data model, `cv`, the coefficient of variation, does not
depend on true exposure. This implies that errors do not fall, in
relative terms, as population rises. Unlike sampling errors, measurement
errors do not get averaged away in large populations.

The exposure data model assumes that the exposure variable is unbiased.
If there is in fact evidence of biases, then this evidence should be
used to create a de-biased version of the variable (eg one where
estimated biases have been subtracted) to supply to
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

`set_datamod_exposure()` can only be used with a Poisson model for rates
in which the dispersion in the rates has been set to zero. The
dispersion in the rates can be set explicitly to zero using
[`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md),
though `set_datamod_exposure()` will also do so.

## The `cv` argument

`cv` can be a single number, in which case the same value is used for
all cells. `cv` can also be a data frame with a with a variable called
`"cv"` and one or more columns with 'by' variables. For instance, a `cv`
of

    data.frame(sex = c("Female", "Male"),
               cv = c(0.01, 0.012))

implies that the coefficient of variation is 0.01 for females and 0.012
for males.

See below for an example where the coefficient of variation is based on
aggregated age groups.

## Mathematical details

The model for observed exposure is

\$\$w_i^{\text{obs}} \sim \text{InvGamma}(2 + d\_{g \lbrack i \rbrack
}^{-1}, (1 + d\_{g \lbrack i\rbrack }^{-1}) w_i^{\text{true}})\$\$

where

- \\w_i^{\text{obs}}\\ is observed exposure for cell \\i\\ (the
  `exposure` argument to
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md));

- \\w_i^{\text{true}}\\ is true exposure for cell \\i\\; and

- \\d\_{g\lbrack i\rbrack }\\ is the value for dispersion that is
  applied to cell \\i\\.

`cv` is \\\sqrt{d_g}\\.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify dispersion of rates

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Original data plus estimated values, including estimates of true value
  for exposure

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  Data models implemented in `bage`

- [confidential](https://bayesiandemography.github.io/bage/reference/confidential.md)
  Confidentialization procedures modeled in `bage`

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## specify model
mod <- mod_pois(injuries ~ age * sex + year,
                data = nzl_injuries,
                exposure = popn) |>
  set_disp(mean = 0) |>
  set_datamod_exposure(cv = 0.025)

## fit the model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    injuries ~ age * sex + year
#> 
#>                  exposure: popn
#>                data model: exposure
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    12         12    0.71
#>          sex NFix()     -     2          2    0.11
#>         year   RW()  year    19         19    0.09
#>      age:sex   RW()   age    24         24    0.43
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.47     0.21      0.22   12      TRUE   relative convergence (4)
#> 

## examine results - note the new variable
## '.popn' with estimates of the true
## population
aug <- mod |>
  augment()
#> ℹ Adding variable `.popn` with true values for `popn`.

## allow different cv's for each sex
cv_sex <- data.frame(sex = c("Female", "Male"),
                     cv = c(0.03, 0.02))
mod <- mod |>
  set_datamod_exposure(cv = cv_sex)
#> → Replacing existing "exposure" data model with new "exposure" data model
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * sex + year
#> 
#>                  exposure: popn
#>                data model: exposure
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age    12         12
#>          sex NFix()     -     2          2
#>         year   RW()  year    19         19
#>      age:sex   RW()   age    24         24
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 

## our outcome variable is confidentialized,
## so we recognize that in the model too
mod <- mod |>
  set_confidential_rr3()
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * sex + year
#> 
#>                  exposure: popn
#>                data model: exposure
#>       confidentialization: rr3
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age    12         12
#>          sex NFix()     -     2          2
#>         year   RW()  year    19         19
#>      age:sex   RW()   age    24         24
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 

## now a model where everyone aged 0-49
## receives one value for cv, and
## everyone aged 50+ receives another
library(poputils) ## for 'age_upper()'
library(dplyr, warn.conflicts = FALSE)
nzl_injuries_age <- nzl_injuries |>
  mutate(age_group = if_else(age_upper(age) < 50,
                             "0-49",
                             "50+"))
cv_age <- data.frame(age_group = c("0-49", "50+"),
                     cv = c(0.05, 0.01))
mod <- mod_pois(injuries ~ age * sex + year,
                data = nzl_injuries_age,
                exposure = popn) |>
  set_disp(mean = 0) |>
  set_datamod_exposure(cv = cv_age)
```
