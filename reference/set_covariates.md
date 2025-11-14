# Specify Covariates

Add covariates to a model.

## Usage

``` r
set_covariates(mod, formula)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- formula:

  A one-sided R [formula](https://rdrr.io/r/stats/formula.html),
  specifying the covariates.

## Value

A modified version of `mod`

## Details

If `set_covariates()` is applied to a model that already has covariates,
`set_covariates()` deletes the existing covariates.

If `set_covariates()` is applied to a fitted model, `set_covariates()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## Covariate data

All variables contained in the `formula` argument to `set_covariates()`
should be in the dataset supplied in the original call to
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
or
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

`set_covariates()` processes the covariate data before adding it to the
model:

- All numeric variables are standardized, using `x <- scale(x)`.

- Categorical variables are converted to sets of indicator variables,
  using treatment contrasts. For instance, variable `x` with categories
  `"high"`, `"medium"`, and `"low"`, is converted into two indicator
  variables, one called `xmedium` and one called `xlow`.

## Mathematical details

When a model includes covariates, the quantity

\$\$\pmb{Z} \pmb{\zeta}\$\$

is added to the linear predictor, where \\\pmb{Z}\\ is a matrix of
standardized covariates, and \\\pmb{\zeta}\\ is a vector of
coefficients. The elements of \\\pmb{\zeta}\\ have prior

\$\$\zeta_p \sim \text{N}(0, 1)\$\$.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model for rates, probabilities, or means

## Examples

``` r
## create a COVID covariate
library(dplyr, warn.conflicts = FALSE)
births <- kor_births |>
  mutate(is_covid = time %in% 2020:2022)
mod <- mod_pois(births ~ age * region + time,
                data = births,
                exposure = popn) |>
  set_covariates(~ is_covid)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    births ~ age * region + time
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age     9          9
#>       region    N()     -    16         16
#>         time   RW()  time    13         13
#>   age:region   RW()   age   144        144
#> 
#>  covariates: ~is_covid
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age
#>    1000     time     age
#> 
```
