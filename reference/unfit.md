# Unfit a Model

Reset a model, deleting all estimates.

## Usage

``` r
unfit(mod)
```

## Arguments

- mod:

  A fitted object of class `"bage_mod"`, object, created through a call
  to
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

## Value

An unfitted version of `mod`.

## See also

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model

- [`set_seeds()`](https://bayesiandemography.github.io/bage/reference/set_seeds.md)
  Reset random seeds

- Functions such as
  [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md),
  [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  and
  [`set_var_age()`](https://bayesiandemography.github.io/bage/reference/set_var_age.md)
  unfit models as side effects.

## Examples

``` r
## create a model, which starts out unfitted
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)
is_fitted(mod)
#> [1] FALSE

## calling 'fit' produces a fitted version
mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
is_fitted(mod)
#> [1] TRUE

## calling 'unfit' resets the model
mod <- unfit(mod)
is_fitted(mod)
#> [1] FALSE
```
