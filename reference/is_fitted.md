# Test Whether a Model has Been Fitted

Test whether
[fit()](https://bayesiandemography.github.io/bage/reference/fit.bage_mod.md)
has been called on a model object.

## Usage

``` r
is_fitted(x)
```

## Arguments

- x:

  An object of class `"bage_mod"`.

## Value

`TRUE` or `FALSE`

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  to specify a model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) to fit a
  model

## Examples

``` r
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)
is_fitted(mod)
#> [1] FALSE
mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
is_fitted(mod)
#> [1] TRUE
```
