# Specify RR3 Data Model

\#' \`r lifecycle::badge('deprecated')

## Usage

``` r
set_datamod_outcome_rr3(mod)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

## Value

A revised version of `mod`.

## Details

This function has been deprecated, and will be removed from future
versions of `bage`. Please used function
[`set_confidential_rr3()`](https://bayesiandemography.github.io/bage/reference/set_confidential_rr3.md)
instead.

## Examples

``` r
## 'injuries' variable in 'nzl_injuries' dataset
## has been randomly rounded to base 3
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn) |>
  set_confidential_rr3() |> ## rather than set_datamod_outcome_rr3
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
```
