
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bage

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/bage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/bage/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/bage/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/bage?branch=main)
<!-- badges: end -->

Fast Bayesian estimation and forecasting of age-specific rates.

## Installation

``` r
devtools::install_github("bayesiandemography/bage")
```

## Example

Fit Poisson model to data on injuries.

``` r
library(bage)
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = injuries,
                exposure = popn) |>
  fit()
mod
#> -- Fitted Poisson model --
#> 
#>    injuries ~ age:sex + ethnicity + year
#> (Intercept) ~ N(scale=10)
#>   ethnicity ~ N()
#>        year ~ RW()
#>     age:sex ~ N()
#> 
#>   exposure: popn
#>    var_age: age
#>   var_time: year
#>     n_draw: 1000
```

Extract model-based and direct estimates.

``` r
augment(mod)
#> # A tibble: 912 × 10
#>    age   sex    ethnicity  year injuries  popn   .fitted  .lower  .upper .obse…¹
#>    <fct> <chr>  <chr>     <int>    <int> <int>     <dbl>   <dbl>   <dbl>   <dbl>
#>  1 0-4   Female Maori      2000       12 35830 0.000244  2.18e-4 2.73e-4 3.35e-4
#>  2 5-9   Female Maori      2000        6 35120 0.0000679 5.64e-5 8.20e-5 1.71e-4
#>  3 10-14 Female Maori      2000        3 32830 0.0000919 7.80e-5 1.09e-4 9.14e-5
#>  4 15-19 Female Maori      2000        6 27130 0.000433  3.96e-4 4.72e-4 2.21e-4
#>  5 20-24 Female Maori      2000        6 24380 0.000422  3.83e-4 4.63e-4 2.46e-4
#>  6 25-29 Female Maori      2000        6 24160 0.000348  3.16e-4 3.85e-4 2.48e-4
#>  7 30-34 Female Maori      2000       12 22560 0.000322  2.91e-4 3.56e-4 5.32e-4
#>  8 35-39 Female Maori      2000        3 22230 0.000350  3.15e-4 3.86e-4 1.35e-4
#>  9 40-44 Female Maori      2000        6 18130 0.000345  3.15e-4 3.78e-4 3.31e-4
#> 10 45-49 Female Maori      2000        6 13770 0.000365  3.31e-4 4.03e-4 4.36e-4
#> # … with 902 more rows, and abbreviated variable name ¹​.observed
```
