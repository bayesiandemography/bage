
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bage <a href="https://github.com/bayesiandemography/bage"><img src="data-raw/sticker/sticker.png" align="right" height="138" /></a>

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
#> 
#> (Intercept) ~ NFix()
#>   ethnicity ~ NFix()
#>        year ~ RW()
#>     age:sex ~ RW()
#> 
#>      dispersion: mean=1
#>        exposure: popn
#>         var_age: age
#>   var_sexgender: sex
#>        var_time: year
#>          n_draw: 1000
```

Extract model-based and direct estimates.

``` r
augment(mod)
#> # A tibble: 912 × 9
#>    age   sex    ethnicity  year injuries  popn .observed
#>    <fct> <chr>  <chr>     <int>    <int> <int>     <dbl>
#>  1 0-4   Female Maori      2000       12 35830 0.000335 
#>  2 5-9   Female Maori      2000        6 35120 0.000171 
#>  3 10-14 Female Maori      2000        3 32830 0.0000914
#>  4 15-19 Female Maori      2000        6 27130 0.000221 
#>  5 20-24 Female Maori      2000        6 24380 0.000246 
#>  6 25-29 Female Maori      2000        6 24160 0.000248 
#>  7 30-34 Female Maori      2000       12 22560 0.000532 
#>  8 35-39 Female Maori      2000        3 22230 0.000135 
#>  9 40-44 Female Maori      2000        6 18130 0.000331 
#> 10 45-49 Female Maori      2000        6 13770 0.000436 
#> # ℹ 902 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>
```
