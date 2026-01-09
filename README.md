
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://github.com/bayesiandemography/bage">
<img src="man/figures/sticker/sticker.png"
       style="float:right; height:138px;" /> </a>

# bage

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/bage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/bage/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/bage/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/bage?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/bage)](https://CRAN.R-project.org/package=bage)
<!-- badges: end -->

Fast Bayesian estimation and forecasting of age-specific rates.

Features:

- Incorporates substantive demographic knowledge via priors
- Supports full Bayesian workflow
- Allows for measurement errors and missing values

## Installation

``` r
install.packages("bage")
```

## Development

A road map for the package is
[here](https://github.com/bayesiandemography/bage/blob/main/.github/ROADMAP.md).

## Example

Fit Poisson model to data on injuries

``` r
library(bage)
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn) |>
  fit()
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    injuries ~ age:sex + ethnicity + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>    ethnicity NFix()     -     2          2    0.45
#>         year   RW()  year    19         19    0.09
#>      age:sex   RW()   age    24         24    0.88
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.34     0.10      0.09   13      TRUE   relative convergence (4)
```

Extract model-based and direct estimates

``` r
augment(mod)
#> # A tibble: 912 × 9
#>    age   sex    ethnicity  year injuries  popn .observed                    .fitted
#>    <fct> <chr>  <chr>     <int>    <int> <int>     <dbl>               <rdbl<1000>>
#>  1 0-4   Female Maori      2000       12 35830 0.000335    0.00025 (2e-04, 0.00033)
#>  2 5-9   Female Maori      2000        6 35120 0.000171  7.2e-05 (5.1e-05, 9.9e-05)
#>  3 10-14 Female Maori      2000        3 32830 0.0000914 9.3e-05 (6.5e-05, 0.00013)
#>  4 15-19 Female Maori      2000        6 27130 0.000221    0.00039 (0.00029, 5e-04)
#>  5 20-24 Female Maori      2000        6 24380 0.000246    0.00039 (0.00029, 5e-04)
#>  6 25-29 Female Maori      2000        6 24160 0.000248  0.00033 (0.00025, 0.00043)
#>  7 30-34 Female Maori      2000       12 22560 0.000532  0.00035 (0.00026, 0.00046)
#>  8 35-39 Female Maori      2000        3 22230 0.000135  0.00032 (0.00023, 0.00042)
#>  9 40-44 Female Maori      2000        6 18130 0.000331  0.00034 (0.00025, 0.00045)
#> 10 45-49 Female Maori      2000        6 13770 0.000436  0.00036 (0.00027, 0.00048)
#> # ℹ 902 more rows
#> # ℹ 1 more variable: .expected <rdbl<1000>>
```
