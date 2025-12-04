# Extract Data and Modeled Values

Extract data and rates, probabilities, or means from a model object. The
return value consists of the original data and one or more columns of
modeled values.

## Usage

``` r
# S3 method for class 'bage_mod'
augment(x, quiet = FALSE, ...)
```

## Arguments

- x:

  Object of class `"bage_mod"`, typically created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- quiet:

  Whether to suppress messages. Default is `FALSE`.

- ...:

  Unused. Included for generic consistency only.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html),
with the original data plus one or more of the following columns:

- `.<outcome>` Corrected or extended version of the outcome variable, in
  applications where the outcome variable has missing values, or a data
  model is being used.

- `.observed` 'Direct' estimates of rates or probabilities, ie counts
  divided by exposure or size (in Poisson and binomial models.)

- `.fitted` Draws of rates, probabilities, or means.

- `.expected` Draws of expected values for rates or probabilities (in
  Poisson that include exposure, or in binomial models.)

Uncertain quantities are represented using
[rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.html).

## Fitted vs unfitted models

[`augment()`](https://generics.r-lib.org/reference/augment.html) is
typically called on a
[fitted](https://generics.r-lib.org/reference/fit.html) model. In this
case, the modeled values are draws from the joint posterior distribution
for rates, probabilities, or means.

[`augment()`](https://generics.r-lib.org/reference/augment.html) can,
however, be called on an unfitted model. In this case, the modeled
values are draws from the joint prior distribution. In other words, the
modeled values are informed by model priors, and by values for
`exposure`, `size`, or `weights`, but not by observed outcomes.

## Imputed values for outcome variable

[`augment()`](https://generics.r-lib.org/reference/augment.html)
automatically imputes any missing values for the outcome variable. If
outcome variable `var` has one or more `NA`s, then `augment` creates a
variable `.var` holding original and imputed values.

## Data model for outcome variable

If the overall model includes a data model for the outcome variable
`var`, then
[`augment()`](https://generics.r-lib.org/reference/augment.html) creates
a new variable `.var` containing estimates of the true value for the
outcome.

## See also

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Extract values for dispersion

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) Short
  summary of a model

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  See if a model has been fitted

- [`unfit()`](https://bayesiandemography.github.io/bage/reference/unfit.md)
  Reset a model

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  Overview of data models implemented in **bage**

## Examples

``` r
set.seed(0)

## specify model
mod <- mod_pois(divorces ~ age + sex + time,
                data = nzl_divorces,
                exposure = population) |>
  set_n_draw(n_draw = 100) ## smaller sample, so 'augment' faster

## fit model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## draw from the posterior distribution
mod |>
  augment()
#> # A tibble: 242 × 8
#>    age   sex     time divorces population .observed                    .fitted
#>    <fct> <chr>  <int>    <dbl>      <dbl>     <dbl>                <rdbl<100>>
#>  1 15-19 Female  2011        0     154460 0         1.1e-05 (5.7e-06, 1.8e-05)
#>  2 15-19 Female  2012        6     153060 0.0000392 1.4e-05 (7.5e-06, 2.1e-05)
#>  3 15-19 Female  2013        3     152250 0.0000197   1.2e-05 (7.1e-06, 2e-05)
#>  4 15-19 Female  2014        3     152020 0.0000197 1.1e-05 (7.2e-06, 1.9e-05)
#>  5 15-19 Female  2015        3     152970 0.0000196 1.1e-05 (6.2e-06, 1.8e-05)
#>  6 15-19 Female  2016        3     154170 0.0000195   1e-05 (6.7e-06, 1.8e-05)
#>  7 15-19 Female  2017        6     154450 0.0000388 1.2e-05 (6.8e-06, 1.9e-05)
#>  8 15-19 Female  2018        0     154170 0         8.4e-06 (5.1e-06, 1.3e-05)
#>  9 15-19 Female  2019        3     154760 0.0000194   1e-05 (6.2e-06, 1.6e-05)
#> 10 15-19 Female  2020        0     154480 0         8.4e-06 (4.4e-06, 1.3e-05)
#> # ℹ 232 more rows
#> # ℹ 1 more variable: .expected <rdbl<100>>

## insert a missing value into outcome variable
divorces_missing <- nzl_divorces
divorces_missing$divorces[1] <- NA

## fitting model and calling 'augument'
## creates a new variable called '.divorces'
## holding observed and imputed values
mod_pois(divorces ~ age + sex + time,
         data = divorces_missing,
         exposure = population) |>
  fit() |>
  augment()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> ℹ Adding variable `.divorces` with true values for `divorces`.
#> # A tibble: 242 × 9
#>    age   sex     time divorces    .divorces population  .observed
#>    <fct> <chr>  <int>    <dbl> <rdbl<1000>>      <dbl>      <dbl>
#>  1 15-19 Female  2011       NA     2 (0, 6)     154460 NA        
#>  2 15-19 Female  2012        6     6 (6, 6)     153060  0.0000392
#>  3 15-19 Female  2013        3     3 (3, 3)     152250  0.0000197
#>  4 15-19 Female  2014        3     3 (3, 3)     152020  0.0000197
#>  5 15-19 Female  2015        3     3 (3, 3)     152970  0.0000196
#>  6 15-19 Female  2016        3     3 (3, 3)     154170  0.0000195
#>  7 15-19 Female  2017        6     6 (6, 6)     154450  0.0000388
#>  8 15-19 Female  2018        0     0 (0, 0)     154170  0        
#>  9 15-19 Female  2019        3     3 (3, 3)     154760  0.0000194
#> 10 15-19 Female  2020        0     0 (0, 0)     154480  0        
#> # ℹ 232 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## specifying a data model for the
## original data also leads to a new
## variable called '.divorces'
mod_pois(divorces ~ age + sex + time,
         data = nzl_divorces,
         exposure = population) |>
  set_datamod_outcome_rr3() |>
  fit() |>
  augment()
#> Warning: `set_datamod_outcome_rr3()` was deprecated in bage 0.9.4.
#> ℹ Please use `set_confidential_rr3()` instead.
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> ℹ Adding variable `.divorces` with true values for `divorces`.
#> # A tibble: 242 × 9
#>    age   sex     time divorces    .divorces population .observed
#>    <fct> <chr>  <int>    <dbl> <rdbl<1000>>      <dbl>     <dbl>
#>  1 15-19 Female  2011        0     1 (0, 2)     154460 0        
#>  2 15-19 Female  2012        6     5 (4, 7)     153060 0.0000392
#>  3 15-19 Female  2013        3     2 (1, 5)     152250 0.0000197
#>  4 15-19 Female  2014        3     3 (1, 4)     152020 0.0000197
#>  5 15-19 Female  2015        3     3 (1, 5)     152970 0.0000196
#>  6 15-19 Female  2016        3     2 (1, 4)     154170 0.0000195
#>  7 15-19 Female  2017        6     5 (4, 7)     154450 0.0000388
#>  8 15-19 Female  2018        0     1 (0, 2)     154170 0        
#>  9 15-19 Female  2019        3     2 (1, 5)     154760 0.0000194
#> 10 15-19 Female  2020        0     1 (0, 2)     154480 0        
#> # ℹ 232 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>
```
