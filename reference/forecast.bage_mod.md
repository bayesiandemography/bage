# Use a Model to Make a Forecast

Forecast rates, probabilities, means, and other model parameters.

## Usage

``` r
# S3 method for class 'bage_mod'
forecast(
  object,
  newdata = NULL,
  labels = NULL,
  output = c("augment", "components"),
  include_estimates = FALSE,
  quiet = FALSE,
  ...
)
```

## Arguments

- object:

  A `bage_mod` object, typically created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- newdata:

  Data frame with data for future periods.

- labels:

  Labels for future values.

- output:

  Type of output returned

- include_estimates:

  Whether to include historical estimates along with the forecasts.
  Default is `FALSE`.

- quiet:

  Whether to suppress messages. Default is `FALSE`.

- ...:

  Not currently used.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html).

## How the forecasts are constructed

Internally, the steps involved in a forecast are:

1.  Forecast time-varying main effects and interactions, e.g. a time
    main effect, or an age-time interaction.

2.  Combine forecasts for the time-varying main effects and interactions
    with non-time-varying parameters, e.g. age effects or dispersion.

3.  Use the combined parameters to generate values for rates,
    probabilities or means.

4.  Optionally, generate values for the outcome variable.

[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
generates values for the outcome variable when,

- `output` is `"augment"`,

- a value has been supplied for `newdata`,

- `newdata` included a value for the exposure, size, or weights variable
  (except if `exposure = 1` or `weights = 1` in the original call to
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)).

[Mathematical
Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
gives more details on the internal calculations in forecasting.

## Output format

When `output` is `"augment"` (the default), the return value from
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) looks
like output from function
[`augment()`](https://generics.r-lib.org/reference/augment.html). When
`output` is `"components"`, the return value looks like output from
[`components()`](https://generics.r-lib.org/reference/components.html).

When `include_estimates` is `FALSE` (the default), the output of
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
excludes values for time-varying parameters for the period covered by
the data. When `include_estimates` is `TRUE`, the output includes these
values. Setting `include_estimates` to `TRUE` can be helpful when
creating graphs that combine estimates and forecasts.

## Forecasting with covariates

Models that contain
[covariates](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
can be used in forecasts, provided that

- all coefficients (the \\\zeta_p\\) are estimated from historical data
  via [`fit()`](https://generics.r-lib.org/reference/fit.html), and

- if any covariates (the columns of \\\pmb{Z}\\) are time-varying, then
  future values for these covariates are supplied via the `newdata`
  argument.

## Forecasting with data models

Models that contain [data
models](https://bayesiandemography.github.io/bage/reference/datamods.md)
can be used in forecasts, provided that

- the data models have no time-varying parameters, or

- future values for time-varying parameters are supplied when the data
  model is first specified.

For examples, see the [Data
Models](https://bayesiandemography.github.io/bage/articles/vig10_datamod.html)
vignette.

## Fitted and unfitted models

[`forecast()`](https://generics.r-lib.org/reference/forecast.html) is
typically used with a
[fitted](https://generics.r-lib.org/reference/fit.html) model, i.e. a
model in which parameter values have been estimated from the data. The
resulting forecasts reflect data and priors.

[`forecast()`](https://generics.r-lib.org/reference/forecast.html) can,
however, be used with an unfitted model. In this case, the forecasts are
based entirely on the priors. See below for an example. Experimenting
with forecasts based entirely on the priors can be helpful for choosing
an appropriate model.

## Warning

The interface for
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) has
not been finalised.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, probabilities, or means, together with
  original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## specify and fit model
mod <- mod_pois(injuries ~ age * sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    injuries ~ age * sex + ethnicity + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    12         12    0.73
#>          sex NFix()     -     2          2    0.11
#>    ethnicity NFix()     -     2          2    0.45
#>         year   RW()  year    19         19    0.09
#>      age:sex   RW()   age    24         24    0.43
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.52     0.24      0.24   15      TRUE   relative convergence (4)
#> 

## forecasts
mod |>
  forecast(labels = 2019:2024)
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> # A tibble: 288 × 9
#>    age   sex   ethnicity  year injuries  popn .observed                  .fitted
#>    <fct> <chr> <chr>     <int>    <dbl> <int>     <dbl>             <rdbl<1000>>
#>  1 0-4   Fema… Maori      2019       NA    NA        NA 2e-04 (0.00014, 0.00026)
#>  2 0-4   Fema… Maori      2020       NA    NA        NA 2e-04 (0.00014, 0.00027)
#>  3 0-4   Fema… Maori      2021       NA    NA        NA 2e-04 (0.00013, 0.00027)
#>  4 0-4   Fema… Maori      2022       NA    NA        NA 2e-04 (0.00014, 0.00028)
#>  5 0-4   Fema… Maori      2023       NA    NA        NA 2e-04 (0.00013, 0.00027)
#>  6 0-4   Fema… Maori      2024       NA    NA        NA 2e-04 (0.00013, 0.00028)
#>  7 0-4   Fema… Non Maori  2019       NA    NA        NA 1e-04 (7.5e-05, 0.00014)
#>  8 0-4   Fema… Non Maori  2020       NA    NA        NA 1e-04 (7.3e-05, 0.00014)
#>  9 0-4   Fema… Non Maori  2021       NA    NA        NA 1e-04 (7.5e-05, 0.00015)
#> 10 0-4   Fema… Non Maori  2022       NA    NA        NA 1e-04 (7.1e-05, 0.00015)
#> # ℹ 278 more rows
#> # ℹ 1 more variable: .expected <rdbl<1000>>

## combined estimates and forecasts
mod |>
  forecast(labels = 2019:2024,
           include_estimates = TRUE)
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> `augment()` for past values...
#> # A tibble: 1,200 × 9
#>    age   sex    ethnicity  year injuries  popn .observed
#>    <fct> <chr>  <chr>     <int>    <dbl> <int>     <dbl>
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
#> # ℹ 1,190 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## hyper-parameters
mod |>
  forecast(labels = 2019:2024,
           output = "components")
#> `components()` for past values...
#> `components()` for future values...
#> # A tibble: 6 × 4
#>   term  component level            .fitted
#>   <chr> <chr>     <chr>       <rdbl<1000>>
#> 1 year  effect    2019   -2.1 (-3.8, -0.4)
#> 2 year  effect    2020   -2.1 (-3.8, -0.4)
#> 3 year  effect    2021   -2.1 (-3.8, -0.4)
#> 4 year  effect    2022  -2.1 (-3.8, -0.42)
#> 5 year  effect    2023  -2.1 (-3.8, -0.41)
#> 6 year  effect    2024  -2.1 (-3.8, -0.41)

## hold back some data and forecast
library(dplyr, warn.conflicts = FALSE)
data_historical <- nzl_injuries |>
  filter(year <= 2015)
data_forecast <- nzl_injuries |>
  filter(year > 2015) |>
  mutate(injuries = NA)
mod_pois(injuries ~ age * sex + ethnicity + year,
         data = data_historical,
         exposure = popn) |>
  fit() |>
  forecast(newdata = data_forecast)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> # A tibble: 144 × 9
#>    age   sex    ethnicity  year     injuries  popn .observed
#>    <fct> <chr>  <chr>     <int> <rdbl<1000>> <int>     <dbl>
#>  1 0-4   Female Maori      2016    8 (3, 15) 41220        NA
#>  2 5-9   Female Maori      2016     2 (0, 6) 43230        NA
#>  3 10-14 Female Maori      2016     2 (0, 6) 37640        NA
#>  4 15-19 Female Maori      2016   12 (5, 20) 36040        NA
#>  5 20-24 Female Maori      2016   10 (4, 18) 33760        NA
#>  6 25-29 Female Maori      2016    8 (3, 15) 30530        NA
#>  7 30-34 Female Maori      2016    6 (2, 12) 24480        NA
#>  8 35-39 Female Maori      2016    6 (1, 12) 23170        NA
#>  9 40-44 Female Maori      2016    6 (1, 12) 23940        NA
#> 10 45-49 Female Maori      2016    6 (2, 13) 23580        NA
#> # ℹ 134 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## forecast using GDP per capita in 2023 as a covariate
mod_births <- mod_pois(births ~ age * region + time,
                       data = kor_births,
                       exposure = popn) |>
  set_covariates(~ gdp_pc_2023) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod_births |>
  forecast(labels = 2024:2025)
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> # A tibble: 288 × 10
#>    age   region             time births  popn gdp_pc_2023 dens_2020 .observed
#>    <chr> <fct>             <int>  <dbl> <int>       <dbl> <chr>         <dbl>
#>  1 10-14 Busan              2024     NA    NA        25.7 NA               NA
#>  2 10-14 Busan              2025     NA    NA        25.7 NA               NA
#>  3 10-14 Chungcheongbuk-do  2024     NA    NA        40.3 NA               NA
#>  4 10-14 Chungcheongbuk-do  2025     NA    NA        40.3 NA               NA
#>  5 10-14 Chungcheongnam-do  2024     NA    NA        50.4 NA               NA
#>  6 10-14 Chungcheongnam-do  2025     NA    NA        50.4 NA               NA
#>  7 10-14 Daegu              2024     NA    NA        22.3 NA               NA
#>  8 10-14 Daegu              2025     NA    NA        22.3 NA               NA
#>  9 10-14 Daejeon            2024     NA    NA        27.6 NA               NA
#> 10 10-14 Daejeon            2025     NA    NA        27.6 NA               NA
#> # ℹ 278 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>
```
