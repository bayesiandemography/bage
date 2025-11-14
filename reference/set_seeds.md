# Reset Random Seeds in Model Object

Reset random seeds stored in a model object. When `new_seeds` is `NULL`
(the default), the new seeds are generated randomly; otherwise they are
taken from `new_seeds`.

## Usage

``` r
set_seeds(mod, new_seeds = NULL)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- new_seeds:

  `NULL` (the default) or a list of integers with names
  `"seed_components"` `"seed_augment"`, `"seed_forecast_components"`,
  and `"seed_forecast_augment"`.

## Value

A revised version of `mod`.

## Details

When an object of class `"bage_mod"` is first created, values are
generated four four random seeds:

- `seed_components`

- `seed_augment`

- `seed_forecast_components`

- `seed_forecast_augment`

When [`fit()`](https://generics.r-lib.org/reference/fit.html),
[`components()`](https://generics.r-lib.org/reference/components.html),
[`augment()`](https://generics.r-lib.org/reference/augment.html), and
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) are
called on the model object, the seeds are used internally to ensure that
he same inputs generate the same outputs, even when the outputs involve
random draws.

End users are unlikely to call `set_seeds()` in a data analysis, though
it may occasionally by useful when building a simulation from scratch.

## See also

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Do a simulation study.
  ([`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  calls `set_seeds()` internally.)

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`unfit()`](https://bayesiandemography.github.io/bage/reference/unfit.md)
  Reset model, deleting estimates

## Examples

``` r
## fit model
mod <- mod_pois(injuries ~ age,
                data = nzl_injuries,
                exposure = popn) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## call 'components()'
components(mod)
#> # A tibble: 15 × 4
#>    term        component level                 .fitted
#>    <chr>       <chr>     <chr>            <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept) -4.2 (-5.7, -2.9)
#>  2 age         effect    0-4         -4.2 (-5.5, -2.8)
#>  3 age         effect    5-9         -5.6 (-6.9, -4.1)
#>  4 age         effect    10-14       -5.1 (-6.4, -3.6)
#>  5 age         effect    15-19       -3.4 (-4.7, -1.9)
#>  6 age         effect    20-24       -3.2 (-4.5, -1.7)
#>  7 age         effect    25-29       -3.3 (-4.6, -1.8)
#>  8 age         effect    30-34       -3.3 (-4.6, -1.8)
#>  9 age         effect    35-39       -3.4 (-4.7, -1.9)
#> 10 age         effect    40-44       -3.4 (-4.7, -1.9)
#> 11 age         effect    45-49         -3.5 (-4.8, -2)
#> 12 age         effect    50-54         -3.5 (-4.8, -2)
#> 13 age         effect    55-59         -3.5 (-4.8, -2)
#> 14 age         hyper     sd            0.7 (0.47, 1.1)
#> 15 disp        disp      disp         0.4 (0.36, 0.45)

## call 'components()' again - same results
components(mod)
#> # A tibble: 15 × 4
#>    term        component level                 .fitted
#>    <chr>       <chr>     <chr>            <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept) -4.2 (-5.7, -2.9)
#>  2 age         effect    0-4         -4.2 (-5.5, -2.8)
#>  3 age         effect    5-9         -5.6 (-6.9, -4.1)
#>  4 age         effect    10-14       -5.1 (-6.4, -3.6)
#>  5 age         effect    15-19       -3.4 (-4.7, -1.9)
#>  6 age         effect    20-24       -3.2 (-4.5, -1.7)
#>  7 age         effect    25-29       -3.3 (-4.6, -1.8)
#>  8 age         effect    30-34       -3.3 (-4.6, -1.8)
#>  9 age         effect    35-39       -3.4 (-4.7, -1.9)
#> 10 age         effect    40-44       -3.4 (-4.7, -1.9)
#> 11 age         effect    45-49         -3.5 (-4.8, -2)
#> 12 age         effect    50-54         -3.5 (-4.8, -2)
#> 13 age         effect    55-59         -3.5 (-4.8, -2)
#> 14 age         hyper     sd            0.7 (0.47, 1.1)
#> 15 disp        disp      disp         0.4 (0.36, 0.45)

## reset seeds
mod <- set_seeds(mod)

## calling 'set_seeds' unfits the model
is_fitted(mod)
#> [1] FALSE

## so we fit it again
mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## when we call components, we get
## different results from earlier
components(mod)
#> # A tibble: 15 × 4
#>    term        component level                 .fitted
#>    <chr>       <chr>     <chr>            <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept) -4.2 (-5.6, -2.9)
#>  2 age         effect    0-4         -4.3 (-5.6, -2.9)
#>  3 age         effect    5-9           -5.6 (-7, -4.3)
#>  4 age         effect    10-14       -5.1 (-6.5, -3.8)
#>  5 age         effect    15-19         -3.4 (-4.7, -2)
#>  6 age         effect    20-24       -3.2 (-4.6, -1.9)
#>  7 age         effect    25-29         -3.3 (-4.7, -2)
#>  8 age         effect    30-34         -3.3 (-4.7, -2)
#>  9 age         effect    35-39       -3.4 (-4.8, -2.1)
#> 10 age         effect    40-44       -3.4 (-4.8, -2.1)
#> 11 age         effect    45-49       -3.5 (-4.9, -2.2)
#> 12 age         effect    50-54       -3.5 (-4.9, -2.1)
#> 13 age         effect    55-59       -3.5 (-4.9, -2.2)
#> 14 age         hyper     sd           0.71 (0.45, 1.1)
#> 15 disp        disp      disp         0.4 (0.36, 0.45)
```
