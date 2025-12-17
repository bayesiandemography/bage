# Create Replicate Data

Use a fitted model to create replicate datasets, typically as a way of
checking a model.

## Usage

``` r
replicate_data(x, condition_on = NULL, n = 19)
```

## Arguments

- x:

  A fitted model, typically created by calling
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md),
  and then [`fit()`](https://generics.r-lib.org/reference/fit.html).

- condition_on:

  Parameters to condition on. Either `"expected"` or `"fitted"`. See
  details.

- n:

  Number of replicate datasets to create. Default is 19.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the following structure:

|                   |                                                                                                                                                                                                                                                                               |
|-------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.replicate`      | data                                                                                                                                                                                                                                                                          |
| `"Original"`      | Original data supplied to [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md), [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md), [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md) |
| `"Replicate 1"`   | Simulated data.                                                                                                                                                                                                                                                               |
| `"Replicate 2"`   | Simulated data.                                                                                                                                                                                                                                                               |
| ...               | ...                                                                                                                                                                                                                                                                           |
| `"Replicate <n>"` | Simulated data.                                                                                                                                                                                                                                                               |

## Details

Use `n` draws from the posterior distribution for model parameters to
generate `n` simulated datasets. If the model is working well, these
simulated datasets should look similar to the actual dataset.

## The `condition_on` argument

With Poisson and binomial models that include dispersion terms (which is
the default), there are two options for constructing replicate data.

- When `condition_on` is `"fitted"`, the replicate data is created
  by (i) drawing values from the posterior distribution for rates or
  probabilities (the \\\gamma_i\\ defined in
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  and
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)),
  and (ii) conditional on these rates or probabilities, drawing values
  for the outcome variable.

- When `condition_on` is `"expected"`, the replicate data is created
  by (i) drawing values from hyper-parameters governing the rates or
  probabilities (the \\\mu_i\\ and \\\xi\\ defined in
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  and
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)),
  then (ii) conditional on these hyper-parameters, drawing values for
  the rates or probabilities, and finally (iii) conditional on these
  rates or probabilities, drawing values for the outcome variable. The
  \`"expected" option is only possible in Poisson and binomial models,
  and only when dispersion is non-zero.

The default for `condition_on` is `"expected"`, in cases where it is
feasible. The `"expected"` option provides a more severe test for a
model than the `"fitted"` option, since "fitted" values are weighted
averages of the "expected" values and the original data.

## Data models for outcomes

If a [data
model](https://bayesiandemography.github.io/bage/reference/datamods.md)
has been provided for the outcome variable, then creation of replicate
data will include a step where errors are added to outcomes. For
instance, the a
[rr3](https://bayesiandemography.github.io/bage/reference/set_datamod_outcome_rr3.md)
data model is used, then `replicate_data()` rounds the outcomes to base
3.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit model.

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, probabilities, or means, together with
  original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Extract values for dispersion

- [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  Forecast, based on a model

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Simulation study of model.

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = 1) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

rep_data <- mod |>
  replicate_data()

library(dplyr)
rep_data |>
  group_by(.replicate) |>
  count(wt = injuries)
#> # A tibble: 20 × 2
#> # Groups:   .replicate [20]
#>    .replicate       n
#>    <fct>        <dbl>
#>  1 Original     21588
#>  2 Replicate 1  21088
#>  3 Replicate 2  20918
#>  4 Replicate 3  21963
#>  5 Replicate 4  20803
#>  6 Replicate 5  20738
#>  7 Replicate 6  21066
#>  8 Replicate 7  21711
#>  9 Replicate 8  20784
#> 10 Replicate 9  20277
#> 11 Replicate 10 20600
#> 12 Replicate 11 21293
#> 13 Replicate 12 21792
#> 14 Replicate 13 21481
#> 15 Replicate 14 21390
#> 16 Replicate 15 20504
#> 17 Replicate 16 21521
#> 18 Replicate 17 21354
#> 19 Replicate 18 21881
#> 20 Replicate 19 22475

## when the overall model includes an rr3 data model,
## replicate data are rounded to base 3
mod_pois(injuries ~ age:sex + ethnicity + year,
         data = nzl_injuries,
         exposure = popn) |>
  set_datamod_outcome_rr3() |>
  fit() |>
  replicate_data()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> # A tibble: 18,240 × 7
#>    .replicate age   sex    ethnicity  year injuries  popn
#>    <fct>      <fct> <chr>  <chr>     <int>    <dbl> <int>
#>  1 Original   0-4   Female Maori      2000       12 35830
#>  2 Original   5-9   Female Maori      2000        6 35120
#>  3 Original   10-14 Female Maori      2000        3 32830
#>  4 Original   15-19 Female Maori      2000        6 27130
#>  5 Original   20-24 Female Maori      2000        6 24380
#>  6 Original   25-29 Female Maori      2000        6 24160
#>  7 Original   30-34 Female Maori      2000       12 22560
#>  8 Original   35-39 Female Maori      2000        3 22230
#>  9 Original   40-44 Female Maori      2000        6 18130
#> 10 Original   45-49 Female Maori      2000        6 13770
#> # ℹ 18,230 more rows
```
