# Simulation Study of a Model

Use simulated data to assess the performance of an estimation model.

## Usage

``` r
report_sim(
  mod_est,
  mod_sim = NULL,
  method = c("standard", "inner-outer"),
  vars_inner = NULL,
  n_sim = 100,
  point_est_fun = c("median", "mean"),
  widths = c(0.5, 0.95),
  report_type = c("short", "long", "full"),
  n_core = 1
)
```

## Arguments

- mod_est:

  The model whose performance is being assessed. An object of class
  `bage_mod`.

- mod_sim:

  The model used to generate the simulated data. If no value is
  supplied, `mod_est` is used.

- method:

  Estimation method used for `mod_est`. See
  [`fit()`](https://generics.r-lib.org/reference/fit.html).

- vars_inner:

  Variables used in inner model with `"inner-outer"`estimation method.
  See [`fit()`](https://generics.r-lib.org/reference/fit.html).

- n_sim:

  Number of sets of simulated data to use. Default is 100.

- point_est_fun:

  Name of the function to use to calculate point estimates. The options
  are `"mean"` and `"median"`. The default is `"mean"`.

- widths:

  Widths of credible intervals. A vector of values in the interval
  `(0, 1]`. Default is `c(0.5, 0.95)`.

- report_type:

  Amount of detail in return value. Options are `"short"` and `"long"`.
  Default is `"short"`.

- n_core:

  Number of cores to use for parallel processing. If `n_core` is `1`
  (the default), no parallel processing is done.

## Value

A named list with a tibble called `"components"` and a tibble called
`"augment"`.

## Warning

The interface for `report_sim()` is still under development and may
change in future.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify binomial model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify normal model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify non-default prior for term

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify non-default prior for dispersion

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  Generate replicate data for a model

## Examples

``` r
## results random, so set seed
set.seed(0)

## make data - outcome variable (deaths here)
## needs to be present, but is not used
data <- data.frame(region = c("A", "B", "C", "D", "E"),
                   population = c(100, 200, 300, 400, 500),
                   deaths = NA)

## simulation with estimation model same as
## data-generating model
mod_est <- mod_pois(deaths ~ region,
                    data = data,
                    exposure = population) |>
  set_prior(`(Intercept)` ~ Known(0))
report_sim(mod_est = mod_est,
           n_sim = 10) ## in practice should use larger value
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> $components
#> # A tibble: 4 × 7
#>   term        component     .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>       <chr>          <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 (Intercept) effect     0               1        1         0           0   
#> 2 region      effect     0.0448          0.6      0.92      0.828       2.40
#> 3 region      hyper     -0.0000343       0.5      0.8       0.506       1.73
#> 4 disp        disp      -0.317           0.4      0.9       0.630       2.64
#> 
#> $augment
#> # A tibble: 2 × 7
#>   .var      .observed  .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>         <dbl>   <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 .fitted        3.62  0.0102      0.44      0.94      0.116      0.342
#> 2 .expected      3.62 -0.683       0.6       0.92      2.45      10.5  
#> 

## simulation with estimation model different
## from data-generating model
mod_sim <- mod_est |>
  set_prior(region ~ N(s = 2))
report_sim(mod_est = mod_est,
           mod_sim = mod_sim,
           n_sim = 10)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
#> $components
#> # A tibble: 4 × 7
#>   term        component .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>       <chr>      <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 (Intercept) effect     0           1        1         0           0   
#> 2 region      effect     0.205       0.5      0.92      1.03        2.93
#> 3 region      hyper     -0.502       0.3      0.8       0.595       1.97
#> 4 disp        disp      -0.119       0.4      0.7       0.915       4.35
#> 
#> $augment
#> # A tibble: 2 × 7
#>   .var      .observed  .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>         <dbl>   <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 .fitted        129.  -0.187      0.44      0.96      0.339      0.984
#> 2 .expected      129. -25.2        0.5       0.92    105.       556.   
#> 
```
