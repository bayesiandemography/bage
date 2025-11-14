# Information on Computations Performed During Model Fitting

Get information on computations performed by function
[`fit()`](https://generics.r-lib.org/reference/fit.html). The
information includes the total time used for fitting, and the time used
for two particular tasks that can be slow: running the optimizer
[`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html), and drawing
from the multivariate normal returned by the TMB. It also includes
values returned by the optimizer: the number of iterations needed, and
messages about convergence.

## Usage

``` r
computations(object)
```

## Arguments

- object:

  A fitted object of class `"bage_mod"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the following variables:

- `time_total` Seconds used for whole fitting process.

- `time_max` Seconds used for optimisiation.

- `time_draw` Seconds used by function
  [`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html).

- `iter` Number of iterations required for optimization.

- `message` Message about convergence returned by optimizer.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) Summarise a
  model

- [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md)
  Specify number of posterior draws

## Examples

``` r
mod <- mod_pois(divorces ~ age + sex + time,
                data = nzl_divorces,
                exposure = population) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

computations(mod)
#> # A tibble: 1 × 6
#>   time_total time_max time_draw  iter converged message                 
#>        <dbl>    <dbl>     <dbl> <int> <lgl>     <chr>                   
#> 1      0.208    0.103    0.0838    13 TRUE      relative convergence (4)
```
