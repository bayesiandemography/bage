# Extract Values for Dispersion

Extract values for the 'dispersion' parameter from a model object.

## Usage

``` r
dispersion(object, quiet = FALSE, original_scale = FALSE)
```

## Arguments

- object:

  Object of class `"bage_mod"`, typically created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- quiet:

  Whether to suppress messages. Default is `FALSE`.

- original_scale:

  Whether values for disperson are on the original scale or the
  transformed scale. Default is `FALSE`.

## Value

An [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html)
(or `NULL` if the model does not include a dispersion parameter.)

## Fitted vs unfitted models

`dispersion()` is typically called on a
[fitted](https://generics.r-lib.org/reference/fit.html) model. In this
case, the values for dispersion are draws from the posterior
distribution. `dispersion()` can, however, be called on an unfitted
model. In this case, the values are drawn from the prior distribution.

## Scaling and Normal models

Internally, models created with
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
are fitted using transformed versions of the outcome and weights
variables. By default, when `dispersion()` is used with these models, it
returns values on the transformed scale. To instead obtain values on the
untransformed scale, set `original_scale` to `TRUE`.

## See also

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters, including dispersion

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify a prior for dispersion

## Examples

``` r
set.seed(0)

## specify model
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)

## prior distribution
mod |>
  dispersion()
#> <rvec_dbl<1000>[1]>
#> [1] 0.62 (0.028, 3.9)

## fit model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## posterior distribution
mod |>
  dispersion()
#> <rvec_dbl<1000>[1]>
#> [1] 0.15 (0.13, 0.17)

## fit normal model
mod <- mod_norm(value ~ age * diag + year,
                data = nld_expenditure,
                weights = 1) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## values on the transformed scale
mod |>
  dispersion()
#> ℹ Values for dispersion are on a transformed scale. See the documentation for `mod_norm()` and `dispersion()` for details.
#> <rvec_dbl<1000>[1]>
#> [1] 0.32 (0.31, 0.34)

## values on the original scale
mod |>
  dispersion(original_scale = TRUE)
#> <rvec_dbl<1000>[1]>
#> [1] 163 (156, 170)
```
