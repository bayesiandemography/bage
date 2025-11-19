# Extract Values for Hyper-Parameters

Extract values for hyper-parameters from a model object.
Hyper-parameters include

- main effects and interactions,

- dispersion,

- trends, seasonal effects, errors,

- SVD, spline, and covariate coefficients,

- standard deviations, correlation coefficients.

## Usage

``` r
# S3 method for class 'bage_mod'
components(object, quiet = FALSE, original_scale = FALSE, ...)
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

  Whether values for `"effect"`, `"trend"`, `"season"`, `"error"` and
  `"disp"` components from a
  [normal](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  model are on the original scale or the transformed scale. Default is
  `FALSE`.

- ...:

  Unused. Included for generic consistency only.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with four columns columns:

The return value contains the following columns:

- `term` Model term that the hyper-parameter belongs to.

- `component` Component within term.

- `level` Element within component .

- `.fitted` An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html)
  containing draws from the posterior distribution.

## Fitted vs unfitted models

[`components()`](https://generics.r-lib.org/reference/components.html)
is typically called on a
[fitted](https://generics.r-lib.org/reference/fit.html) model. In this
case, the values returned are draws from the joint posterior
distribution for the hyper-parameters in the model.

[`components()`](https://generics.r-lib.org/reference/components.html)
can, however, be called on an unfitted model. In this case, the values
returned are draws from the joint *prior* distribution. In other words,
the values incorporate model priors, and any `exposure`, `size`, or
`weights` argument, but not observed outcomes.

## Scaling and Normal models

Internally, models created with
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
are fitted using transformed versions of the outcome and weights
variables. By default, when
[`components()`](https://generics.r-lib.org/reference/components.html)
is used with these models, it returns values for `.fitted` that are
based on the transformed versions. To instead obtain values for
`"effect"`, `"trend"`, `"season"`, `"error"` and `"disp"` that are based
on the untransformed versions, set `original_scale` to `TRUE`.

## See also

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, means, or probabilities, together with
  original data

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Extract values for dispersion

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) Extract a
  one-line summary of a model

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

## Examples

``` r
set.seed(0)

## specify model
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)

## extract prior distribution
## of hyper-parameters
mod |>
  components()
#> ℹ Model not fitted, so values drawn straight from prior distribution.
#> # A tibble: 37 × 4
#>    term        component level                   .fitted
#>    <chr>       <chr>     <chr>              <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept)  -0.027 (-1.9, 1.8)
#>  2 age         effect    0-4            -0.018 (-2, 1.9)
#>  3 age         effect    5-9           0.033 (-2.7, 3.1)
#>  4 age         effect    10-14        -0.063 (-3.4, 3.7)
#>  5 age         effect    15-19           0.019 (-4, 4.4)
#>  6 age         effect    20-24         0.099 (-4.5, 4.9)
#>  7 age         effect    25-29         0.085 (-4.4, 5.5)
#>  8 age         effect    30-34       -0.0021 (-5.3, 5.7)
#>  9 age         effect    35-39         0.025 (-5.6, 6.6)
#> 10 age         effect    40-44        -0.033 (-6.2, 6.4)
#> # ℹ 27 more rows

## fit model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## extract posterior distribution
## of hyper-parameters
mod |>
  components()
#> # A tibble: 37 × 4
#>    term        component level                   .fitted
#>    <chr>       <chr>     <chr>              <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept)  -2.5 (-4.2, -0.76)
#>  2 age         effect    0-4          -2.4 (-3.9, -0.75)
#>  3 age         effect    5-9           -3.8 (-5.3, -2.2)
#>  4 age         effect    10-14         -3.3 (-4.9, -1.7)
#>  5 age         effect    15-19        -1.5 (-3.1, 0.062)
#>  6 age         effect    20-24            -1.4 (-3, 0.2)
#>  7 age         effect    25-29          -1.5 (-3, 0.069)
#>  8 age         effect    30-34        -1.6 (-3.1, 0.032)
#>  9 age         effect    35-39       -1.6 (-3.2, -0.075)
#> 10 age         effect    40-44       -1.6 (-3.2, -0.032)
#> # ℹ 27 more rows

## fit normal model
mod <- mod_norm(value ~ age * diag + year,
                data = nld_expenditure,
                weights = 1) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## dispersion (= standard deviation in normal model)
## on the transformed scale
mod |>
  components() |>
  subset(component == "disp")
#> ℹ Values for `.fitted` from `components()` are on a transformed scale. See the documentation for `mod_norm()` and `components()` for details.
#> # A tibble: 1 × 4
#>   term  component level           .fitted
#>   <chr> <chr>     <chr>      <rdbl<1000>>
#> 1 disp  disp      disp  0.32 (0.31, 0.34)

## disperson on the original scale
mod |>
  components(original_scale = TRUE) |>
  subset(component == "disp")
#> # A tibble: 1 × 4
#>   term  component level        .fitted
#>   <chr> <chr>     <chr>   <rdbl<1000>>
#> 1 disp  disp      disp  163 (156, 170)
```
