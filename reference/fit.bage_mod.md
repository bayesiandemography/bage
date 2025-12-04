# Fit a Model

Derive the posterior distribution for a model.

## Usage

``` r
# S3 method for class 'bage_mod'
fit(
  object,
  method = c("standard", "inner-outer"),
  vars_inner = NULL,
  optimizer = c("multi", "nlminb", "BFGS", "CG"),
  quiet = TRUE,
  max_jitter = 1e-04,
  start_oldpar = FALSE,
  ...
)
```

## Arguments

- object:

  A `bage_mod` object, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- method:

  Estimation method. Current choices are `"standard"` (the default) and
  `"inner-outer"`. See below for details.

- vars_inner:

  Names of variables to use for inner model when `method` is
  `"inner-outer". If `NULL`(the default)`vars_inner\` is the
  [age](https://bayesiandemography.github.io/bage/reference/set_var_age.md),
  [sex/gender](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md),
  and
  [time](https://bayesiandemography.github.io/bage/reference/set_var_time.md)
  variables.

- optimizer:

  Which optimizer to use. Current choices are `"multi"`, `"nlminb"`,
  `"BFGS"`, and `"CG"`. Default is `"multi"`. See below for details.

- quiet:

  Whether to suppress messages from optimizer. Default is `TRUE`.

- max_jitter:

  Maximum quantity to add to diagonal of precision matrix, if Cholesky
  factorization is failing. Default is 0.0001.

- start_oldpar:

  Whether the optimizer should start at previous estimates. Used only
  when [`fit()`](https://generics.r-lib.org/reference/fit.html) is being
  called on a fitted model. Default is `FALSE`.

- ...:

  Not currently used.

## Value

A `bage_mod` object

## Estimation methods

When `method` is `"standard"` (the default), all parameters, other than
the lowest-level rates, probabilities, or means are jointly estimated
within TMB.

When `method` is `"inner-outer"`, estimation is carried out in multiple
steps, which, in large models, can sometimes reduce computation times.
In Step 1, a model only using the `inner` variables is fitted to the
data. In Step 2, a model only using the `outer` variables is fitted to
the data. In Step 3, values for dispersion are calculated. Parameter
estimates from steps 1, 2, and 3 are then combined.

## Optimizer

The choices for the `optimizer` argument are:

- `"multi"` Try `"nlminb"`, and if that fails, restart from the
  parameter values where `"nlminb"` stopped, using `"BFGS"`. The
  default.

- `"nlminb"` [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html)

- `"BFGS"` [`stats::optim()`](https://rdrr.io/r/stats/optim.html) using
  method `"BFGS"`.

- `"GC"` [`stats::optim()`](https://rdrr.io/r/stats/optim.html) using
  method `"CG"` (conjugate gradient).

## Cholesky factorization and `max_jitter`

Sampling from the posterior distribution requires performing a Cholesky
factorization of the precision matrix returned by TMB. This
factorization sometimes fails because of numerical problems. Adding a
small quantity to the diagonal of the precision matrix can alleviate
numerical problems, while potentially reducing accuracy. If the Cholesky
factorization initially fails, `bage` will try again with progressively
larger quantities added to the diagonal, up to the maximum set by
`max_jitter`. Increasing the value of `max_jitter` can help suppress
numerical problems. A safer strategy, however, is to simplify the model,
or to use more informative priors.

## Aggregation

Up to version 0.9.8 of `bage`,
[`fit()`](https://generics.r-lib.org/reference/fit.html) always
aggregated across cells with identical values of the predictor variables
in `formula` (ie the variables to the right of `~`) before fitting. For
instance, if a dataset contained deaths and population disaggregated by
age and sex, but the model formula was `deaths ~ age`, then
[`fit()`](https://generics.r-lib.org/reference/fit.html) would aggregate
deaths and population within each age category before fitting the model.
From version 0.9.9,
[`fit()`](https://generics.r-lib.org/reference/fit.html) only aggregates
across cells with identical values if no data model is used, and if the
model is Poisson with dispersion set to 0 or is normal. Note that this
change in behavior has no effect on most models, since most models
include all variables used to classify outcomes.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

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
  Simulation study of a model

- [`unfit()`](https://bayesiandemography.github.io/bage/reference/unfit.md)
  Reset a model

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Check if a model has been fitted

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## specify model
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)

## examine unfitted model
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age + sex + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age    12         12
#>          sex NFix()     -     2          2
#>         year   RW()  year    19         19
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     year     age           sex
#> 

## fit model
mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## examine fitted model
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    injuries ~ age + sex + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    12         12    0.76
#>          sex NFix()     -     2          2    0.71
#>         year   RW()  year    19         19    0.09
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     year     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.39     0.18      0.19   12      TRUE   relative convergence (4)
#> 

## extract rates
aug <- augment(mod)
aug
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

## extract hyper-parameters
comp <- components(mod)
comp
#> # A tibble: 37 × 4
#>    term        component level                   .fitted
#>    <chr>       <chr>     <chr>              <rdbl<1000>>
#>  1 (Intercept) effect    (Intercept)     -2.4 (-4, -0.7)
#>  2 age         effect    0-4          -2.4 (-4.2, -0.74)
#>  3 age         effect    5-9           -3.8 (-5.6, -2.1)
#>  4 age         effect    10-14         -3.4 (-5.1, -1.7)
#>  5 age         effect    15-19          -1.6 (-3.3, 0.1)
#>  6 age         effect    20-24         -1.5 (-3.3, 0.21)
#>  7 age         effect    25-29        -1.6 (-3.4, 0.089)
#>  8 age         effect    30-34        -1.7 (-3.4, 0.063)
#>  9 age         effect    35-39       -1.7 (-3.5, -0.023)
#> 10 age         effect    40-44       -1.7 (-3.5, -0.056)
#> # ℹ 27 more rows
```
