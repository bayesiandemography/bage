# Specify Number of Draws from Prior or Posterior Distribution

Specify the number of draws from the posterior distribution to be used
in model output. A newly-created `bage_mod` object has an `n_draw` value
of 1000. Higher values may be appropriate for characterizing the tails
of distributions, or for publication-quality graphics and summaries.

## Usage

``` r
set_n_draw(mod, n_draw = 1000L)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- n_draw:

  Number of draws.

## Value

A `bage_mod` object

## Details

If the new value for `n_draw` is greater than the old value, and the
model has already been fitted, then the model is
[unfitted](https://bayesiandemography.github.io/bage/reference/unfit.md),
and function [`fit()`](https://generics.r-lib.org/reference/fit.html)
may need to be called again.

## See also

- [`n_draw.bage_mod()`](https://bayesiandemography.github.io/bage/reference/n_draw.bage_mod.md)
  query the value of `n_draw`

- [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`components()`](https://generics.r-lib.org/reference/components.html)
  functions for drawing from prior or posterior distribution - the
  output of which is affected by the value of `n_draw`

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for a term

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify prior for dispersion

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`unfit()`](https://bayesiandemography.github.io/bage/reference/unfit.md)
  Reset a model

## Examples

``` r
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn)
mod # value for 'n_draw' displayed in object
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age:sex + ethnicity + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>    ethnicity NFix()     -     2          2
#>         year   RW()  year    19         19
#>      age:sex   RW()   age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     year     age           sex
#> 
n_draw(mod) # or use 'n_draw()' to query
#> [1] 1000

mod <- mod |>
  set_n_draw(n_draw = 5000)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age:sex + ethnicity + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>    ethnicity NFix()     -     2          2
#>         year   RW()  year    19         19
#>      age:sex   RW()   age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    5000     year     age           sex
#> 
```
