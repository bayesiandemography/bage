# Specify Prior for Dispersion or Standard Deviation

Specify the mean of prior for the dispersion parameter (in Poisson and
binomial models) or the standard deviation parameter (in normal models.)

## Usage

``` r
set_disp(mod, mean = 1)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- mean:

  Mean value for the exponential prior. In Poisson and binomial models,
  can be set to 0. Default is `1`.

## Value

A `bage_mod` object

## Details

The dispersion or mean parameter has an exponential distribution with
mean \\\mu\\,

\$\$p(\xi) = \frac{1}{\mu}\exp\left(\frac{-\xi}{\mu}\right).\$\$

By default \\\mu\\ equals 1.

In Poisson and binomial models, `mean` can be set to `0`, implying that
the dispersion term is also `0`. In normal models, `mean` must be
non-negative.

If `set_disp()` is applied to a fitted model, `set_disp()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model for rates, probabilities, or means

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for a term

- [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md)
  Specify the number of draws

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Test whether a model is fitted

## Examples

``` r
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn)
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
#>    1000     year     age           sex
#> 
mod |> set_disp(mean = 0.1)
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
#>  disp: mean = 0.1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     year     age           sex
#> 
mod |> set_disp(mean = 0)
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
#>  n_draw var_time var_age var_sexgender
#>    1000     year     age           sex
#> 
```
