# Specify Prior for Model Term

Specify a prior distribution for an intercept, a main effect, or an
interaction.

## Usage

``` r
set_prior(mod, formula)
```

## Arguments

- mod:

  A `bage_mod` object, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- formula:

  A formula giving the term and a function for creating a prior.

## Value

A modified `bage_mod` object.

## Details

If `set_prior()` is applied to a fitted model, `set_prior()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Current choices for prior distributions

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Test whether a model is fitted

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify prior for dispersion

## Examples

``` r
mod <- mod_pois(injuries ~ age + year,
                data = nzl_injuries,
                exposure = popn)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age    12         12
#>         year   RW()  year    19         19
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age
#>    1000     year     age
#> 
mod |> set_prior(age ~ RW2())
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age + year
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age  RW2()   age    12         12
#>         year   RW()  year    19         19
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age
#>    1000     year     age
#> 
```
