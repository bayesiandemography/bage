# Specify Time Variable

Specify which variable (if any) represents time. Functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
try to infer the time variable from variable names, but do not always
get it right.

## Usage

``` r
set_var_time(mod, name)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- name:

  The name of the time variable.

## Value

A `bage_mod` object

## Details

In an R [`formula`](https://rdrr.io/r/stats/formula.html), a 'variable'
is different from a 'term'. For instance,

`~ time + region + time:region`

contains variables `time` and `region`, and terms `time`, `region`, and
`time:region`.

By default, **bage** gives a term involving time a
([`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md))
prior. Changing the time variable via `set_var_time()` can change
priors: see below for an example.

If `set_var_time()` is applied to a fitted model, `set_var_time()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [`set_var_age()`](https://bayesiandemography.github.io/bage/reference/set_var_age.md)
  Set age variable

- [`set_var_sexgender()`](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md)
  Sex sex or gender variable

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Test if model has been fitted

- internally, **bage** uses
  [`poputils::find_var_time()`](https://bayesiandemography.github.io/poputils/reference/find_var_time.html)
  to locate time variables

## Examples

``` r
## rename time variable to something unusual
injuries2 <- nzl_injuries
injuries2$calendar_year <- injuries2$year

## mod_pois does not recognize time variable
mod <- mod_pois(injuries ~ age * ethnicity + calendar_year,
                data = injuries2,
                exposure = popn)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * ethnicity + calendar_year
#> 
#>                  exposure: popn
#> 
#>           term  prior along n_par n_par_free
#>    (Intercept) NFix()     -     1          1
#>            age   RW()   age    12         12
#>      ethnicity NFix()     -     2          2
#>  calendar_year    N()     -    19         19
#>  age:ethnicity   RW()   age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_age
#>    1000     age
#> 

## so we set the time variable explicitly
## (which, as a side effect, changes the prior on
## the time main effect)
mod |>
  set_var_time(name = "calendar_year")
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * ethnicity + calendar_year
#> 
#>                  exposure: popn
#> 
#>           term  prior         along n_par n_par_free
#>    (Intercept) NFix()             -     1          1
#>            age   RW()           age    12         12
#>      ethnicity NFix()             -     2          2
#>  calendar_year   RW() calendar_year    19         19
#>  age:ethnicity   RW()           age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw      var_time var_age
#>    1000 calendar_year     age
#> 
```
