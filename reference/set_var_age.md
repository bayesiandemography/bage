# Specify Age Variable

Specify which variable (if any) represents age. Functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
try to infer the age variable from variable names, but do not always get
it right.

## Usage

``` r
set_var_age(mod, name)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- name:

  The name of the age variable.

## Value

A `bage_mod` object

## Details

In an R [`formula`](https://rdrr.io/r/stats/formula.html), a 'variable'
is different from a 'term'. For instance,

`~ age + region + age:region`

contains variables `age` and `region`, and terms `age`, `region`, and
`age:region`.

By default, **bage** gives a term involving age a
([`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md))
prior. Changing the age variable via `set_var_age()` can change priors:
see below for an example.

If `set_var_age()` is applied to a fitted model, `set_var_age()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [`set_var_sexgender()`](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md)
  Set sex or gender variable

- [`set_var_time()`](https://bayesiandemography.github.io/bage/reference/set_var_time.md)
  Set time variable

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Test whether a model is fitted

- internally, **bage** uses
  [`poputils::find_var_age()`](https://bayesiandemography.github.io/poputils/reference/find_var_age.html)
  to locate age variables

## Examples

``` r
## rename 'age' variable to something unusual
injuries2 <- nzl_injuries
injuries2$age_last_birthday <- injuries2$age

## mod_pois does not recognize age variable
mod <- mod_pois(injuries ~ age_last_birthday * ethnicity + year,
                data = injuries2,
                exposure = popn)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age_last_birthday * ethnicity + year
#> 
#>                  exposure: popn
#> 
#>                         term  prior along n_par n_par_free
#>                  (Intercept) NFix()     -     1          1
#>            age_last_birthday    N()     -    12         12
#>                    ethnicity NFix()     -     2          2
#>                         year   RW()  year    19         19
#>  age_last_birthday:ethnicity    N()     -    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time
#>    1000     year
#> 

## so we set the age variable explicitly
## (which, as a side effect, changes the prior on
## the age main effect)
mod |>
  set_var_age(name = "age_last_birthday")
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age_last_birthday * ethnicity + year
#> 
#>                  exposure: popn
#> 
#>                         term  prior             along n_par n_par_free
#>                  (Intercept) NFix()                 -     1          1
#>            age_last_birthday   RW() age_last_birthday    12         12
#>                    ethnicity NFix()                 -     2          2
#>                         year   RW()              year    19         19
#>  age_last_birthday:ethnicity    N()                 -    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time           var_age
#>    1000     year age_last_birthday
#> 
```
