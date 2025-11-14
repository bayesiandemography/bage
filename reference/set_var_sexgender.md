# Specify Sex or Gender Variable

Specify which variable (if any) represents sex or gender. Functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
try to infer the sex/gender variable from variable names, but do not
always get it right.

## Usage

``` r
set_var_sexgender(mod, name)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- name:

  The name of the sex or gender variable.

## Value

A `"bage_mod"` object

## Details

In an R [`formula`](https://rdrr.io/r/stats/formula.html), a 'variable'
is different from a 'term'. For instance,

`~ gender + region + gender:region`

contains variables `gender` and `region`, and terms `gender`, `region`,
and `gender:region`.

If `set_var_sexgender()` is applied to a fitted model,
`set_var_sexgender()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [`set_var_age()`](https://bayesiandemography.github.io/bage/reference/set_var_age.md)
  Set age variable

- [`set_var_time()`](https://bayesiandemography.github.io/bage/reference/set_var_time.md)
  Set time variable

- [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Test whether model is fitted

- internally, **bage** uses
  [`poputils::find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.html)
  to locate sex or gender variables

- internally, **bage** uses
  [`poputils::find_label_female()`](https://bayesiandemography.github.io/poputils/reference/find_label_female.html)
  to locate female categories within a sex or gender variable

- internally, **bage** uses
  [`poputils::find_label_male()`](https://bayesiandemography.github.io/poputils/reference/find_label_male.html)
  to locate male categories within a sex or gender variable

## Examples

``` r
## rename 'sex' variable to something unexpected
injuries2 <- nzl_injuries
injuries2$biological_sex <- injuries2$sex

## mod_pois does not recognize sex variable
mod <- mod_pois(injuries ~ age * biological_sex + year,
                data = injuries2,
                exposure = popn)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * biological_sex + year
#> 
#>                  exposure: popn
#> 
#>                term  prior along n_par n_par_free
#>         (Intercept) NFix()     -     1          1
#>                 age   RW()   age    12         12
#>      biological_sex NFix()     -     2          2
#>                year   RW()  year    19         19
#>  age:biological_sex   RW()   age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age
#>    1000     year     age
#> 

## so we set the sex variable explicitly
mod |>
  set_var_sexgender(name = "biological_sex")
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    injuries ~ age * biological_sex + year
#> 
#>                  exposure: popn
#> 
#>                term  prior along n_par n_par_free
#>         (Intercept) NFix()     -     1          1
#>                 age   RW()   age    12         12
#>      biological_sex NFix()     -     2          2
#>                year   RW()  year    19         19
#>  age:biological_sex   RW()   age    24         24
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age  var_sexgender
#>    1000     year     age biological_sex
#> 
```
