# Printing a Model

After calling a function such as
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
or
[`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
it is good practice to print the model object at the console, to check
the model's structure. The output from
[`print()`](https://rdrr.io/r/base/print.html) has the following
components:

- A header giving the class of the model and noting whether the model
  has been fitted.

- A [formula](https://rdrr.io/r/stats/formula.html) giving the outcome
  variable and terms for the model.

- A table giving the number of parameters, and (fitted models only) the
  standard deviation across those parameters, a measure of the term's
  importance. See
  [`priors()`](https://bayesiandemography.github.io/bage/reference/priors.md)
  and [`tidy()`](https://generics.r-lib.org/reference/tidy.html).

- Values for other model settings. See
  [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md),
  [`set_var_age()`](https://bayesiandemography.github.io/bage/reference/set_var_age.md),
  [`set_var_sexgender()`](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md),
  [`set_var_time()`](https://bayesiandemography.github.io/bage/reference/set_var_time.md),
  [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md)

- Details on computations (fitted models only). See
  [`computations()`](https://bayesiandemography.github.io/bage/reference/computations.md).

## Usage

``` r
# S3 method for class 'bage_mod'
print(x, ...)
```

## Arguments

- x:

  Object of class `"bage_mod"`, typically created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- ...:

  Unused. Included for generic consistency only.

## Value

`x`, invisibly.

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [fit.bage_mod()](https://generics.r-lib.org/reference/fit.html) and
  [`is_fitted()`](https://bayesiandemography.github.io/bage/reference/is_fitted.md)
  Model fitting

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, probabilities, or means, together with
  original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Extract values for dispersion

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors for model terms

- [tidy.bage_mod()](https://generics.r-lib.org/reference/tidy.html)
  Number of parameters, and standard deviations

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Dispersion

- [`set_var_age()`](https://bayesiandemography.github.io/bage/reference/set_var_age.md),
  [`set_var_sexgender()`](https://bayesiandemography.github.io/bage/reference/set_var_sexgender.md),
  [`set_var_time()`](https://bayesiandemography.github.io/bage/reference/set_var_time.md)
  Age, sex/gender and time variables

- [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md)
  Model draws

## Examples

``` r
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)

## print unfitted model
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

mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

## print fitted model
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
#>        0.50     0.21      0.25   12      TRUE   relative convergence (4)
#> 
```
