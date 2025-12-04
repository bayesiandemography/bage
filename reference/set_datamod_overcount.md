# Specify Overcount Data Model

Specify a data model for the outcome in a Poisson model, where the
outcome is subject to overcount

## Usage

``` r
set_datamod_overcount(mod, rate)
```

## Arguments

- mod:

  An object of class `"bage_mod_pois"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

- rate:

  The prior for the overcoverage rate. A data frame with a variable
  called `"mean"`, a variable called `"disp"`, and, optionally, one or
  more 'by' variables.

## Value

A revised version of `mod`.

## Details

The overcount data model assumes that reported values for the outcome
overstate the actual values. The reported values might be affected by
double-counting, for instance, or might include some people or events
that are not in the target population.

## The `rate` argument

The `rate` argument specifies a prior distribution for the overcoverage
rate. `rate` is a data frame with a variable called `"mean"`, a variable
called `"disp"`, and, optionally, one or more 'by' variables. For
instance, a `rate` of

    data.frame(sex = c("Female", "Male"),
               mean = c(0.05, 0.03),
               disp = c(0.1, 0.15))

implies that the reported value for the outcome is expected to overstate
the true value by about 5% for females, and about 3% for females, with
greater unceratinty for males than females.

## Mathematical details

The model for the observed outcome is

\$\$y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i\$\$ \$\$\epsilon_i
\sim \text{Poisson}(\kappa\_{g\[i\]} \gamma_i w_i)\$\$ \$\$\kappa_g \sim
\text{Gamma}(1/d_g, 1/(d_g m_g))\$\$

where

- \\y_i^{\text{obs}}\\ is the observed outcome for cell \\i\\;

- \\y_i^{\text{true}}\\ is the true outcome for cell \\i\\;

- \\\epsilon_i\\ overcount in cell \\i\\;

- \\\gamma_i\\ is the rate for cell \\i\\;

- \\w_i\\ is exposure for cell \\i\\;

- \\\kappa\_{g\[i\]}\\ is the overcoverage rate for cell \\i\\;

- \\m_g\\ is the expected value for \\\kappa_g\\ (specified via `rate`);
  and

- \\d_g\\ is disperson for \\\kappa_g\\ (specified via `rate`).

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Original data plus estimated values, including estimates of true value
  for the outcome variable

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Estimated values for model parameters, including inclusion
  probabilities and overcount rates

- [`set_datamod_undercount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_undercount.md)
  An undercount-only data model

- [`set_datamod_miscount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_miscount.md)
  An undercount-and-overcount data model

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  All data models implemented in `bage`

- [confidential](https://bayesiandemography.github.io/bage/reference/confidential.md)
  Confidentialization procedures modeled in `bage`

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## specify 'rate'
rate <- data.frame(sex = c("Female", "Male"),
                   mean = c(0.1, 0.13),
                   disp = c(0.2, 0.2))

## specify model
mod <- mod_pois(divorces ~ age * sex + time,
                data = nzl_divorces,
                exposure = population) |>
  set_datamod_overcount(rate)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    divorces ~ age * sex + time
#> 
#>                  exposure: population
#>                data model: overcount
#> 
#>         term  prior along n_par n_par_free
#>  (Intercept) NFix()     -     1          1
#>          age   RW()   age    11         11
#>          sex NFix()     -     2          2
#>         time   RW()  time    11         11
#>      age:sex   RW()   age    22         22
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     time     age           sex
#> 

## fit model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    divorces ~ age * sex + time
#> 
#>                  exposure: population
#>                data model: overcount
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    11         11    2.00
#>          sex NFix()     -     2          2    0.36
#>         time   RW()  time    11         11    0.13
#>      age:sex   RW()   age    22         22    0.33
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     time     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.27     0.14      0.11   17      TRUE   relative convergence (4)
#> 

## original data, plus imputed values for outcome
mod |>
  augment()
#> ℹ Adding variable `.divorces` with true values for `divorces`.
#> # A tibble: 242 × 9
#>    age   sex     time divorces    .divorces population .observed
#>    <fct> <chr>  <int>    <dbl> <rdbl<1000>>      <dbl>     <dbl>
#>  1 15-19 Female  2011        0     0 (0, 0)     154460 0        
#>  2 15-19 Female  2012        6     6 (4, 6)     153060 0.0000392
#>  3 15-19 Female  2013        3     3 (2, 3)     152250 0.0000197
#>  4 15-19 Female  2014        3     3 (1, 3)     152020 0.0000197
#>  5 15-19 Female  2015        3     3 (1, 3)     152970 0.0000196
#>  6 15-19 Female  2016        3     3 (2, 3)     154170 0.0000195
#>  7 15-19 Female  2017        6     6 (4, 6)     154450 0.0000388
#>  8 15-19 Female  2018        0     0 (0, 0)     154170 0        
#>  9 15-19 Female  2019        3     3 (2, 3)     154760 0.0000194
#> 10 15-19 Female  2020        0     0 (0, 0)     154480 0        
#> # ℹ 232 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## parameter estimates
library(dplyr)
mod |>
  components() |>
  filter(term == "datamod")
#> # A tibble: 2 × 4
#>   term    component level              .fitted
#>   <chr>   <chr>     <chr>         <rdbl<1000>>
#> 1 datamod rate      Female 0.091 (0.033, 0.19)
#> 2 datamod rate      Male    0.11 (0.044, 0.25)

## the data have in fact been confidentialized,
## so we account for that, in addition
## to accounting for overcoverage
mod <- mod |>
 set_confidential_rr3() |>
 fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    divorces ~ age * sex + time
#> 
#>                  exposure: population
#>                data model: overcount
#>       confidentialization: rr3
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    11         11    1.93
#>          sex NFix()     -     2          2    0.32
#>         time   RW()  time    11         11    0.13
#>      age:sex   RW()   age    22         22    0.30
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender optimizer
#>    1000     time     age           sex    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.63     0.35      0.25   19      TRUE   relative convergence (4)
#> 
```
