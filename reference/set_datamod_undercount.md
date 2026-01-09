# Specify Undercount Data Model

Specify a data model for the outcome in a Poisson or binomial model,
where the outcome is subject to undercount.

## Usage

``` r
set_datamod_undercount(mod, prob)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  or
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md).

- prob:

  The prior for the probability that a person or event in the target
  population will correctly enumerated. A data frame with a variable
  called `"mean"`, a variable called `"disp"`, and, optionally, one or
  more 'by' variables.

## Value

A revised version of `mod`.

## Details

The undercount data model assumes that reported values for the outcome
variable understate the true values, because the reported values miss
some people or events in the target population. In other words, the
probability that any given unit in the target population will be
included in the reported outcome is less than 1.

## The `prob` argument

The `prob` argument specifies a prior distribution for the probability
that a person or event in the target population is included in the
reported outcome. `prob` is a data frame with a variable called
`"mean"`, a variable called `"disp"`, and, optionally, one or more 'by'
variables. For instance, a `prob` of

    data.frame(sex = c("Female", "Male"),
               mean = c(0.95, 0.92),
               disp = c(0.02, 0.015))

implies that the expected value for the inclusion probability is 0.95
for females and 0.92 for males, with slightly more uncertainty for
females than for males.

## Mathematical details

The model for the observed outcome is

\$\$y_i^{\text{obs}} \sim \text{Binomial}(y_i^{\text{true}},
\pi\_{g\[i\]})\$\$ \$\$\pi_g \sim \text{Beta}(m_g^{(\pi)} / d_g^{(\pi)},
(1-m_g^{(\pi)}) / d_g^{(\pi)})\$\$

where

- \\y_i^{\text{obs}}\\ is the observed outcome for cell \\i\\;

- \\y_i^{\text{true}}\\ is the true outcome for cell \\i\\;

- \\\pi\_{g\[i\]}\\ is the probability that a member of the target
  population in cell \\i\\ is correctly enumerated in that cell;

- \\m_g\\ is the expected value for \\\pi_g\\ (specified via `prob`);
  and

- \\d_g\\ is disperson for \\\pi_g\\ (specified via `prob`).

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Original data plus estimated values, including estimates of true value
  for the outcome variable

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Estimated values for model parameters, including inclusion
  probabilities and overcount rates

- [`set_datamod_overcount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_overcount.md)
  An overcount-only data model

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
## specify 'prob'
prob <- data.frame(sex = c("Female", "Male"),
                   mean = c(0.95, 0.97),
                   disp = c(0.05, 0.05))

## specify model
mod <- mod_pois(divorces ~ age * sex + time,
                data = nzl_divorces,
                exposure = population) |>
  set_datamod_undercount(prob)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    divorces ~ age * sex + time
#> 
#>                  exposure: population
#>                data model: undercount
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
#>                data model: undercount
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
#>        0.33     0.17      0.13   17      TRUE   relative convergence (4)
#> 

## original data, plus imputed values for outcome
mod |>
  augment()
#> ℹ Adding variable `.divorces` with true values for `divorces`.
#> # A tibble: 242 × 9
#>    age   sex     time divorces    .divorces population .observed
#>    <fct> <chr>  <int>    <dbl> <rdbl<1000>>      <dbl>     <dbl>
#>  1 15-19 Female  2011        0     0 (0, 1)     154460 0        
#>  2 15-19 Female  2012        6     6 (6, 7)     153060 0.0000392
#>  3 15-19 Female  2013        3     3 (3, 4)     152250 0.0000197
#>  4 15-19 Female  2014        3     3 (3, 4)     152020 0.0000197
#>  5 15-19 Female  2015        3     3 (3, 4)     152970 0.0000196
#>  6 15-19 Female  2016        3     3 (3, 4)     154170 0.0000195
#>  7 15-19 Female  2017        6     6 (6, 7)     154450 0.0000388
#>  8 15-19 Female  2018        0     0 (0, 1)     154170 0        
#>  9 15-19 Female  2019        3     3 (3, 4)     154760 0.0000194
#> 10 15-19 Female  2020        0     0 (0, 1)     154480 0        
#> # ℹ 232 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## parameter estimates
library(dplyr)
mod |>
  components() |>
  filter(term == "datamod")
#> # A tibble: 2 × 4
#>   term    component level         .fitted
#>   <chr>   <chr>     <chr>    <rdbl<1000>>
#> 1 datamod prob      Female 0.96 (0.81, 1)
#> 2 datamod prob      Male   0.98 (0.86, 1)

## the data have in fact been confidentialized,
## so we account for that, in addition
## to accounting for undercoverage
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
#>                data model: undercount
#>       confidentialization: rr3
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    11         11    1.94
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
#>        0.72     0.38      0.30   19      TRUE   relative convergence (4)
#> 
```
