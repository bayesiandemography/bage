# Specify Miscount Data Model

Specify a data model for the outcome in a Poisson model, where the
outcome is subject to undercount and overcount.

## Usage

``` r
set_datamod_miscount(mod, prob, rate)
```

## Arguments

- mod:

  An object of class `"bage_mod_pois"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

- prob:

  The prior for the probability that a person or event in the target
  population will correctly enumerated. A data frame with a variable
  called `"mean"`, a variable called `"disp"`, and, optionally, one or
  more 'by' variables.

- rate:

  The prior for the overcoverage rate. A data frame with a variable
  called `"mean"`, a variable called `"disp"`, and, optionally, one or
  more 'by' variables.

## Value

A revised version of `mod`.

## Details

The miscount data model is essentially a combination of the
[undercount](https://bayesiandemography.github.io/bage/reference/set_datamod_undercount.md)
and
[overcount](https://bayesiandemography.github.io/bage/reference/set_datamod_overcount.md)
data models. It assumes that reported outcome is the sum of two
quantities:

1.  *Units from target population, undercounted* People or events
    belonging to the target population, in which each unit's inclusion
    probability is less than 1.

2.  *Overcount* People or events that do not belong to target
    population, or that are counted more than once.

If, for instance, a census enumerates 91 people from a true population
of 100, but also mistakenly enumerates a further 6 people, then

- the true value for the outcome variable is 100

- the value for the undercounted target population is 91,

- the value for the overcount is 6, and

- the observed value for the outcome variable is 91 + 6 = 97.

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

## The `rate` argument

The `rate` argument specifies a prior distribution for the overcoverage
rate. `rate` is a data frame with a variable called `"mean"`, a variable
called `"disp"`, and, optionally, one or more 'by' variables. For
instance, a `rate` of

    data.frame(mean = 0.03, disp = 0.1)

implies that the expected value for the overcoverage rate is 0.03, with
a dispersion of 0.1. Since no 'by' variables are included, the same mean
and dispersion values are applied to all cells.

## Mathematical details

The model for the observed outcome is

\$\$y_i^{\text{obs}} = u_i + v_i\$\$ \$\$u_i \sim
\text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]})\$\$ \$\$v_i \sim
\text{Poisson}(\kappa\_{h\[i\]} \gamma_i w_i)\$\$ \$\$\pi_g \sim
\text{Beta}(m_g^{(\pi)} / d_g^{(\pi)}, (1-m_g^{(\pi)}) /
d_g^{(\pi)})\$\$ \$\$\kappa_h \sim \text{Gamma}(1/d_h^{(\kappa)},
1/(d_h^{(\kappa)} m_h^{(\kappa)}))\$\$

where

- \\y_i^{\text{obs}}\\ is the observed outcome for cell \\i\\;

- \\y_i^{\text{true}}\\ is the true outcome for cell \\i\\;

- \\\gamma_i\\ is the rate for cell \\i\\;

- \\w_i\\ is exposure for cell \\i\\;

- \\\pi\_{g\[i\]}\\ is the probability that a member of the target
  population in cell \\i\\ is correctly enumerated in that cell;

- \\\kappa\_{h\[i\]}\\ is the overcoverage rate for cell \\i\\;

- \\m_g^{(\pi)}\\ is the expected value for \\\pi_g\\ (specified via
  `prob`);

- \\d_g^{(\pi)}\\ is disperson for \\\pi_g\\ (specified via `prob`);

- \\m_h^{(\kappa)}\\ is the expected value for \\\kappa_h\\ (specified
  via `rate`); and

- \\d_h^{(\kappa)}\\ is disperson for \\\kappa_h\\ (specified via
  `rate`).

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

- [`set_datamod_overcount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_overcount.md)
  An overcount-only data model

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  All data models implemented in `bage`

- [confidential](https://bayesiandemography.github.io/bage/reference/confidential.md)
  Confidentialization procedures modeled in `bage`

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## specify 'prob' and 'rate'
prob <- data.frame(sex = c("Female", "Male"),
                   mean = c(0.95, 0.97),
                   disp = c(0.05, 0.05))
rate <- data.frame(mean = 0.03, disp = 0.15)

## specify model
mod <- mod_pois(divorces ~ age * sex + time,
                data = nzl_divorces,
                exposure = population) |>
  set_datamod_miscount(prob = prob, rate = rate)
mod
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    divorces ~ age * sex + time
#> 
#>                  exposure: population
#>                data model: miscount
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
#>                data model: miscount
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
#>        0.38     0.21      0.15   17      TRUE   relative convergence (4)
#> 

## original data, plus imputed values for outcome
mod |>
  augment()
#> ℹ Adding variable `.divorces` with true values for `divorces`.
#> # A tibble: 242 × 9
#>    age   sex     time divorces    .divorces population .observed
#>    <fct> <chr>  <int>    <dbl> <rdbl<1000>>      <dbl>     <dbl>
#>  1 15-19 Female  2011        0     0 (0, 1)     154460 0        
#>  2 15-19 Female  2012        6     6 (5, 7)     153060 0.0000392
#>  3 15-19 Female  2013        3     3 (2, 4)     152250 0.0000197
#>  4 15-19 Female  2014        3     3 (2, 4)     152020 0.0000197
#>  5 15-19 Female  2015        3     3 (2, 4)     152970 0.0000196
#>  6 15-19 Female  2016        3     3 (2, 4)     154170 0.0000195
#>  7 15-19 Female  2017        6     6 (5, 7)     154450 0.0000388
#>  8 15-19 Female  2018        0     0 (0, 1)     154170 0        
#>  9 15-19 Female  2019        3     3 (2, 4)     154760 0.0000194
#> 10 15-19 Female  2020        0     0 (0, 1)     154480 0        
#> # ℹ 232 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>

## parameter estimates
library(dplyr)
mod |>
  components() |>
  filter(term == "datamod")
#> # A tibble: 3 × 4
#>   term    component level               .fitted
#>   <chr>   <chr>     <chr>          <rdbl<1000>>
#> 1 datamod prob      Female       0.96 (0.82, 1)
#> 2 datamod prob      Male         0.98 (0.86, 1)
#> 3 datamod rate      rate   0.027 (0.011, 0.055)

## the data have in fact been confidentialized,
## so we account for that, in addition
## to accounting for undercoverage and
## overcoverage
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
#>                data model: miscount
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
#>        0.81     0.43      0.34   17      TRUE   relative convergence (4)
#> 
```
