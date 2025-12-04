# Specify Noise Data Model

Specify a data model in which

`observed outcome = true outcome + error`,

where the error has a symmetric distribution with mean 0.

If the true outcome has a normal distribution, then the error has a
normal distribution. If the true outcome has a Poisson distribution,
then the error has a symmetric Skellam distribution.

## Usage

``` r
set_datamod_noise(mod, sd)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  or
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

- sd:

  Standard deviation of measurement errors. A single number, or a data
  frame with 'by' variables.

## Value

A revised version of `mod`.

## Details

The model assumes that the outcome variable is unbiased. If there is in
fact evidence of biases, then this evidence should be used to create a
de-biased version of the outcome variable in `data`, and this de-biased
version should be used by
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
or
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md).

If `set_datamod_noise()` is used with a Poisson model, then the
dispersion term for the Poisson rates must be set to zero. This can be
done using
[`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md),
though `set_datamod_noise()` will also do so.

## The Skellam distribution

The Skellam distribution is restricted to integers, but can take
positive and negative values.

If

\$\$X_1 \sim \text{Poisson}(\mu_1)\$\$ \$\$X_2 \sim
\text{Poisson}(\mu_2)\$\$

then

\$\$Y = X_1 - X_2\$\$

has a \\\text{Skellam}(\mu_1, \mu_2)\\ distribution. If \\\mu_1 =
\mu_2\\, then the distribution is symmetric.

## The `sd` argument

`sd` can be a single number, in which case the same standard deviation
is used for all cells. `sd` can also be a data frame with a with a
variable called `"sd"` and one or more columns with 'by' variables. For
instance, a `sd` of

    data.frame(sex = c("Female", "Male"),
               sd = c(330, 240))

implies that measurement errors have standard deviation 330 for females
and 240 for males.

## Mathematical details

The model for the observed outcome is

\$\$y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i\$\$

with

\$\$\epsilon_i \sim \text{N}(0, s\_{g\[i\]}^2)\$\$

if \\y_i^{\text{true}}\\ has a normal distribution, and

\$\$\epsilon_i \sim \text{Skellam}(0.5 s\_{g\[i\]}^2, 0.5
s\_{g\[i\]}^2)\$\$

if \\y_i^{\text{true}}\\ has a Poisson distribution, where

- \\y_i^{\text{obs}}\\ is the observed outcome for cell \\i\\;

- \\y_i^{\text{true}}\\ is the true outcome for cell \\i\\;

- \\\epsilon_i\\ is the measurement error for cell \\i\\; and

- \\s\_{g\lbrack i\rbrack }\\ is the standard deviation of the
  measurement error for cell \\i\\.

## See also

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Original data plus estimated values, including estimates of true value
  for outcome

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  Data models implemented in `bage`

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
## Normal model ------------------------------

## prepare outcome variable
library(dplyr, warn.conflicts = FALSE)
spend <- nld_expenditure |>
  mutate(log_spend = log(value + 1))

## specify model
mod <- mod_norm(log_spend ~ age * diag + year,
                data = spend,
                weights = 1) |>
  set_datamod_noise(sd = 0.1)

## fit model
mod <- mod |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod
#> 
#>     ------ Fitted normal model ------
#> 
#>    log_spend ~ age * diag + year
#> 
#>                data model: noise
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age    18         18    0.28
#>         diag    N()     -    18         18    0.05
#>         year   RW()  year     4          4    0.06
#>     age:diag   RW()   age   324        324    0.93
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age optimizer
#>    1000     year     age    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        0.35     0.13      0.19   20      TRUE   relative convergence (4)
#> 

## create new aggregated diagnositic
## group variable
library(dplyr, warn.conflicts = FALSE)
spend <- spend |>
  mutate(diag_ag = case_when(
    diag == "Neoplasms" ~ diag,
    diag == "Not allocated" ~ diag,
    TRUE ~ "Other"
  ))

## assume size of measurement errors
## varies across these aggregated groups
sd_diag <- data.frame(diag_ag = c("Neoplasms",
                                  "Not allocated",
                                  "Other"),
                      sd = c(0.05, 0.2, 0.1))

## fit model that uses diagnostic-specific
## standard deviations
mod <- mod_norm(log_spend ~ age * diag + year,
                data = spend,
                weights = 1) |>
  set_datamod_noise(sd = sd_diag)


## Poisson model -----------------------------

mod <- mod_pois(deaths ~ month,
                data = usa_deaths,
                exposure = 1) |>
  set_datamod_noise(sd = 200)
#> → Setting dispersion to zero. (Required when using noise data model with Poisson rates model.)
```
