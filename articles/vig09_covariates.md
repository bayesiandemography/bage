# Covariates

## 1 Introduction

Covariates can be used to extend standard **bage** models in two ways:

1.  incorporating information beyond what is contained in classifying
    variables, such as age, sex, or region; and
2.  dealing with subsets of the data with unusual behavior.

This vignette gives a brief description of covariates in **bage** and
then illustrates their use with a case study of births in South Korea.

## 2 Mathematical details

A model in **bage** typically has a structure that is something like:
\\\begin{align} y\_{ast} & \sim \text{Poisson}(\gamma\_{ast} w\_{ast})
\\ \log \gamma\_{ast} & \sim \text{Gamma}(\xi^{-1}, (\mu\_{ast}
\xi)^{-1}) \\ \log \mu\_{ast} & = \beta^{(0)} +
\beta\_{as}^{\text{age:sex}} + \beta_t^{\text{time}} \tag{2.1}
\end{align}\\ where

- \\y\_{ast}\\ is the outcome for people in age group \\a\\ and sex
  \\s\\ during period \\t\\;
- \\w\_{ast}\\ is exposure;
- \\\gamma\_{ast}\\ is a rate;
- \\\xi\\ governs overall dispersion;
- \\\beta^{(0)}\\ is an intercept;
- \\\pmb{\beta}^{\text{age:sex}}\\ and \\\pmb{\beta}^{\text{time}}\\ are
  terms formed the classifying variables age, sex, and time;
- \\\beta\_{as}^{\text{age:sex}}\\ and \\\pmb{\beta}\_t^{\text{time}}\\
  are elements of these terms; and
- the intercept and the terms formed from the classifying variables all
  have prior distributions.

In a model with covariates, [(2.1)](#eq:priormod) changes to
\\\begin{equation} \log \mu\_{ast} = \beta^{(0)} +
\beta\_{as}^{\text{age:sex}} + \beta_t^{\text{time}} + (\pmb{Z}
\pmb{\zeta})\_{ast} \end{equation}\\

where

- \\\pmb{Z}\\ is a \\N \times P\\ covariate matrix;
- \\\pmb{\zeta}\\ is a \\P\\-element vector of coefficients; and
- \\(\pmb{Z} \pmb{\zeta})\_{ast}\\ is an element from the vector
  obtained by multiplying \\\pmb{Z}\\ and \\\pmb{\zeta}\\.

Change [(2.2)](#eq:prior-mod-no-cov) to \\\begin{equation} \mu_i =
\sum\_{m=0}^M \beta\_{j_i^m}^{(m)} + (\pmb{Z} \pmb{\eta})\_i \tag{2.2}
\end{equation}\\

where - \\\pmb{Z}\\ is an \\I \times P\\ matrix of covariates; and -
\\\pmb{\eta}\\ is a vector of coefficients.

The covariate matrix \\\pmb{Z}\\ is derived from the raw covariate data
by scaling any numeric variables to have mean 0 and standard deviation
1, and by converting any categorical variables to sets of indicator
variables. The conversion to indicator variables follows the rules that
R uses for “treatment” contrasts. If the categorical has \\C\\
categories, then \\C-1\\ indicator variabls are constructed, with the
first category being omitted.

Each element of \\\pmb{\eta}\\ has prior \\\begin{equation} \eta_p \sim
\text{N}(0, 1) \end{equation}\\

## 3 Example: Births in South Korea

To illustrate the use of covariates, we will analyse data on births in
South Korea.

### 3.1 Preliminaries

Besides **bage** itself, we use **dplyr** and **vctrs** for data
manipulation, and **ggplot2** for plotting results.

``` r
suppressPackageStartupMessages({
  library(bage)
  library(dplyr)
  library(ggplot2)
})
```

Our data is a subset of the the data frame `kor_births`, which is part
of **bage**.

``` r
births <- kor_births |>
  filter(region %in% levels(region)[1:5])
```

The data frame gives numbers of births disaggregated by age of mother,
region, and year. It also contains a numeric variable called
`gdp_pc_2023` that gives GDP per capita (in thousands of US dollars) in
2023, and a categorical variable (with levels `"Low"`, `"Medium"`, and
`"High"`) describing population density in 2020.

``` r
births
#> # A tibble: 585 × 7
#>    age   region  time births  popn gdp_pc_2023 dens_2020
#>    <chr> <fct>  <int>  <int> <int>       <dbl> <chr>    
#>  1 10-14 Busan   2011      1 89822        25.7 High     
#>  2 10-14 Busan   2012      0 83884        25.7 High     
#>  3 10-14 Busan   2013      0 79061        25.7 High     
#>  4 10-14 Busan   2014      1 74741        25.7 High     
#>  5 10-14 Busan   2015      0 68783        25.7 High     
#>  6 10-14 Busan   2016      1 64905        25.7 High     
#>  7 10-14 Busan   2017      1 64251        25.7 High     
#>  8 10-14 Busan   2018      0 63249        25.7 High     
#>  9 10-14 Busan   2019      0 62154        25.7 High     
#> 10 10-14 Busan   2020      0 63498        25.7 High     
#> # ℹ 575 more rows
```

### 3.2 Covariates that bring in extra information

The variables `gdp_pc_2023` and `dens_2020` are both examples of
covariates that contribute extra information to the model, beyond what
is contained in the outcome, exposure, and classifying variables.

We use function
[`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
to instruct
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
to treat these variables as covariates.

``` r
mod_gdp_dens <- mod_pois(births ~ (age + region + time)^2,
                         data = births,
                         exposure = popn) |>
  set_covariates(~ gdp_pc_2023 + dens_2020) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod_gdp_dens
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    births ~ (age + region + time)^2
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age     9          9    2.70
#>       region    N()     -    16         16    0.21
#>         time   RW()  time    13         13    0.20
#>   age:region   RW()   age   144        144    0.22
#>     age:time   RW()  time   117        117    1.14
#>  region:time   RW()  time   208        208    0.23
#> 
#>  covariates: ~gdp_pc_2023 + dens_2020
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age optimizer
#>    1000     time     age    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        1.45     0.82      0.48   30      TRUE   relative convergence (4)
```

To obtain estimates of the coefficients (ie estimates of the
\\\zeta_p\\) we call function
[`components()`](https://generics.r-lib.org/reference/components.html)
and filter out rows for the `"covariates"` term.

``` r
mod_gdp_dens |>
  components() |>
  filter(term == "covariates")
#> # A tibble: 2 × 4
#>   term       component level                      .fitted
#>   <chr>      <chr>     <chr>                 <rdbl<1000>>
#> 1 covariates coef      gdp_pc_2023     -0.23 (-1.4, 0.99)
#> 2 covariates coef      dens_2020Medium -0.93 (-2.5, 0.73)
```

### 3.3 Covariates that allow for unusual subsets

In East Asia, years with the Dragon zodiac sign sometimes have
larger-than-usual numbers of births. To allow for this possibility, we
create a dragon-year covariate, and incorporate it into a new model.

``` r
births <- births |>
  mutate(is_dragon_year = time == 2012)
mod_dragon <- mod_pois(births ~ (age + region + time)^2,
                      data = births,
                      exposure = popn) |>
  set_covariates(~ is_dragon_year) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...

mod_dragon |>
  components() |>
  filter(term == "covariates")
#> # A tibble: 1 × 4
#>   term       component level                           .fitted
#>   <chr>      <chr>     <chr>                      <rdbl<1000>>
#> 1 covariates coef      is_dragon_yearTRUE 0.069 (-0.045, 0.18)
```

There is some evidence for extra births, though there is substantial
uncertainty about the size of the effect.

Next we expand the model to allow the dragon-year effect to differ
across age groups. We create a variable that takes the values
`"baseline"` in all years, except in dragon years, when it takes the
name of the age group. We turn this variable into a factor with
`"baseline"` as its first level.

``` r
births <- births |>
  mutate(is_dragon_year_age = if_else(time == 2012, age, "baseline"),
         is_dragon_year_age = factor(is_dragon_year_age, 
                                     levels = c("baseline", unique(age))))
births |>
  filter(time %in% 2011:2013)
#> # A tibble: 135 × 9
#>    age   region   time births  popn gdp_pc_2023 dens_2020 is_dragon_year
#>    <chr> <fct>   <int>  <int> <int>       <dbl> <chr>     <lgl>         
#>  1 10-14 Busan    2011      1 89822        25.7 High      FALSE         
#>  2 10-14 Busan    2012      0 83884        25.7 High      TRUE          
#>  3 10-14 Busan    2013      0 79061        25.7 High      FALSE         
#>  4 10-14 Daegu    2011      3 75776        22.3 Medium    FALSE         
#>  5 10-14 Daegu    2012      2 70399        22.3 Medium    TRUE          
#>  6 10-14 Daegu    2013      0 66858        22.3 Medium    FALSE         
#>  7 10-14 Gwangju  2011      1 53033        26.0 Medium    FALSE         
#>  8 10-14 Gwangju  2012      3 49716        26.0 Medium    TRUE          
#>  9 10-14 Gwangju  2013      0 46918        26.0 Medium    FALSE         
#> 10 10-14 Incheon  2011      0 83927        29.3 Medium    FALSE         
#> # ℹ 125 more rows
#> # ℹ 1 more variable: is_dragon_year_age <fct>
```

We create a new model with the age-sepcific dragon-year indicator.

``` r
mod_dragon_age <- mod_pois(births ~ (age + region + time)^2,
                         data = births,
                         exposure = popn) |>
  set_covariates(~ is_dragon_year_age) |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
mod_dragon_age
#> 
#>     ------ Fitted Poisson model ------
#> 
#>    births ~ (age + region + time)^2
#> 
#>                  exposure: popn
#> 
#>         term  prior along n_par n_par_free std_dev
#>  (Intercept) NFix()     -     1          1       -
#>          age   RW()   age     9          9    2.71
#>       region    N()     -    16         16    0.16
#>         time   RW()  time    13         13    0.18
#>   age:region   RW()   age   144        144    0.23
#>     age:time   RW()  time   117        117    1.16
#>  region:time   RW()  time   208        208    0.22
#> 
#>  covariates: ~is_dragon_year_age
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age optimizer
#>    1000     time     age    nlminb
#> 
#>  time_total time_max time_draw iter converged                    message
#>        1.02     0.42      0.55   22      TRUE   relative convergence (4)
```

Rather than a single dragon-year coefficient, we have a coefficient for
each age group. We extract them and tidy up the labels.

``` r
mod_dragon_age |>
  components() |>
  filter(term == "covariates") |>
  mutate(age = sub("is_dragon_year_age", "", level)) |>
  select(age, .fitted)
#> # A tibble: 9 × 2
#>   age               .fitted
#>   <chr>        <rdbl<1000>>
#> 1 10-14  0.58 (-0.024, 1.3)
#> 2 15-19 -0.012 (-0.21, 0.2)
#> 3 20-24 0.045 (-0.16, 0.23)
#> 4 25-29 0.065 (-0.12, 0.25)
#> 5 30-34 0.079 (-0.11, 0.26)
#> 6 35-39 0.043 (-0.15, 0.25)
#> 7 40-44  0.072 (-0.1, 0.26)
#> 8 45-49  0.083 (-0.2, 0.35)
#> 9 50-54  0.33 (-0.25, 0.92)
```

### 3.4 Forecasting

If all the covariates in a model are fixed, then the model can be
forecasted as normal.

``` r
mod_gdp_dens |>
  forecast(labels = 2024:2025)
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> # A tibble: 90 × 10
#>    age   region   time births  popn gdp_pc_2023 dens_2020 .observed
#>    <chr> <fct>   <int>  <dbl> <int>       <dbl> <chr>         <dbl>
#>  1 10-14 Busan    2024     NA    NA        25.7 High             NA
#>  2 10-14 Busan    2025     NA    NA        25.7 High             NA
#>  3 10-14 Daegu    2024     NA    NA        22.3 Medium           NA
#>  4 10-14 Daegu    2025     NA    NA        22.3 Medium           NA
#>  5 10-14 Gwangju  2024     NA    NA        26.0 Medium           NA
#>  6 10-14 Gwangju  2025     NA    NA        26.0 Medium           NA
#>  7 10-14 Incheon  2024     NA    NA        29.3 Medium           NA
#>  8 10-14 Incheon  2025     NA    NA        29.3 Medium           NA
#>  9 10-14 Seoul    2024     NA    NA        43.4 High             NA
#> 10 10-14 Seoul    2025     NA    NA        43.4 High             NA
#> # ℹ 80 more rows
#> # ℹ 2 more variables: .fitted <rdbl<1000>>, .expected <rdbl<1000>>
```

If, however, a covariate varies over time, forecasting only works if
values for future periods are provided. The following code will result
in an error:

    mod_dragon |>
      forecast(labels = 2024:2025)

Instead we need to create a `newdata` data frame…

``` r
newdata <- expand.grid(age = unique(kor_births$age),
                       region = unique(kor_births$region),
                       time = 2024:2025) |>
  mutate(is_dragon_year = FALSE)
head(newdata)
#>     age region time is_dragon_year
#> 1 10-14  Busan 2024          FALSE
#> 2 15-19  Busan 2024          FALSE
#> 3 20-24  Busan 2024          FALSE
#> 4 25-29  Busan 2024          FALSE
#> 5 30-34  Busan 2024          FALSE
#> 6 35-39  Busan 2024          FALSE
```

…and supply it to
[`forecast()`](https://generics.r-lib.org/reference/forecast.html).

``` r
mod_dragon |>
  forecast(newdata = newdata)
#> `components()` for past values...
#> `components()` for future values...
#> `augment()` for future values...
#> # A tibble: 288 × 11
#>    age   region           time births  popn gdp_pc_2023 dens_2020 is_dragon_year
#>    <chr> <fct>           <int>  <dbl> <int>       <dbl> <chr>     <lgl>         
#>  1 10-14 Busan            2024     NA    NA          NA NA        FALSE         
#>  2 15-19 Busan            2024     NA    NA          NA NA        FALSE         
#>  3 20-24 Busan            2024     NA    NA          NA NA        FALSE         
#>  4 25-29 Busan            2024     NA    NA          NA NA        FALSE         
#>  5 30-34 Busan            2024     NA    NA          NA NA        FALSE         
#>  6 35-39 Busan            2024     NA    NA          NA NA        FALSE         
#>  7 40-44 Busan            2024     NA    NA          NA NA        FALSE         
#>  8 45-49 Busan            2024     NA    NA          NA NA        FALSE         
#>  9 50-54 Busan            2024     NA    NA          NA NA        FALSE         
#> 10 10-14 Chungcheongbuk…  2024     NA    NA          NA NA        FALSE         
#> # ℹ 278 more rows
#> # ℹ 3 more variables: .observed <dbl>, .fitted <rdbl<1000>>,
#> #   .expected <rdbl<1000>>
```
