# 7. Simulation

## 1 Introduction

Simulation studies can be used to assess the performance of a model. The
basic idea is to generate some parameter values, use these parameters to
generate some data, use the data to try to infer the original parameter
values, and then see how close the inferred parameter values are to the
actual parameter values.

![Simulation study of a model](vig07_fig.png)

Figure 1.1: Simulation study of a model

Function
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
automates the process of doing a simulation study. We are still
experimenting with
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md),
and the interface may change. Suggestions are welcome, ideally through
raising an issue
[here](https://github.com/bayesiandemography/bage/issues).

## 2 Estimation model matches data generation model

The most straightforward type of simulation is when the
`estimation model' used to do the inference matches the`data generation
model’ used to create the data. Even when the estimation model matches
the data generation model, the inferred values for the parameters will
not exactly reproduce the actual values, since data is drawn at random,
and provides a noisy signal about parameters it was generated from.
However, if the experiment is repeated many times, with a different
randomly-drawn dataset each time, the errors should more or less average
out at zero, 50% credible intervals should contain the true values close
to 50% of the time, and 95% credible intervals should contain the true
values close to 95% of the time.

To illustrate, we use investigate the performance of a model of divorce
rates in New Zealand.

We reduce the number of ages and time periods to speed up the
calculations.

``` r
library(bage)
#> Loading required package: rvec
#> 
#> Attaching package: 'rvec'
#> The following objects are masked from 'package:stats':
#> 
#>     sd, var
#> The following object is masked from 'package:base':
#> 
#>     rank
library(dplyr, warn.conflicts = FALSE)
library(poputils)

divorces_small <- nzl_divorces |>
  filter(age_upper(age) < 40,
         time >= 2018) |>
  droplevels()
```

We also replace the default weakly-informative priors with some
informative priors. Generating synthetic data from weakly-informative
priors leads to data with some absurdly high or low values. These is
little point in testing our model on data with values more extreme than
we will ever see. The extreme values can also cause numerical problems.
In upcoming releases of `bage` we intend to default informative versions
of priors. For the time being, however, we create the priors by hand.

``` r
mod <- mod_pois(divorces ~ age + sex + time,
                data = divorces_small,
                    exposure = population) |>
  set_prior(`(Intercept)` ~ Known(-1)) |>
  set_prior(age ~ RW(sd = 0.05, s = 0.05)) |>
  set_prior(time ~ AR1(s = 0.05))
mod     
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    divorces ~ age + sex + time
#> 
#>                  exposure: population
#> 
#>         term              prior along n_par n_par_free
#>  (Intercept)          Known(-1)     -     1          1
#>          age RW(s=0.05,sd=0.05)   age     4          4
#>          sex             NFix()     -     2          2
#>         time        AR1(s=0.05)  time     4          4
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     time     age           sex
```

To do the simulation study, we pass the model to
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md).
If only one model is supplied,
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
assumes that that model should be used as the estimation model and as
the data generation model. By default
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
repeats the experiment 100 times, generating a different dataset each
time.

``` r
set.seed(0)
res <- report_sim(mod_est = mod)
res
#> $components
#> # A tibble: 7 × 7
#>   term        component   .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>       <chr>        <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 (Intercept) effect     0           1         1         0           0    
#> 2 age         effect    -0.0107      0.518     0.958     0.103       0.297
#> 3 age         hyper      0.00948     0.45      0.8       0.0481      0.180
#> 4 sex         effect    -0.00397     0.455     0.905     0.303       0.873
#> 5 time        effect     0.00111     0.688     0.968     0.0669      0.194
#> 6 time        hyper      0.00417     0.45      0.9       0.0431      0.141
#> 7 disp        disp      -0.0705      0.52      0.97      0.265       0.779
#> 
#> $augment
#> # A tibble: 2 × 7
#>   .var      .observed      .error .cover_50 .cover_95 .length_50 .length_95
#>   <chr>         <dbl>       <dbl>     <dbl>     <dbl>      <dbl>      <dbl>
#> 1 .fitted       0.604 -0.00000674     0.476     0.949    0.00199    0.00576
#> 2 .expected     0.604 -0.0223         0.44      0.922    0.153      0.459
```

The output from
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
is a list of two data frames. The first data frame contains results for
parameters associated with the
[`components()`](https://generics.r-lib.org/reference/components.html)
function: main effects and interactions, associated hyper-parameters,
and dispersion. The second data frame contains results for parameters
associated with the
[`augment()`](https://generics.r-lib.org/reference/augment.html)
function: the lowest-level rates parameters.

As can be seen in the results, the errors do not average out at exactly
zero, 50% credible intervals do not contain the true value exactly 50%
of the time, and 95% credible intervals do not contain the true value
exactly 95% of the time. However, increasing the number of simulations
from the default value of 100 to, say, 1000 will reduce the average size
of the errors closer to zero, and bring the actual coverage rates closer
to their advertised values. When larger values of `n_sim` are used, it
can be helpful to use parallel processing to speed up calculations,
which is done through the `n_core` argument.

## 3 Estimation model different from data generation model

In actual applications, no estimation model ever perfectly describes the
true data generating process. It can therefore be helpful to see how
robust a given model is to misspecification, that is, to cases where the
estimation model differs from the data generation model.

With
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md),
this can be done by using one model for the `mod_est` argument, and a
different model for the `mod_sim` argument.

Consider, for instance, a case where the time effect is generated from a
random walk, while the estimation model continues to use a first-order
autoregressive prior,

``` r
mod_rw <- mod |>
  set_prior(time ~ RW(s = 0.05))
mod_rw  
#> 
#>     ------ Unfitted Poisson model ------
#> 
#>    divorces ~ age + sex + time
#> 
#>                  exposure: population
#> 
#>         term              prior along n_par n_par_free
#>  (Intercept)          Known(-1)     -     1          1
#>          age RW(s=0.05,sd=0.05)   age     4          4
#>          sex             NFix()     -     2          2
#>         time         RW(s=0.05)  time     4          4
#> 
#>  disp: mean = 1
#> 
#>  n_draw var_time var_age var_sexgender
#>    1000     time     age           sex
```

A simulation where the estimation model differs from the data generation
mode can be generated as follows:

``` r
set.seed(0)
report_sim(mod_est = mod, mod_sim = mod_rw) ## not run
```

## 4 The relationship between `report_sim()` and `replicate_data()`

Functions
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
and
[`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
overlap, in that both use simulated data to provide insights into model
performance. Their aims are, however, different. Typically,
[`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
is used before fitting a model, to assess its performance across a
random selection of possible datasets, while
[`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
is used after fitting a model, to assess its performance on the dataset
to hand.
