# Specify a Binomial Model

Specify a model where the outcome is drawn from a binomial distribution.

## Usage

``` r
mod_binom(formula, data, size)
```

## Arguments

- formula:

  An R [formula](https://rdrr.io/r/stats/formula.html), specifying the
  outcome and predictors.

- data:

  A data frame containing the outcome and predictor variables, and the
  number of trials.

- size:

  Name of the variable giving the number of trials (with or without
  quote marks.)

## Value

An object of class `bage_mod`.

## Details

The model is hierarchical. The probabilities in the binomial
distribution are described by a prior model formed from dimensions such
as age, sex, and time. The terms for these dimension themselves have
models, as described in
[priors](https://bayesiandemography.github.io/bage/reference/priors.md).
These priors all have defaults, which depend on the type of term (eg an
intercept, an age main effect, or an age-time interaction.)

## Mathematical details

The likelihood is

\$\$y_i \sim \text{binomial}(\gamma_i; w_i)\$\$

where

- subscript \\i\\ identifies some combination of the the classifying
  variables, such as age, sex, and time;

- \\y_i\\ is a count, such of number of births, such as age, sex, and
  region;

- \\\gamma_i\\ is a probability of 'success'; and

- \\w_i\\ is the number of trials.

The probabilities \\\gamma_i\\ are assumed to be drawn a beta
distribution

\$\$y_i \sim \text{Beta}(\xi^{-1} \mu_i, \xi^{-1} (1 - \mu_i))\$\$

where

- \\\mu_i\\ is the expected value for \\\gamma_i\\; and

- \\\xi\\ governs dispersion (ie variance.)

Expected value \\\mu_i\\ equals, on a logit scale, the sum of terms
formed from classifying variables,

\$\$\text{logit} \mu_i = \sum\_{m=0}^{M} \beta\_{j_i^m}^{(m)}\$\$

where

- \\\beta^{0}\\ is an intercept;

- \\\beta^{(m)}\\, \\m = 1, \dots, M\\, is a main effect or interaction;
  and

- \\j_i^m\\ is the element of \\\beta^{(m)}\\ associated with cell
  \\i\\.

The \\\beta^{(m)}\\ are given priors, as described in
[priors](https://bayesiandemography.github.io/bage/reference/priors.md).

\\\xi\\ has an exponential prior with mean 1. Non-default values for the
mean can be specified with
[`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md).

The model for \\\mu_i\\ can also include covariates, as described in
[`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md).

## See also

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify Poisson model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify normal model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify non-default prior for term

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify non-default prior for dispersion

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for probabilities, together with original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  Forecast parameters and outcomes

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Check model using simulation study

- [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  Check model using replicate data

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  Detailed descriptions of models

## Examples

``` r
mod <- mod_binom(oneperson ~ age:region + age:year,
                 data = nzl_households,
                 size = total)
```
