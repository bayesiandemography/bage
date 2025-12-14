# Specify a Poisson Model

Specify a model where the outcome is drawn from a Poisson distribution.

## Usage

``` r
mod_pois(formula, data, exposure)
```

## Arguments

- formula:

  An R [formula](https://rdrr.io/r/stats/formula.html), specifying the
  outcome and predictors.

- data:

  A data frame containing outcome, predictor, and, optionally, exposure
  variables.

- exposure:

  Name of the exposure variable or `NULL`. See below for details.

## Value

An object of class `bage_mod_pois`.

## Details

The model is hierarchical. The rates in the Poisson distribution are
described by a prior model formed from dimensions such as age, sex, and
time. The terms for these dimension themselves have models, as described
in
[priors](https://bayesiandemography.github.io/bage/reference/priors.md).
These priors all have defaults, which depend on the type of term (eg an
intercept, an age main effect, or an age-time interaction.)

## Specifying exposure

The `exposure` argument can take three forms:

- the name of a variable in `data`, with or without quote marks, eg
  `"population"` or `population`;

- `NULL`, in which case a pure "counts" model with no exposure, is
  produced; or

- **\[deprecated\]** the number `1`, in which case a pure "counts" model
  is also produced (though this option is deprecated, and will
  eventially be removed).

## Mathematical details

The likelihood is

\$\$y_i \sim \text{Poisson}(\gamma_i w_i)\$\$

where

- subscript \\i\\ identifies some combination of the classifying
  variables, such as age, sex, and time;

- \\y_i\\ is an outcome, such as deaths;

- \\\gamma_i\\ is rates; and

- \\w_i\\ is exposure.

In some applications, there is no obvious population at risk. In these
cases, exposure \\w_i\\ can be set to 1 for all \\i\\.

The rates \\\gamma_i\\ are assumed to be drawn a gamma distribution

\$\$y_i \sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1})\$\$

where

- \\\mu_i\\ is the expected value for \\\gamma_i\\; and

- \\\xi\\ governs dispersion (i.e. variation), with lower values
  implying less dispersion.

Expected value \\\mu_i\\ equals, on the log scale, the sum of terms
formed from classifying variables,

\$\$\log \mu_i = \sum\_{m=0}^{M} \beta\_{j_i^m}^{(m)}\$\$

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

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify normal model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify non-default prior for term

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify non-default prior for dispersion

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, together with original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  Forecast parameters and outcomes

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Check model using a simulation study

- [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  Check model using replicate data

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  Detailed description of models

## Examples

``` r
## model with exposure
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn)

## model without exposure
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = NULL)
```
