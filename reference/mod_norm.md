# Specify a Normal Model

Specify a model where the outcome is drawn from a normal distribution.

## Usage

``` r
mod_norm(formula, data, weights = NULL)
```

## Arguments

- formula:

  An R [formula](https://rdrr.io/r/stats/formula.html), specifying the
  outcome and predictors.

- data:

  A data frame containing outcome, predictor, and, optionally, weights
  variables.

- weights:

  Name of the weights variable, a `1`, or a formula. See below for
  details.

## Value

An object of class `bage_mod_norm`.

## Details

The model is hierarchical. The means in the normal distribution are
described by a prior model formed from dimensions such as age, sex, and
time. The terms for these dimension themselves have models, as described
in
[priors](https://bayesiandemography.github.io/bage/reference/priors.md).
These priors all have defaults, which depend on the type of term (eg an
intercept, an age main effect, or an age-time interaction.)

## Scaling of outcome and weights

Internally, `mod_norm()` scales the outcome variable to have mean 0 and
standard deviation 1, and scales the weights to have mean 1. This
scaling allows `mod_norm()` to use the same menu of priors as
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
and
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md).

[`augment()`](https://generics.r-lib.org/reference/augment.html) always
returns values on the original scale, rather than the transformed scale.

[`components()`](https://generics.r-lib.org/reference/components.html)
by default returns values on the transformed scale. But if
`original_scale` is `TRUE`, it returns some types of values on the
original scale. See
[`components()`](https://generics.r-lib.org/reference/components.html)
for details.

## Specifying weights

There are three options for creating an unweighted model:

- do not supply a value for the `weights` variable;

- set `weights` equal to `NULL`; or

- **\[deprecated\]** set weights equal to `1`, though this option is
  deprecated, and will eventually be removed.

To create a weighted model, supply the name of the weighting variable in
`data`, quoted or unquoted.

## Mathematical details

The likelihood is

\$\$y_i \sim \text{N}(\gamma_i, w_i^{-1} \sigma^2)\$\$ where

- subscript \\i\\ identifies some combination of the classifying
  variables, such as age, sex, and time,

- \\y_i\\ is the value of the outcome variable,

- \\w_i\\ is a weight.

In some applications, \\w_i\\ is set to 1 for all \\i\\.

Internally, **bage** works with standardized versions of \\\gamma_i\\
and \\\sigma^2\\:

\$\$\mu_i = (\gamma_i - \bar{y}) / s\$\$ \$\$\xi^2 = \sigma^2 / (\bar{w}
s^2)\$\$ where \$\$\bar{y} = \sum\_{i=1}^n y_i / n\$\$ \$\$s =
\sqrt{\sum\_{i=1}^n (y_i - \bar{y})^2 / (n-1)}\$\$ \$\$\bar{w} =
\sum\_{i=1}^n w_i / n\$\$

Mean parameter \\\mu_i\\ is modelled as the sum of terms formed from
classifying variables and covariates,

\$\$\mu_i = \sum\_{m=0}^{M} \beta\_{j_i^m}^{(m)}\$\$

where

- \\\beta^{0}\\ is an intercept;

- \\\beta^{(m)}\\, \\m = 1, \dots, M\\, is a main effect or interaction;
  and

- \\j_i^m\\ is the element of \\\beta^{(m)}\\ associated with cell
  \\i\\,

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

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify binomial model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify non-default prior for term

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify non-default prior for standard deviation

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Fit a model

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for means, together with original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  Forecast parameters and outcomes

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Check model using a simulation study

- [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  Check model using replicate data data for a model

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  Detailed description of models

## Examples

``` r
## model without weights
mod <- mod_norm(value ~ diag:age + year,
                data = nld_expenditure)

## model with weights
nld_expenditure$wt <- sqrt(nld_expenditure$value)
mod <- mod_norm(value ~ diag:age + year,
                data = nld_expenditure,
                weights = wt)
```
