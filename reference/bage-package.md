# Package 'bage'

Bayesian estimation and forecasting of age-specific rates. Estimation
uses [TMB](https://CRAN.R-project.org/package=TMB), and is fast.

## Example workflow

1.  Specify model using
    [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)

2.  Fit model using
    [`fit()`](https://generics.r-lib.org/reference/fit.html)

3.  Extract results using
    [`augment()`](https://generics.r-lib.org/reference/augment.html)

4.  Check model using
    [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)

## Functions

**Specify model**

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  Specify a Poisson model

- [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md)
  Specify a binomial model

- [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a normal model

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for main effect or interaction

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors for main effects or interactions

- [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md)
  Specify prior for dispersion/variance

- [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
  Add covariates to model

- [datamods](https://bayesiandemography.github.io/bage/reference/datamods.md)
  Overview of data models (measurement error models)

- [confidential](https://bayesiandemography.github.io/bage/reference/confidential.md)
  Overview of confidentialization models

**Fit model**

- [`fit()`](https://generics.r-lib.org/reference/fit.html) Derive
  posterior distribution

**Extract results**

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Original data, plus observation-level estimates

- [components()](https://bayesiandemography.github.io/bage/reference/components.bage_mod.md)
  Hyper-parameters

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Dispersion parameter (a type of hyper-parameter)

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) One-line
  summary

- [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md)
  Specify number of prior or posterior draws

**Forecast**

- [`forecast()`](https://generics.r-lib.org/reference/forecast.html) Use
  model to obtain estimates for future periods

**Check model**

- [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  Compare real and simulated data

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  Simulation study of model

## Data

- [datasets](https://bayesiandemography.github.io/bage/reference/datasets.md)
  Overview of datasets

- [svds](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs

## See also

Useful links:

- <https://bayesiandemography.github.io/bage/>

- <https://github.com/bayesiandemography/bage>

- Report bugs at <https://github.com/bayesiandemography/bage/issues>

## Author

**Maintainer**: John Bryant <john@bayesiandemography.com>

Authors:

- Junni Zhang <junnizhang@163.com>

Other contributors:

- Bayesian Demography Limited \[copyright holder\]
