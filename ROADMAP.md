# Roadmap

This document describes high-level plans for the development of `bage`.
For detailed planning and issue tracking see [GitHub
issues](https://github.com/bayesiandemography/bage/issues). For past
changes, see
[NEWS](https://github.com/bayesiandemography/bage/blob/main/NEWS.md).

------------------------------------------------------------------------

## Scope of the bage package

What we are aiming for in the long run.

#### Included

- Fast scalable, estimation and forecasting of disaggregated demographic
  rates
- Measurement error models and confidentialization
- Automated imputation
- Support for complete Bayesian workflow
  - Prior predictive distribution
  - Posterior prediction distribution
  - Simulation
  - Model comparison

#### Not included

- Demographic accounts
- Individual-level models

------------------------------------------------------------------------

## Upcoming releases

#### 0.12.0 - Fitting multiple models

- Automated model-fitting for multiple versions of inputs
  - Outcome and/or offset specified via rvecs
  - Auomatically run model multiple times and pool results
  - Optimization via re-use of model objects and starting points
- Simulation
  - Create S3 objects to hold simulation results
  - Simulation results queried via
    [`augment()`](https://generics.r-lib.org/reference/augment.html),
    [`components()`](https://generics.r-lib.org/reference/components.html),
    [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
    `glimpse()`
  - Support comparison of multiple models
- Leave One Group Out (LOGO) for model comparison
  - Compare models’ ability to impute values when single categories
    dropped

#### Revised approach to standardization

- Will be implemented if experiments successful
- Revised approach to standardization of terms in prior model
  - Estimates for non-time dimensions that already appear in the model
    will be centered within TMB
  - The `con` argument in `set_prior_*()` functions (which controls
    standardization at present) will be removed (it will be redundant,
    as all terms will be standardized)

#### Sparse Poisson and binomial models

- Will be implemented if experiments successful
- Specialised calculations in case where outcomes include high (ie 95%
  or more) percentage of zeros

#### Version 1.0.0

- API stabilised: remove experimental tag
- Slower deprecation schedule
  - Higher bar for deprecation, and deprecate over a longer period

------------------------------------------------------------------------

## Current deprecated features

- Using `exposure = 1` to specify a counts-only model in
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  - Soft-deprecated, but will be hard-deprecated before version 1.0.0
  - Alternative: `exposure = NULL`
- Using `weights = 1` to specify a non-weighted model in
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  - Soft-deprecated, but will be hard-deprecated before version 1.0.0
  - Alternative: omit `weights` argument from call, or set
    `weights = NULL`
- Users warned that `con` argument to `set_prior_*()` functions
  experimental
