
# Roadmap

This document describes high-level plans for the development of `bage`. For detailed planning and issue tracking see [GitHub issues](https://github.com/bayesiandemography/bage/issues). For past changes, see [NEWS](https://github.com/bayesiandemography/bage/blob/main/NEWS.md).

---

## Scope of the bage package

#### Included

- Fast scalable, estimation and forecasting of disaggregated
  demographic rates
- 
- Measurement error models and confidentialization
- Automated handling of missing variables, imputation
- Support for complete Bayesian workflow

#### Not included

- Demographic accounts
  

---
## Moving out of experimental status

#### Current situation

- Still experimenting with algoriths, features
- API still subject to change (with warnings)

#### Version 1.0.0

- API stabilised: remove experimental tag
- Slower deprecation schedule
  - Higher bar for deprecation, and deprecate over a longer period

---

## Upcoming features

#### Fitting multiple models

- Automated model-fitting for multiple versions of inputs
  - Outcome and/or offset specified via rvecs
  - Auomatically run model multiple times and pool results
  - Optimization via re-use of model objects and starting points
- Simulation
  - Create S3 objects to hold simulation results
  - Simulation results queried via `augment()`, `components()`, `tidy()`, `glimpse()`
  - Support comparison of multiple models
- Leave One Group Out (LOGO) for model comparison
  - Compare models' ability to impute values when single categories dropped
  

## Extending suite of priors

- Origin-destination
- Spatial smoothing
- Local level and linear trend priors
- Regime switching


## Model outputs

- Automatic generation of model equations
- Automatic generation of HTML document describing model
- Option of more structured output from `components()`

## Optimisation

- Continued work behind the scenes to increase speed, reduce memory
  usage
  
  
## Measurement error models

- Expand current suite of generic measurement error models
- Utilities (eg small models) to help users create inputs for
  measurement error models


## Documentation

- More vignettes 
- Shiny app for exploring priors
- Long term: An open access book


---

## Current deprecated features

- Using `exposure = 1` to specify a counts-only model in `mod_pois()`
  - Soft-deprecated, but will be hard-deprecated before version 1.0.0
  - Alternative: `exposure = NULL`
- Using `weights = 1` to specify a non-weighted model in `mod_norm()`
  - Soft-deprecated, but will be hard-deprecated before version 1.0.0
  - Alternative: omit `weights` argument from call, or set `weights = NULL`
