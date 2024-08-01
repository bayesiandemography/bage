
# bage 0.6.2

## Changes to interface

* Removed `SVDS()`, `SVDS_AR()`, `SVDS_AR1()`, `SVDS_RW()`, and
  `SVDS_RW2()` priors. Added `indep` argument to corresponding `SVD`
  priors. `SVD` priors now choose between 'total', 'independent' and
  'joint' models based on (1) the value of `indep` argument, (2) the
  value of `var_sexgender` and the name of the term.
  
## Changes to data

* Object `HMD` now contains 5 components, rather than 10.


# bage 0.6.1

## Changes to calculations

* Fixed problems with standardization of forecast
* Added an intercept term to `Lin()` and `LinAR()` priors

# bage 0.6.0

## Issues

* Standardization of forecasts not working correctly.

## Changes to interface

* Added priors `SVD_AR()`, `SVDS_AR()`, `SVD_AR1()`, `SVDS_AR1()`,
  `SVD_RW()`, `SVDS_RW()`, `SVD_RW2()`, `SVDS_RW2()`

## Internal calculations

* Changed values that are stored in object: removed `draws_linpred`, added
 `draws_effectfree`, `draws_spline`, and `draws_svd`. Modified/added
 downstream functions.
* Calculation of 'along_by' and 'agesex' matrices pushed downwards
  into lower-level functions.

# bage 0.5.1

## Changes to interface

* Moved HMD code to package **bssvd**.

# bage 0.5.0

## Changes to interface

* Combined interaction (eg ELin) and main effect (eg Lin) versions of
  priors
* Removed function `compose_time()`
* Added priors RWSeas and RW2Seas
* Improved `report_sim()`


# bage 0.4.2

## Changes to interface

* Tidying of online help (not yet complete).

# bage 0.4.1

## New functions

* Added 'bage_ssvd' method for `components()`.

## Changes to interface

* `augment()` method for `bage_mod` objects now calculated value for
  `.fitted` in cases where the outcome or exposure/size is NA, rather
  than setting the value of `.fitted` to `NA`.
  
## Internal calculations

* Standardization of effects only done if `components()` is
  called. `augment()` uses the linear predictor (which does not need
  standardization.)
* Internally, draws for the linear predictor, the hyper-parameters and
  (if included in model) `disp` are stored, rather than the full
  standardized components.
* Standardization algorithm repeats up to 100 times, or until all
  residuals are less than 0.0001.
* With the new configuration, calculations for large matrices that
  previously failed with error message "Internal error: Final residual
  not 0" are now running.
  
## Simulations

* When drawing from the prior, the intercept is always set to 0. Terms
  with SVD or Known priors are not touched. All other terms are
  centered.


# bage 0.4.0

## Changes to back-end for SVD priors

* Move most functions for creating 'bage_ssvd' objects to package
  'bssvd'.
* Allowed number of components of a 'bage_ssvd' object to differ from
  10.
  
## Bug fixes

* Corrected error in calculation of logit in `ssvd_comp()`.


# bage 0.3.2

## New functions

* `forecast.bage_mod()` Forecasting. Interface not yet finalised.

## Bug fixes

* Corrected error in C++ template for Lin and ELin priors (due to use
  of integer arithmetic.)

# bage 0.2.2

## New functions

* `generate.bage_ssvd()` Generate random age-sex profiles from SVD.

## Bug fixes

* Internal function `draw_vals_effect_mod()` was malfunctioning on models that contained SVD priors.
