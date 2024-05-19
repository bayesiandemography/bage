
# bage 0.4.1

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
  previously failed are now running.
  
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
