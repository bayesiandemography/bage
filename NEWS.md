
# bage 0.4.1

## New functions

* Added 'bage_ssvd' method for `components()`.

## Changes to interface

* `augment()` method for `bage_mod` objects now calculated value for
  `.fitted` in cases where the outcome or exposure/size is NA, rather
  than setting the value of `.fitted` to `NA`.


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
