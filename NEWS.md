
# bage 0.7.8

## Datasets

* Added `HFD`, a scaled SVD object holding data from the Human
  Fertiltiy Database
* Changed names of data objects:
  - `deaths` --> `isl_deaths`
  - `expenditure` --> `nld_expenditure`
  - `divorces` --> `nzl_divorces`
  - `injuries` --> `nzl_injuries`
  - `us_acc_deaths` --> `usa_deaths`
* Added new data object `kor_births`, births in South Korea


# bage 0.7.7

## Bug fixes

* `report_sim()` now works on fitted models. Thank you to Ollie Pike
  for pointing out that it previously did not.
* Removed redundant levels from `age` variable in `divorces`.

## Changes to internal calculations

* Removed internal **bage** function `rr3()`. Call **poputils**
  function `rr3()` instead.


# bage 0.7.6

## Changes to interface

* Added `newdata` argument to `forecast()`.
* Added minimum version numbers for **rvec** and **poputils**.


# bage 0.7.5.1

## Bug fixes

* Fixed bug in code for simulating from `Lin()` and `Lin_AR()` priors.


# bage 0.7.5

## Changes to interface

* Added arguments `method` and `vars_inner` to `fit()`. When
  `method` is `"standard"` (the default) `fit()` uses the existing
  calculation methods. When `method` is `"inner-outer"`, `fit()` uses
  a new, somewhat experimental calculation method that involves
  fitting an inner model using a subset of variables, and then an
  outer model using the remaining variables. With big datasets,
  `"inner-outer"` can be faster, and use less memory, but give very
  similar results.
* Added information on numbers of parameters, and standard deviations
  to output for print. Thank you to Duncan Elliot for suggesting
  printing numbers of parameters.
  
## Changes to calculations

* `fit()` now internally aggregates input data before fitting, so that
  cells with the same combinations of predictor variables are
  combined. This increases speed and reduces memory usage.
  
## Changes to documentation

* Added help for `print.bage_mod`

# bage 0.7.4

## Changes to interface

* Function `ssvd()` no longer exported. Will export once package
  **bssvd** matures.
* **bage** released on to CRAN


# bage 0.7.3

## Changes to data and examples

* Modified example for `augment()` so it runs faster
* Reduced size of `divorces` dataset


# bage 0.7.2

## Changes to interface

* Added first data model. New function is `set_datamod_outcome_rr3()`,
  which deals with the case where the outcome variable has been
  randomly rounded to base 3.
* `augment()` now creates a new version of the outcome variable if (i)
  the outcome variable has `NA`s, or (ii) a data model is being
  applied to the outcome variable. The name of the new variable is
  created by added a `.` to the start of the name of the outcome
  variable.
* A help page summarising available data models

# bage 0.7.1

## Changes to interface

* There are now three choices for the `standardization` argument:
  `"terms"`, `"anova"`, and `"none"`. With `"terms"`, all effects,
  plus assoicated SVD coefficients, and trend, cyclical, and
  seasonal terms, are centered independently. With `"anova"`, the type
  of standardization descibed in Section 15.6 of Gelman et al (2014)
  Bayesian Data Analysis, is applied to the effects.


# bage 0.7.0 

## Changes to calculations

* Further simplification of standardization, but likely in future
  to split into two types of standardization: one that gives an 
  ANOVA-style decomposition of effects, and one that helps with
  understanding the dynamics of each term.
  
## Changes to infrastructure

* Added Makevars file.

## Changes to documentation

* Stopped referring to second-order walks as equivalent to random
  walks with drift. (A second-order random walk differs from a random
  walk in that the implied drift term in a second-order random walk
  can vary over time.)


# bage 0.6.3

## Changes to calculations

* Changed standardization of forecasts so that forecasts are
  standardized along the 'along' dimension by choosing the values that
  makes them consistent with time trends in the estimation period, and
  then standardizing within each value of the along dimensions.



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
