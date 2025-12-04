# Changelog

## bage 0.10.3

### Bug fix

- Fixed bug in “inner-outer” calculation method reported by Daniel
  Timothy (issue
  [\#106](https://github.com/bayesiandemography/bage/issues/106)). The
  calculations were doing an indirect check of whether the user has
  specified an offset, where it would have been better to have done a
  direct check, leading to failure in some edge cases.

## bage 0.10.3

### Articles

- Tidying of package website
- Tidying of description of data model in Mathematical Details article.

## bage 0.10.2

CRAN release: 2025-11-19

### Bug fix

- A test was leaving a file behind in the R_USER_CACHE_DIR. Problem
  fixed.

### vignettes

- Removed numbers from titles, and used pkgdown yaml, rather than
  numbers in front of files, to control the order of articles in the
  website.

## bage 0.10.1

CRAN release: 2025-11-17

### vignettes

- All but the first vignette have been changed to ‘articles’, so that
  they appear on the pkgdown-derived website on GitHub, but are not part
  of the package itself. Rebuilding the vignettes was taking too long,
  and checking times were exceeding CRAN limits.

### datasets

- Fixed bug in `prt_deaths` where deaths and population datasets had not
  merged properly.
- Fixed bug in `usa_deaths`, where the deaths column had class `"ts"`
  rather than `"numeric"`.
- Added a \`datasets’ overview page.

## bage 0.10.0

### Extending facilities for scaled SVD priors

- Added new scaled SVDs `CSA`, `HIMD_R`, `HIMD_P1`, `HIMD_P5`, `WMD_C`,
  `WMD_E`, for use in modeling school attendance, internal migration,
  and marriage. Also added help page for all scaled SVDs.
- Function
  [`ssvd()`](https://bayesiandemography.github.io/bage/reference/ssvd.md),
  for creating scaled SVDs is now exported. (Previously it was an
  internal function.)
- Version information has been added to scaled SVDs. Functions for
  creating SVD-based priors, such as \[SVD()\] and \[SVD_AR()\] now
  include an argument `v` that can be used to specify a version.

### Changes to interface

- [`generate()`](https://generics.r-lib.org/reference/generate.html)
  method for scaled SVDs now use the term `sex` rather than`sexgender`
  in results, for consistency with
  [`components()`](https://generics.r-lib.org/reference/components.html).
- [`generate()`](https://generics.r-lib.org/reference/generate.html) and
  [`components()`](https://generics.r-lib.org/reference/components.html)
  methods for scaled SVDs now default to showing results disaggregated
  by sex/gender. Previously the default was to show results for the
  total population.

## bage 0.9.10

### Changes to internal calculations

- Covariates (as created by
  [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
  now use sparse rather than dense matrices, which has significantly
  speeded up computations, and reduced memory usage, for models with
  categorical covariates.
- Internal calculations for post-processing results from TMB have been
  refactored to speed them up, and reduce memory usage. (More outputs
  are built piece-by-piece rather than in one go.) Some large models now
  run significantly faster, and models that previously could not be
  fitted because of memory limitations now can.

## bage 0.9.9

### Changes to model behavior

- Up to version 0.9.8,
  [`fit()`](https://generics.r-lib.org/reference/fit.html) silently
  aggregated rows with duplicated values for the predictors (ie the
  variables to the right side of the `~` in the model formula. From
  0.9.9 this aggregation only occurs with Poisson models where
  dispersion has been set to 0, or with Normal models. Even in these
  cases, aggregation no longer happens if the model includes a data
  model. In cases where aggregation would previously have occurred,
  [`fit()`](https://generics.r-lib.org/reference/fit.html) warns users
  about the behavior change. Thank you to Salah Merad for pointing out
  that the aggregation was leading to unexpected results.

### Changes to interface

- The `"inner-outer"` fitting method can now be used with models that
  include covariates.
- A function called
  [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  has been added, which returns prior or posterior values of the `disp`
  parameters. (Previously the only ways to obtain values of `disp` was
  to use function
  [`components()`](https://generics.r-lib.org/reference/components.html).)

## bage 0.9.8

CRAN release: 2025-10-16

### Changes to internal calculations

- The update of the TMB to version 1.9.18 appears to have changed
  outputs, which caused a test involving replicate data to fail. The
  test has been make more robust, by generating more draws.

### Bug fixes

- Function
  [`unfit()`](https://bayesiandemography.github.io/bage/reference/unfit.md)
  was not previously removing information on the `"inner-outer"` fitting
  method, making it appear that a model had been fit using this method,
  when it had in fact been re-fit using the standard method. This has
  been corrected.

## bage 0.9.7

CRAN release: 2025-10-07

### Changes to internal calculations

- The process for carrying out the Cholesky factorization and then
  drawing from the multivariate normal has been made more robust. When
  carrying out the Cholesky decomposition, small values are added to the
  diagonal at each failure, up to a limit that can be controlled by the
  user. Fallbacks (eg to using dense matrix calculations) have also been
  added to the function for generating multiple draws.

## bage 0.9.6

CRAN release: 2025-10-01

### Data models

- Added data models, also known as measurement error models. The main
  user-visible functions are
  [`set_datamod_exposure()`](https://bayesiandemography.github.io/bage/reference/set_datamod_exposure.md),
  [`set_datamod_miscount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_miscount.md),
  [`set_datamod_noise()`](https://bayesiandemography.github.io/bage/reference/set_datamod_noise.md),
  `set_datamod_over()`, and `set_datamod_under()`. In introduction to
  data models is given in vignette 10.

### Interface

- Extra checking of inputs for model constructor functions, eg checking
  that factors have at least two levels.

### Changes to internal calculations

- Improved efficiency of some C++ code in the TMB template, through
  greater use of call by reference.

## bage 0.9.5

### Interface

- Added method
  [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.html)
  for querying the `n_draw` value of a model object.
- Deprecated the use of formulas for specifying exposure, size, or
  weights. Formulas complicate measurement error models and forecasts.
  Formulas will be completely removed in future.
- In
  [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md),
  changed the default for `condition_on` to `"fitted"` in cases where
  dispersion is zero, since the `"expected"` option is not permitted in
  these cases.
- Extra checking of inputs for model constructor functions, eg checking
  that factors have at least two levels.

### Documentation

- Extended documentation of
  [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)
  prior in Mathematical Details vignette, and on online help, to
  describe case where `s = 0`. Also updated description of prior in
  online help to match current implementation.

### Bug fixes

- Fixed bug in RR3, where values for rates were being calculated from
  confidentialized values of the outcome variable, rather than (imputed)
  true values.
- Fixed bug in aggregation of exposure for Poisson where `exposure = 1`,
  and aggregation of weights in normal models where `weights = 1`.

## bage 0.9.4

CRAN release: 2025-07-20

### Interface

- Modified help for
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  to clarify prior for dispersion. Added default value (of 1) to
  [`set_disp()`](https://bayesiandemography.github.io/bage/reference/set_disp.md).
  This does not affect behaviour, but is a bit clearer for users.
  ([\#94](https://github.com/bayesiandemography/bage/issues/94))
- In
  [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md),
  added paragraph in online documentation to note that calling
  [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
  on a model that already has covariates deletes the existing
  covariates. Also added a warning message to
  [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
  when covariates being overwritten.
  ([\#95](https://github.com/bayesiandemography/bage/issues/95)).
- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  emits message (not warning) if one or more rates are suspiciously high
  (which is often a sympton of inaccurate exposures.)
  ([\#96](https://github.com/bayesiandemography/bage/issues/96))
- Added a guarded version of
  [`rpois_rvec()`](https://bayesiandemography.github.io/rvec/reference/dpois_rvec.html),
  which sets x\[i\] equal to lambda\[i\] for lambda\[i\] \> 1e8. This
  avoids numeric problems which can lead to valgrind errors. The guarded
  version is called by
  [`augment()`](https://generics.r-lib.org/reference/augment.html) and
  by
  [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md).
  The user is warned when the threshold of 1e8 is exceeded.

### Bug fix

- Fixed bug in
  [`set_n_draw()`](https://bayesiandemography.github.io/bage/reference/set_n_draw.md),
  which was failing to thin covariate coefficient draws.
  ([\#97](https://github.com/bayesiandemography/bage/issues/97))

## bage 0.9.3

### Interface

- In the online help for
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  and in the Mathematical Details vignette (vignette 2), give a new
  parameterisation of the
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  model, expressed on the original scale, not the transformed scale (and
  closer to the paramterisation used by
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
  and
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md).)

### Bug fix

- Modify the `bage_mod_norm` method for
  [`replicate_data()`](https://bayesiandemography.github.io/bage/reference/replicate_data.md)
  so that it returns results on the original scale.
- Modify the `bage_mod_norm` methods for the helper functions for
  [`augment()`](https://generics.r-lib.org/reference/augment.html) so
  that they properly incorporate weights.

## bage 0.9.2

### Interface

- Added `original_scale` argument to
  [`components()`](https://generics.r-lib.org/reference/components.html),
  to be used with normal models. Also added message remining users that,
  with normal models, components were on a log scale
  ([\#88](https://github.com/bayesiandemography/bage/issues/88))
- Added more information on progress.
- Tidied printing of model objects.
- Added new checks for outcome variable: no `NaN` and no `Inf`
  permitted.

### Bug fixes

- Fixed bug introducted when `fit_default()` refactored in 0.9.1. Bug
  meant that when optimizer switched from
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) to
  [`optim()`](https://rdrr.io/r/stats/optim.html) on non-convergence,
  [`optim()`](https://rdrr.io/r/stats/optim.html) was not starting from
  old parameter values.

### Documentation

- Added entry for
  [`RW2_Infant()`](https://bayesiandemography.github.io/bage/reference/RW2_Infant.md)
  to priors table. Hat-tip to Luke Morris for noticing that entry was
  missing.
  ([\#87](https://github.com/bayesiandemography/bage/issues/87))

## bage 0.9.1

### Changes to interface

- Added function
  [`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md).
  Models can now include covariates. Covariates are predictors other
  than the cross-classifying dimensions such as age, sex, or time –
  though covariates can be formed from these dimensions.
- Added variables `gdp_pc_2023` and `dens_2020` to dataset `kor_births`.
- Added `prt_deaths` dataset.
- Added
  [`set_seeds()`](https://bayesiandemography.github.io/bage/reference/set_seeds.md)
  function, allowing users to reset random seeds (though this would be
  uncommon in normal use.)

### Changes to documentation

- Added covariates vignette.

### Changes to internal calculations

- More careful handling of `NA`s in offset and predictor variables.

## bage 0.9.0

CRAN release: 2025-01-08

- From 0.9.0 onwards we will use a formal deprecation make any breaking
  changes
- Tweaks to printing of `"bage_mod"` objects
- Started vignette replicating analyses from the book *Bayesian
  Demographic Estimation and Forecasting*

## bage 0.8.6

### Changes to interface

- Added `"multi"` option for `optimizer` argument to
  [`fit()`](https://generics.r-lib.org/reference/fit.html). With
  `"multi"`, the
  [`fit()`](https://generics.r-lib.org/reference/fit.html) function
  first tries [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) and if
  that fails switches to [`optim()`](https://rdrr.io/r/stats/optim.html)
  with method `"BFGS"`.
- Added a warning if the calculations do not converge
- Modified the printout for `"bage_mod"` objects to show the time spent
  by [`TMB::sdreport`](https://rdrr.io/pkg/TMB/man/sdreport.html) rather
  than the time spent by drawing from the multivariate normal (which,
  since **bage** started using **sparseMVN**, is very short).
- Finished vignette 1.
- Added covariates
- Added `gdp_pc_2023` and `dens_2020` variables to `kor_births`

## bage 0.8.5

### Changes to internal calculations

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  excludes comparisons of `"hyper"` parameters (eg standard deviations)
  if the simulation model and estimation model use different priors with
  different classes for that term. For instance if the simulation model
  uses a
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
  prior for age and the estimation model uses a
  [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)
  prior for age, then
  [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  will not report on the standard deviation parameter for age.
- Added warning to documentation for
  [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  stating that the interface is still under development.

## bage 0.8.4

### Changes to interface

- Changed `zero_sum` argument to `con` (short for “constraint”).
  `con = "none"` corresponds to `zero_sum = FALSE`, and `con = "by"`
  corresponds to `zero_sum = TRUE`. Additional options will be added in
  future.
- Added `sd` argument to
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
  [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md),
  [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
  and
  [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md).
  The initial value of the random walks are drawn from a `N(0, sd^2)`
  prior. By default `sd` equals `1`, but it can be set to
  0.  
- Loosened restrictions in
  [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  and
  [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)
  priors so that the coefficients no longer need to be consistent with
  stationarity. The Stan user guide recommends against building in
  stationarity:
  <https://mc-stan.org/docs/stan-users-guide/time-series.html#autoregressive.section>
  Also, testing for stationarity often causes numerical problems.

## bage 0.8.3

### Changes to internal calculations

- Corrected bug in forecasting of
  [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  and
  [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)
  priors.
- Modified prior for coefficients of
  [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)
  and
  [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)
  priors, so that partical autocorrelation function (PACF), rather than
  the AR coefficients themselves, are restricted to (-1, 1). Restricting
  the PACF to (-1,1) ensures stationarity.

## bage 0.8.2

### Changes to interface

- Check to see that model object was created using current version of
  ‘bage’.
- Added `optimizer` argument to
  [`fit()`](https://generics.r-lib.org/reference/fit.html), giving
  choice between three ways of optimizing
- Modifed behaviour of `quiet` argument to
  [`fit()`](https://generics.r-lib.org/reference/fit.html) so that when
  it is `TRUE`, trace output from the optimizer is shown.
- Added `start_oldpar` argument to
  [`fit()`](https://generics.r-lib.org/reference/fit.html), to allow
  calculations to be restarted on a model that has already been fitted.
- Modified printing of `"bage_mod"` object.

## bage 0.8.1

### Changes to interface

- Modified construction of `computations` part of models so that it
  works with models fitted using the “inner-outer” method. Extended the
  [`print()`](https://rdrr.io/r/base/print.html) method for `"bage_mod"`
  so that it shows extra output for models fitted using the
  `"inner-outer"` method.

## bage 0.8.0

### Changes to interface

- Added ‘along’ column to tidy and print methods for `"bage_mod"`
  objects. (Thank you to Andrew Taylor for suggesting this.)
- Allow `s = 0` in
  [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)
  priors
- Added `zero_sum` argument to priors with an `along` dimension. When
  `zero_sum` is `TRUE`, values for each combination of a `by` variable
  and the `along` variable are constrained to sum to zero. This can
  allow better identification of higher-level terms in complicated
  models. It can also slow computations, and has virtually no effect on
  estimates of the lowest-level rates, probabilities, and means.
- Removed post-estimation standardization. We now rely on explicit
  constraints instead to give interpretable values for main effects and
  interactions.
- Added
  [`RW2_Infant()`](https://bayesiandemography.github.io/bage/reference/RW2_Infant.md)
  prior for modelling age-patterns of mortality rates.
- The `s_seas` parameter in
  [`RW_Seas()`](https://bayesiandemography.github.io/bage/reference/RW_Seas.md)
  and
  [`RW2_Seas()`](https://bayesiandemography.github.io/bage/reference/RW2_Seas.md)
  now defaults to 0, rather than 1, so that seasonal effects are by
  default fixed over time rather than varying. Using varying seasonal
  effects can greatly increase computation times.
- Moved **rvec** from Imports to Depends, so that it loads when **bage**
  is loaded. Manipulating results from **bage** models without **rvec**
  loaded can lead to strange errors.
- Added information on computations to printout from fitted model
  objects.
- Added function
  [`computations()`](https://bayesiandemography.github.io/bage/reference/computations.md),
  which can be used to extract this information from fitted model
  objects.
- Added `quiet` argument to
  [`fit()`](https://generics.r-lib.org/reference/fit.html). When `quiet`
  is `TRUE` (the default), warnings generated by
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) are suppressed.
  (These warnings are virtually always about NAs early in the
  optimization process and are nothing to worry about.)

### Changes to internal calculations

- Removed some unnecessary coercion of sparse matrices to dense matrices
  (which could sometimes cause memory problems)
- Added extra constraints to some priors - eg the first element of
  random walks is now zero. This often (but not always) helps make raw
  estimates of main effects and interactions more interpretable, and can
  speed up computations slightly.
- In the normal model, we now rescale the weights so that they have a
  mean of 1. This allows us to use the same default prior for dispersion
  (an exponential prior with mean 1), regardless of the original
  weights. The rescaling of the weights affects the estimated value for
  dispersion, but does not affect the estimates for any other
  parameters.
- Generation of posterior sample now using fast methods from package
  **sparseMVN** where possible.

## bage 0.7.8

### Datasets

- Added `HFD`, a scaled SVD object holding data from the Human Fertiltiy
  Database
- Changed names of data objects:
  - `deaths` –\> `isl_deaths`
  - `expenditure` –\> `nld_expenditure`
  - `divorces` –\> `nzl_divorces`
  - `injuries` –\> `nzl_injuries`
  - `us_acc_deaths` –\> `usa_deaths`
- Added new data object `kor_births`, births in South Korea

## bage 0.7.7

### Bug fixes

- [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)
  now works on fitted models. Thank you to Ollie Pike for pointing out
  that it previously did not.
- Removed redundant levels from `age` variable in `divorces`.

### Changes to internal calculations

- Removed internal **bage** function
  [`rr3()`](https://bayesiandemography.github.io/poputils/reference/rr3.html).
  Call **poputils** function
  [`rr3()`](https://bayesiandemography.github.io/poputils/reference/rr3.html)
  instead.

## bage 0.7.6

### Changes to interface

- Added `newdata` argument to
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html).
- Added minimum version numbers for **rvec** and **poputils**.

## bage 0.7.5.1

### Bug fixes

- Fixed bug in code for simulating from
  [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)
  and
  [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)
  priors.

## bage 0.7.5

### Changes to interface

- Added arguments `method` and `vars_inner` to
  [`fit()`](https://generics.r-lib.org/reference/fit.html). When
  `method` is `"standard"` (the default)
  [`fit()`](https://generics.r-lib.org/reference/fit.html) uses the
  existing calculation methods. When `method` is `"inner-outer"`,
  [`fit()`](https://generics.r-lib.org/reference/fit.html) uses a new,
  somewhat experimental calculation method that involves fitting an
  inner model using a subset of variables, and then an outer model using
  the remaining variables. With big datasets, `"inner-outer"` can be
  faster, and use less memory, but give very similar results.
- Added information on numbers of parameters, and standard deviations to
  output for print. Thank you to Duncan Elliot for suggesting printing
  numbers of parameters.

### Changes to calculations

- [`fit()`](https://generics.r-lib.org/reference/fit.html) now
  internally aggregates input data before fitting, so that cells with
  the same combinations of predictor variables are combined. This
  increases speed and reduces memory usage.

### Changes to documentation

- Added help for `print.bage_mod`

## bage 0.7.4

CRAN release: 2024-08-28

### Changes to interface

- Function
  [`ssvd()`](https://bayesiandemography.github.io/bage/reference/ssvd.md)
  no longer exported. Will export once package **bssvd** matures.
- **bage** released on to CRAN

## bage 0.7.3

### Changes to data and examples

- Modified example for
  [`augment()`](https://generics.r-lib.org/reference/augment.html) so it
  runs faster
- Reduced size of `divorces` dataset

## bage 0.7.2

### Changes to interface

- Added first data model. New function is
  [`set_datamod_outcome_rr3()`](https://bayesiandemography.github.io/bage/reference/set_datamod_outcome_rr3.md),
  which deals with the case where the outcome variable has been randomly
  rounded to base 3.
- [`augment()`](https://generics.r-lib.org/reference/augment.html) now
  creates a new version of the outcome variable if (i) the outcome
  variable has `NA`s, or (ii) a data model is being applied to the
  outcome variable. The name of the new variable is created by added a
  `.` to the start of the name of the outcome variable.
- A help page summarising available data models

## bage 0.7.1

### Changes to interface

- There are now three choices for the `standardization` argument:
  `"terms"`, `"anova"`, and `"none"`. With `"terms"`, all effects, plus
  assoicated SVD coefficients, and trend, cyclical, and seasonal terms,
  are centered independently. With `"anova"`, the type of
  standardization descibed in Section 15.6 of Gelman et al (2014)
  Bayesian Data Analysis, is applied to the effects.

## bage 0.7.0

### Changes to calculations

- Further simplification of standardization, but likely in future to
  split into two types of standardization: one that gives an ANOVA-style
  decomposition of effects, and one that helps with understanding the
  dynamics of each term.

### Changes to infrastructure

- Added Makevars file.

### Changes to documentation

- Stopped referring to second-order walks as equivalent to random walks
  with drift. (A second-order random walk differs from a random walk in
  that the implied drift term in a second-order random walk can vary
  over time.)

## bage 0.6.3

### Changes to calculations

- Changed standardization of forecasts so that forecasts are
  standardized along the ‘along’ dimension by choosing the values that
  makes them consistent with time trends in the estimation period, and
  then standardizing within each value of the along dimensions.

## bage 0.6.2

### Changes to interface

- Removed `SVDS()`, `SVDS_AR()`, `SVDS_AR1()`, `SVDS_RW()`, and
  `SVDS_RW2()` priors. Added `indep` argument to corresponding `SVD`
  priors. `SVD` priors now choose between ‘total’, ‘independent’ and
  ‘joint’ models based on (1) the value of `indep` argument, (2) the
  value of `var_sexgender` and the name of the term.

### Changes to data

- Object `HMD` now contains 5 components, rather than 10.

## bage 0.6.1

### Changes to calculations

- Fixed problems with standardization of forecast
- Added an intercept term to
  [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)
  and `LinAR()` priors

## bage 0.6.0

### Issues

- Standardization of forecasts not working correctly.

### Changes to interface

- Added priors
  [`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  `SVDS_AR()`,
  [`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  `SVDS_AR1()`,
  [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  `SVDS_RW()`,
  [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  `SVDS_RW2()`

### Internal calculations

- Changed values that are stored in object: removed `draws_linpred`,
  added `draws_effectfree`, `draws_spline`, and `draws_svd`.
  Modified/added downstream functions.
- Calculation of ‘along_by’ and ‘agesex’ matrices pushed downwards into
  lower-level functions.

## bage 0.5.1

### Changes to interface

- Moved HMD code to package **bssvd**.

## bage 0.5.0

### Changes to interface

- Combined interaction (eg ELin) and main effect (eg Lin) versions of
  priors
- Removed function `compose_time()`
- Added priors RWSeas and RW2Seas
- Improved
  [`report_sim()`](https://bayesiandemography.github.io/bage/reference/report_sim.md)

## bage 0.4.2

### Changes to interface

- Tidying of online help (not yet complete).

## bage 0.4.1

### New functions

- Added ‘bage_ssvd’ method for
  [`components()`](https://generics.r-lib.org/reference/components.html).

### Changes to interface

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  method for `bage_mod` objects now calculated value for `.fitted` in
  cases where the outcome or exposure/size is NA, rather than setting
  the value of `.fitted` to `NA`.

### Internal calculations

- Standardization of effects only done if
  [`components()`](https://generics.r-lib.org/reference/components.html)
  is called.
  [`augment()`](https://generics.r-lib.org/reference/augment.html) uses
  the linear predictor (which does not need standardization.)
- Internally, draws for the linear predictor, the hyper-parameters and
  (if included in model) `disp` are stored, rather than the full
  standardized components.
- Standardization algorithm repeats up to 100 times, or until all
  residuals are less than 0.0001.
- With the new configuration, calculations for large matrices that
  previously failed with error message “Internal error: Final residual
  not 0” are now running.

### Simulations

- When drawing from the prior, the intercept is always set to 0. Terms
  with SVD or Known priors are not touched. All other terms are
  centered.

## bage 0.4.0

### Changes to back-end for SVD priors

- Move most functions for creating ‘bage_ssvd’ objects to package
  ‘bssvd’.
- Allowed number of components of a ‘bage_ssvd’ object to differ from
  10. 

### Bug fixes

- Corrected error in calculation of logit in `ssvd_comp()`.

## bage 0.3.2

### New functions

- [`forecast.bage_mod()`](https://bayesiandemography.github.io/bage/reference/forecast.bage_mod.md)
  Forecasting. Interface not yet finalised.

### Bug fixes

- Corrected error in C++ template for Lin and ELin priors (due to use of
  integer arithmetic.)

## bage 0.2.2

### New functions

- [`generate.bage_ssvd()`](https://bayesiandemography.github.io/bage/reference/generate.bage_ssvd.md)
  Generate random age-sex profiles from SVD.

### Bug fixes

- Internal function `draw_vals_effect_mod()` was malfunctioning on
  models that contained SVD priors.
