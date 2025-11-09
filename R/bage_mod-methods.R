## methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Extract Data and Modeled Values
#'
#' Extract data and rates, probabilities, or means
#' from a model object.
#' The return value consists of the original
#' data and one or more columns of modeled values.
#'
#' @section Fitted vs unfitted models:
#'
#' `augment()` is typically called on a [fitted][fit()]
#' model. In this case, the modeled values are
#' draws from the joint posterior distribution for rates,
#' probabilities, or means.
#'
#' `augment()` can, however, be called on an
#' unfitted model. In this case, the modeled values
#' are draws from the joint prior distribution.
#' In other words, the modeled values are informed by
#' model priors, and by values for `exposure`, `size`, or `weights`,
#' but not by observed outcomes.
#'
#' @section Imputed values for outcome variable:
#'
#' `augment()` automatically imputes any missing
#' values for the outcome variable. If outcome variable
#' `var` has one or more `NA`s, then `augment`
#' creates a variable `.var`
#' holding original and imputed values.
#'
#' @section Data model for outcome variable:
#'
#' If the overall model includes a data model
#' for the outcome variable `var`,
#' then `augment()` creates a new variable `.var` containing
#' estimates of the true value for the outcome.
#'
#' @param x Object of class `"bage_mod"`, typically
#' created with [mod_pois()], [mod_binom()],
#' or [mod_norm()].
#' @param quiet Whether to suppress messages.
#' Default is `FALSE`.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns
#' A [tibble][tibble::tibble-package], with the original
#' data plus one or more of the following columns:
#'
#' - `.<outcome>` Corrected or extended version of
#'   the outcome variable, in applications where the
#'   outcome variable has missing values, or a data model
#'   is being used.
#' - `.observed` 'Direct' estimates of rates or
#'   probabilities, ie counts divided by exposure or size
#'   (in Poisson and binomial models.)
#' - `.fitted` Draws of rates, probabilities,
#'   or means.
#' - `.expected` Draws of expected values for
#'   rates or probabilities (in Poisson
#'   that include exposure, or in binomial models.)
#'
#' Uncertain quantities are represented using
#' [rvecs][rvec::rvec()].
#'
#' @seealso
#' - [components()] Extract values for hyper-parameters
#' - [dispersion()] Extract values for dispersion
#' - [tidy()] Short summary of a model
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit()] Fit a model
#' - [is_fitted()] See if a model has been fitted
#' - [unfit()] Reset a model
#' - [datamods] Overview of data models implemented in **bage**
#'
#' @examples
#' set.seed(0)
#'
#' ## specify model
#' mod <- mod_pois(divorces ~ age + sex + time,
#'                 data = nzl_divorces,
#'                 exposure = population) |>
#'   set_n_draw(n_draw = 100) ## smaller sample, so 'augment' faster
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#'
#' ## draw from the posterior distribution
#' mod |>
#'   augment()
#'
#' ## insert a missing value into outcome variable
#' divorces_missing <- nzl_divorces
#' divorces_missing$divorces[1] <- NA
#'
#' ## fitting model and calling 'augument'
#' ## creates a new variable called '.divorces'
#' ## holding observed and imputed values
#' mod_pois(divorces ~ age + sex + time,
#'          data = divorces_missing,
#'          exposure = population) |>
#'   fit() |>
#'   augment()
#'
#' ## specifying a data model for the
#' ## original data also leads to a new
#' ## variable called '.divorces'
#' mod_pois(divorces ~ age + sex + time,
#'          data = nzl_divorces,
#'          exposure = population) |>
#'   set_datamod_outcome_rr3() |>
#'   fit() |>
#'   augment()
#' @export
augment.bage_mod <- function(x,
                             quiet = FALSE,
                             ...) {
  check_old_version(x = x, nm_x = "x")
  is_fitted <- is_fitted(x)
  check_flag(x = quiet, nm_x = "quiet")
  check_has_no_dots(...)
  if (is_fitted)
    ans <- draw_vals_augment_fitted(mod = x, quiet = quiet)
  else {
    if (!quiet)
      cli::cli_alert_info("Model not fitted, so drawing values straight from prior distribution.")
    ans <- draw_vals_augment_unfitted(mod = x, quiet = quiet)
  }
  ans
}


## 'can_aggregate' ------------------------------------------------------------

## HAS_TESTS
#' Test Whether a Model Can Aggregate Across Duplicate
#' Values for Predictors
#'
#' @param mod An object of class `"bage_mod"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
can_aggregate <- function(mod) {
  UseMethod("can_aggregate")
}

## HAS_TESTS
#' @export
can_aggregate.bage_mod_pois <- function(mod) {
  disp_fixed <- isTRUE(all.equal(mod$mean_disp, 0))
  disp_fixed && !has_datamod(mod)
}

## HAS_TESTS
#' @export
can_aggregate.bage_mod_binom <- function(mod) {
  FALSE
}

## HAS_TESTS
#' @export
can_aggregate.bage_mod_norm <- function(mod) {
  !has_datamod(mod)
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract Values for Hyper-Parameters
#'
#' @description
#'
#' Extract values for hyper-parameters
#' from a model object. Hyper-parameters include
#'
#' - main effects and interactions,
#' - dispersion,
#' - trends, seasonal effects, errors,
#' - SVD, spline, and covariate coefficients,
#' - standard deviations, correlation coefficients.
#'
#' @section Fitted vs unfitted models:
#'
#' `components()` is typically called on a [fitted][fit()]
#' model. In this case, the values returned are
#' draws from the joint posterior distribution for the
#' hyper-parameters in the model.
#'
#' `components()` can, however, be called on an
#' unfitted model. In this case, the values returned
#' are draws from the joint *prior* distribution.
#' In other words, the values incorporate
#' model priors, and any `exposure`, `size`, or `weights`
#' argument, but not observed outcomes.
#'
#' @section Scaling and Normal models:
#'
#' Internally, models created with [mod_norm()]
#' are fitted using transformed versions of the
#' outcome and weights variables. By default, when `components()`
#' is used with these models,
#' it returns values for `.fitted`
#' that are based on the transformed versions.
#' To instead obtain values for `"effect"`, `"trend"`, `"season"`,
#' `"error"` and `"disp"` that are based on the
#' untransformed versions,
#' set `original_scale` to `TRUE`.
#'
#' @inheritParams augment.bage_mod
#' @param object Object of class `"bage_mod"`,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param original_scale Whether values for
#' `"effect"`, `"trend"`, `"season"`,
#' `"error"` and `"disp"` components from
#' a [normal][mod_norm()] model are on the original
#' scale or the transformed scale. Default is `FALSE`.
#'
#' @returns
#' A [tibble][tibble::tibble-package]
#' with four columns columns:
#'
#' The return value contains the following columns:
#'
#' - `term` Model term that the hyper-parameter belongs to.
#' - `component` Component within term.
#' - `level` Element within component .
#' - `.fitted` An [rvec][rvec::rvec()] containing
#'   draws from the posterior distribution.
#'
#' @seealso
#' - [augment()] Extract values for rates,
#'   means, or probabilities,
#'   together with original data
#' - [dispersion()] Extract values for dispersion
#' - [tidy()] Extract a one-line summary of a model
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit()] Fit a model
#' - [is_fitted()] See if a model has been fitted
#' - [unfit()] Reset a model
#'
#' @examples
#' set.seed(0)
#'
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#'
#' ## extract prior distribution
#' ## of hyper-parameters
#' mod |>
#'   components()
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#'
#' ## extract posterior distribution
#' ## of hyper-parameters
#' mod |>
#'   components()
#'
#' ## fit normal model
#' mod <- mod_norm(value ~ age * diag + year,
#'                 data = nld_expenditure,
#'                 weights = 1) |>
#'   fit()
#'
#' ## dispersion (= standard deviation in normal model)
#' ## on the transformed scale
#' mod |>
#'   components() |>
#'   subset(component == "disp")
#'
#' ## disperson on the original scale
#' mod |>
#'   components(original_scale = TRUE) |>
#'   subset(component == "disp")
#' @export
components.bage_mod <- function(object,
                                quiet = FALSE,
                                original_scale = FALSE,
                                ...) {
  check_old_version(x = object, nm_x = "object")
  check_flag(x = quiet, nm_x = "quiet")
  check_original_scale(original_scale = original_scale, mod = object)
  is_fitted <- is_fitted(object)
  is_norm <- inherits(object, "bage_mod_norm")
  check_has_no_dots(...)
  if (!quiet && is_norm && !original_scale)
    cli::cli_alert_info(paste("Values for {.arg .fitted} from",
                              "{.fun components} are on a transformed scale.",
                              "See the documentation for {.fun mod_norm} and",
                              "{.fun components} for details."))
  if (is_fitted)
    ans <- draw_vals_components_fitted(object)
  else {
    if (!quiet)
      cli::cli_alert_info("Model not fitted, so values drawn straight from prior distribution.")
    n_draw <- object$n_draw
    ans <- draw_vals_components_unfitted(mod = object, n_sim = n_draw)
  }
  if (is_norm && original_scale)
    ans <- rescale_components(components = ans, mod = object)
  ans <- sort_components(components = ans, mod = object)
  ans
}


## 'computations' -------------------------------------------------------------

#' Information on Computations Performed During Model Fitting
#'
#' Get information on computations performed by function [fit()].
#' The information includes the total time used for fitting, and
#' the time used for two particular tasks that can be slow:
#' running the optimizer [stats::nlminb()],
#' and drawing from the multivariate normal returned
#' by the TMB. It also includes values returned by the optimizer:
#' the number of iterations needed, and messages about convergence.
#'
#' @param object A fitted object of class `"bage_mod"`.
#'
#' @returns A [tibble][tibble::tibble-package] with the following
#' variables:
#' - `time_total` Seconds used for whole fitting process.
#' - `time_max` Seconds used for optimisiation.
#' - `time_draw` Seconds used by function [TMB::sdreport()].
#' - `iter` Number of iterations required for optimization.
#' - `message` Message about convergence returned by optimizer.
#'
#' @seealso
#' - [mod_pois()],[mod_binom()],[mod_norm()] Specify a model
#' - [fit()] Fit a model
#' - [tidy()] Summarise a model
#' - [set_n_draw()] Specify number of posterior draws
#'
#' @examples
#' mod <- mod_pois(divorces ~ age + sex + time,
#'                 data = nzl_divorces,
#'                 exposure = population) |>
#'   fit()
#'
#' computations(mod)
#' @export
computations <- function(object) {
  UseMethod("computations")
}

#' @export
computations.bage_mod <- function(object) {
  check_old_version(x = object, nm_x = "object")
  if (is_fitted(object))
    object$computations
  else {
    cli::cli_alert_warning("Model not fitted.")
    NULL
  }
}


## HAS_TESTS
#' Extract Values for Dispersion
#'
#' Extract values for the 'dispersion'
#' parameter from a model object.
#'
#' @section Fitted vs unfitted models:
#'
#' `dispersion()` is typically called on a [fitted][fit()]
#' model. In this case, the values for dispersion are
#' draws from the posterior distribution.
#' `dispersion()` can, however, be called on an
#' unfitted model. In this case, the values
#' are drawn from the prior distribution.
#' 
#' @section Scaling and Normal models:
#'
#' Internally, models created with [mod_norm()]
#' are fitted using transformed versions of the
#' outcome and weights variables. By default, when `dispersion()`
#' is used with these models,
#' it returns values on the transformed scale.
#' To instead obtain values on the untransformed
#' scale, set `original_scale` to `TRUE`.
#'
#' @inheritParams augment.bage_mod
#' @param object Object of class `"bage_mod"`,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param original_scale Whether values for
#' disperson are on the original
#' scale or the transformed scale.
#' Default is `FALSE`.
#'
#' @returns An [rvec][rvec::rvec]
#' (or `NULL` if the model does not
#' include a dispersion parameter.)
#'
#' @seealso
#' - [components()] Extract values for hyper-parameters,
#'   including dispersion
#' - [set_disp()] Specify a prior for dispersion
#'
#' @examples
#' set.seed(0)
#'
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#'
#' ## prior distribution
#' mod |>
#'   dispersion()
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#'
#' ## posterior distribution
#' mod |>
#'   dispersion()
#'
#' ## fit normal model
#' mod <- mod_norm(value ~ age * diag + year,
#'                 data = nld_expenditure,
#'                 weights = 1) |>
#'   fit()
#'
#' ## values on the transformed scale
#' mod |>
#'   dispersion()
#'
#' ## values on the original scale
#' mod |>
#'   dispersion(original_scale = TRUE)
#' @export
dispersion <- function(object,
                       quiet = FALSE,
                       original_scale = FALSE) {
  UseMethod("dispersion")
}

#' @export
dispersion.bage_mod <- function(object,
                                quiet = FALSE,
                                original_scale = FALSE) {
  check_old_version(x = object, nm_x = "object")
  check_flag(x = quiet, nm_x = "quiet")
  check_original_scale(original_scale = original_scale, mod = object)
  has_disp <- has_disp(object)
  is_fitted <- is_fitted(object)
  is_norm <- inherits(object, "bage_mod_norm")
  if (!quiet && is_norm && !original_scale)
    cli::cli_alert_info(paste("Values for dispersion are on a transformed scale.",
                              "See the documentation for {.fun mod_norm} and",
                              "{.fun dispersion} for details."))
  if (has_disp) {
    if (is_fitted) {
      ans <- object$draws_disp
      ans <- matrix(ans, nrow = 1L)
      ans <- rvec::rvec_dbl(ans)
    }
    else {
      n_draw <- object$n_draw
      seed_components <- object$seed_components
      seed_restore <- make_seed()
      set.seed(seed_components)
      ans <- draw_vals_disp(object, n_sim = n_draw)
      set.seed(seed_restore)
    }
    if (is_norm && original_scale) {
      outcome_sd <- object$outcome_sd
      offset_mean <- object$offset_mean
      ans <- sqrt(offset_mean) * outcome_sd * ans
    }
  }
  else
    ans <- NULL
  ans
}


## 'draw_fitted_given_outcome' ------------------------------------------------

#' Draw Values for '.fitted' when 'outcome' (and 'offset')
#' Known, and when 'disp' non-NULL
#'
#' @param mod Object of class 'bage_mod'
#' @param expected Backtransformed linear predictor. An rvec.
#' @param disp Dispersion. An rvec.
#' @param outcome Values for outcome.
#' @param offset Values for offset. 
#'
#' @returns An rvec.
#'
#' @noRd
draw_fitted_given_outcome <- function(mod,
                                      outcome,
                                      offset,
                                      expected,
                                      disp) {
  UseMethod("draw_fitted_given_outcome")
}

## HAS_TESTS
#' @export
draw_fitted_given_outcome.bage_mod_pois <- function(mod,
                                                    outcome,
                                                    offset,
                                                    expected,
                                                    disp) {
  ## reformat everything to numeric vectors of
  ## same length to deal with NAs
  ## in 'outcome' and 'offset'
  n_val <- length(expected)
  n_draw <- rvec::n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  if (rvec::is_rvec(offset))
    offset <- as.numeric(offset)
  else
    offset <- rep(offset, times = n_draw)
  expected <- as.numeric(expected)
  disp <- as.numeric(disp)
  disp <- rep(disp, each = n_val)
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  ans <- stats::rgamma(n = length(expected),
                       shape = outcome + 1 / disp,
                       rate = offset + 1 / (disp * expected))
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## HAS_TESTS
#' @export
draw_fitted_given_outcome.bage_mod_binom <- function(mod,
                                                     outcome,
                                                     offset,
                                                     expected,
                                                     disp) {
  ## reformat everything to numeric vectors of
  ## same length to deal with NAs
  ## in 'outcome' and 'offset'
  n_val <- length(expected)
  n_draw <- rvec::n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  if (rvec::is_rvec(offset))
    offset <- as.numeric(offset)
  else
    offset <- rep(offset, times = n_draw)
  expected <- as.numeric(expected)
  disp <- as.numeric(disp)
  disp <- rep(disp, each = n_val)
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  ans <- stats::rbeta(n = length(expected),
                      shape1 = outcome + expected / disp,
                      shape2 = offset - outcome + (1 - expected) / disp)
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## 'draw_vals_augment_fitted' -------------------------------------------------

#' Draw '.fitted' and Possibly '.expected' from Fitted Model
#'
#' @param mod A fitted object of class 'bage_mod'
#' @param quiet Whether to suppress messages.
#'
#' @returns A tibble
#'
#' @noRd
draw_vals_augment_fitted <- function(mod, quiet) {
  UseMethod("draw_vals_augment_fitted")
}

## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod <- function(mod, quiet) {
  ## extract values
  data <- mod$data
  dimnames_terms <- mod$dimnames_terms
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod <- mod$datamod
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_offset_data <- get_nm_offset_data(mod)
  inv_transform <- get_fun_inv_transform(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## prepare inputs
  components <- components(mod)
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  expected <- inv_transform(linpred)
  has_confidential <- has_confidential(mod)
  has_datamod <- has_datamod(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  has_missing_outcome <- anyNA(outcome)
  has_disp <- has_disp(mod)
  has_varying_offset <- has_varying_offset(mod)
  ## create return object, possibly includin '.observed'
  ans <- mod$data
  if (has_varying_offset)
    ans$.observed <- outcome / offset
  ## draw values for outcome and offset, where necessary
  if (has_confidential) {
    expected_obs <- make_expected_obs(mod = mod,
                                      components = components,
                                      expected = expected)
    disp_obs <- make_disp_obs(mod) ## vector or NULL
    sd_obs <- make_sd_obs(mod) ## vector or NULL
    outcome <- draw_outcome_obs_given_conf(confidential = confidential,
                                           nm_distn = nm_distn,
                                           outcome_conf = outcome,
                                           offset = offset,
                                           expected_obs = expected_obs,
                                           disp_obs = disp_obs,
                                           sd_obs = sd_obs)
  }
  if (has_datamod_outcome)
    outcome <- draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = nm_distn,
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp)
  if (has_datamod_offset)
    offset <- draw_offset_true_given_obs(datamod = datamod,
                                         nm_distn = nm_distn,
                                         components = components,
                                         outcome = outcome,
                                         offset_obs = offset,
                                         expected = expected)
  if (has_missing_outcome)
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
                                   offset = offset,
                                   expected = expected,
                                   disp = disp)
  ## derive '.fitted' and, in models with dispersion, '.expected'
  if (has_disp) {
    ans$.fitted <- draw_fitted_given_outcome(mod = mod,
                                             outcome = outcome,
                                             offset = offset,
                                             expected = expected,
                                             disp = disp)
    ans$.expected <- expected
  }
  else
    ans$.fitted <- expected
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## assemble and return answer
  has_modified_outcome <- (has_confidential
    || has_datamod_outcome
    || has_missing_outcome)
  if (has_modified_outcome) {
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet) {
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for",
                                "{.var {nm_outcome_data}}."))
    }
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome,
                        nm_x = nm_outcome_data_true)
  }
  if (has_datamod_offset) {
    nm_offset_data_true <- paste0(".", nm_offset_data)
    if (!quiet) {
      cli::cli_alert_info(paste("Adding variable {.var {nm_offset_data_true}}",
                                "with true values for",
                                "{.var {nm_offset_data}}."))
    }
    ans <- insert_after(df = ans,
                        nm_after = nm_offset_data,
                        x = offset,
                        nm_x = nm_offset_data_true)
  }    
  ans
}

## HAS_TESTS
## Assume no confidentialization, and no data model for offset
#' @export
draw_vals_augment_fitted.bage_mod_norm <- function(mod, quiet) {
  ## extract values
  data <- mod$data
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod <- mod$datamod
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## prepare inputs
  linpred <- make_linpred_from_stored_draws(mod = mod,
                                            point = FALSE,
                                            rows = NULL)
  disp <- get_disp(mod)
  has_datamod_outcome <- has_datamod(mod)
  has_missing_outcome <- anyNA(outcome)
  fun_orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  fun_orig_scale_offset <- get_fun_orig_scale_offset(mod)
  fun_orig_scale_disp <- get_fun_orig_scale_disp(mod)
  outcome_orig_scale <- fun_orig_scale_linpred(outcome)
  offset_orig_scale <- fun_orig_scale_offset(offset)
  expected_orig_scale <- fun_orig_scale_linpred(linpred)
  disp_orig_scale <- fun_orig_scale_disp(disp)
  ## create return object
  ans <- mod$data
  ## draw values for outcome, where necessary
  if (has_datamod_outcome)
    outcome_orig_scale <- draw_outcome_true_given_obs(
      datamod = datamod,
      nm_distn = nm_distn,
      components = NULL, # no parameters
      outcome = outcome_orig_scale,
      offset = offset_orig_scale,
      expected = expected_orig_scale,
      disp = disp_orig_scale
    )
  if (has_missing_outcome)
    outcome_orig_scale <- impute_outcome_true(
      nm_distn = nm_distn,
      outcome = outcome_orig_scale,
      offset = offset_orig_scale,
      expected = expected_orig_scale,
      disp = disp_orig_scale
    )
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## derive '.fitted', on original scale
  ans$.fitted <- expected_orig_scale
  ## assemble and return answer
  has_modified_outcome <- (has_datamod_outcome || has_missing_outcome)
  if (has_modified_outcome) {
    nm_outcome_data <- get_nm_outcome_data(mod)
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet) {
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for",
                                "{.var {nm_outcome_data}}."))
    }
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome_orig_scale,
                        nm_x = nm_outcome_data_true)
  }
  ans
}


## 'draw_vals_augment_unfitted' -----------------------------------------------

#' Draw '.fitted' and Possibly '.expected' from Unfitted Model
#'
#' @param mod Object of class 'bage_mod'
#' @param quiet Whether to suppress messages.
#'
#' @returns A tibble
#'
#' @noRd
draw_vals_augment_unfitted <- function(mod, quiet) {
  UseMethod("draw_vals_augment_unfitted")
}

## HAS_TESTS
#' @export
draw_vals_augment_unfitted.bage_mod <- function(mod, quiet) {
  ## extract values
  data <- mod$data
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod <- mod$datamod
  dimnames_terms <- mod$dimnames_terms
  n_draw <- n_draw(mod)
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_offset_data <- get_nm_offset_data(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## prepare inputs
  has_confidential <- has_confidential(mod)
  has_datamod <- has_datamod(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  has_disp <- has_disp(mod)
  ans <- mod$data
  ## draw values for components
  components <- draw_vals_components_unfitted(mod = mod,
                                              n_sim = n_draw)
  ## derive expected values
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  expected <- inv_transform(linpred)
  ## obtain values for 'fitted', and, in models with dispersion, 'disp'
  if (has_disp) {
    is_disp <- components$component == "disp"
    disp <- components$.fitted[is_disp]
    fitted <- draw_fitted(nm_distn = nm_distn,
                          expected = expected,
                          disp = disp)
  }
  else
    fitted <- expected
  ## obtain values for true outcome
  outcome <- draw_outcome_true(nm_distn = nm_distn,
                               offset = offset,
                               fitted = fitted,
                               disp = NULL)
  ## if reported outcome different from true outcome,
  ## then record true and reported outcomes;
  ## otherwise just record true outcome
  is_reported_outcome_not_true <- has_datamod_outcome || has_confidential
  if (is_reported_outcome_not_true) {
    outcome_true <- outcome
    if (has_datamod_outcome)
      outcome <- draw_outcome_obs_given_true(datamod = datamod,
                                             components = components,
                                             outcome_true = outcome,
                                             offset = offset,
                                             fitted = fitted)
    if (has_confidential)
      outcome <- draw_outcome_confidential(confidential = confidential,
                                           outcome_obs = outcome)
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome_true,
                        nm_x = nm_outcome_data_true)
  }
  else {
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome
  }
  ## if reported offset different from
  ## true offset, then record true and
  ## observed offsets
  if (has_datamod_offset) {
    offset_obs <- draw_offset_obs_given_true(datamod = datamod,
                                             components = components,
                                             offset_true = offset)
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_offset_data}}."))
    ans[[nm_offset_data]] <- offset_obs
    nm_offset_data_true <- paste0(".", nm_offset_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_offset_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_offset_data,
                        x = offset,
                        nm_x = nm_offset_data_true)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## add '.observed', '.fitted', and, in models
  ## with dispersion, '.expected', and return
  has_varying_offset <- has_varying_offset(mod)
  if (has_varying_offset)
    ans$.observed <- ans[[nm_outcome_data]] / ans[[nm_offset_data]]
  ans$.fitted <- fitted
  if (has_disp)
    ans$.expected <- expected
  ans
}


## HAS_TESTS
## Assumes that no confidentialization procedures,
## and no data models for offsets exist for Normal models
#' @export
draw_vals_augment_unfitted.bage_mod_norm <- function(mod, quiet) {
  ## extract values
  data <- mod$data
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod <- mod$datamod
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  fun_orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  fun_orig_scale_offset <- get_fun_orig_scale_offset(mod)
  fun_orig_scale_disp <- get_fun_orig_scale_disp(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## prepare inputs
  has_datamod <- has_datamod(mod)
  ans <- mod$data
  ## draw values for components
  components <- draw_vals_components_unfitted(mod = mod,
                                              n_sim = n_draw)
  ## derive linpred
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  ## obtain values for 'fitted', 'disp',
  ## and 'offset' on original scale
  is_disp <- components$component == "disp"
  disp <- components$.fitted[is_disp]
  offset_orig_scale <- fun_orig_scale_offset(offset)
  fitted_orig_scale <- fun_orig_scale_linpred(linpred)
  disp_orig_scale <- fun_orig_scale_disp(disp)
  ## obtain values for true outcome
  outcome_true_orig_scale <- draw_outcome_true(nm_distn = nm_distn,
                                               offset = offset_orig_scale,
                                               fitted = fitted_orig_scale,
                                               disp = disp_orig_scale)
  ## if model has datamodel for outcome,
  ## then record true and reported outcomes;
  ## otherwise just record true outcome
  if (has_datamod) {
    outcome_obs_orig_scale <- draw_outcome_obs_given_true(
      datamod = datamod,
      components = components,
      outcome_true = outcome_true_orig_scale,
      offset = offset_orig_scale,
      fitted = fitted_orig_scale
    )
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome_obs_orig_scale
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome_true_orig_scale,
                        nm_x = nm_outcome_data_true)
  }
  else {
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome_true_orig_scale
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## add '.fitted' and return answer
  ans$.fitted <- fitted_orig_scale
  ans
}


## 'equation' -----------------------------------------------------------------

## #' @importFrom generics equation
## #' @export
## generics::equation


## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

## HAS_TESTS
#' Fit a Model
#'
#' Derive the posterior distribution
#' for a model.
#'
#' @section Estimation methods:
#'
#' When `method` is `"standard"` (the default),
#' all parameters, other than
#' the lowest-level rates, probabilities, or
#' means are jointly estimated within TMB.
#'
#' When `method` is `"inner-outer"`, estimation is
#' carried out in multiple steps, which, in large models,
#' can sometimes reduce computation times.
#' In Step 1, a model only using the `inner` variables
#' is fitted to the data.
#' In Step 2, a model only using the
#' `outer` variables is fitted to the data.
#' In Step 3, values for dispersion are calculated.
#' Parameter estimates from steps 1, 2, and 3
#' are then combined.
#'
#' @section Optimizer:
#'
#' The choices for the `optimizer` argument are:
#'
#' - `"multi"` Try `"nlminb"`, and if that fails,
#'   restart from the parameter values where `"nlminb"` stopped,
#'   using `"BFGS"`. The default.
#' - `"nlminb"` [stats::nlminb()]
#' - `"BFGS"` [stats::optim()] using method `"BFGS"`.
#' - `"GC"` [stats::optim()] using method `"CG"` (conjugate gradient).
#'
#' @section Cholesky factorization and `max_jitter`:
#'
#' Sampling from the posterior distribution requires
#' performing a Cholesky factorization of the precision
#' matrix returned by TMB. This factorization sometimes
#' fails because of numerical problems. Adding a small
#' quantity to the diagonal of the precision matrix
#' can alleviate numerical problems, while potentially
#' reducing accuracy. If the Cholesky factorization
#' initially fails, `bage` will try again with progressively
#' larger quantities added to the diagonal, up to the
#' maximum set by `max_jitter`. Increasing the value of
#' `max_jitter` can help suppress numerical problems.
#' A safer strategy, however, is to simplify
#' the model, or to use more informative priors.
#'
#' @section Aggregation:
#'
#' Up to version 0.9.8 of `bage`, `fit()` always aggregated
#' across cells with identical values of the
#' predictor variables
#' in `formula` (ie the variables to the right of `~`)
#' before fitting. For instance,
#' if a dataset contained deaths and population
#' disaggregated by age and sex, but the model formula
#' was `deaths ~ age`, then `fit()` would aggregate
#' deaths and population within each age category
#' before fitting the model. From
#' version 0.9.9, `fit()`
#' only aggregates across cells with identical
#' values if no data model is used,
#' and if the model is Poisson with
#' dispersion set to 0 or is normal.
#' Note that this change in behavior has no effect
#' on most models, since most models include all
#' variables used to classify outcomes. 
#'
#' @param object A `bage_mod` object,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param method Estimation method. Current
#' choices are `"standard"` (the default)
#' and `"inner-outer"`.
#' See below for details.
#' @param vars_inner Names of variables to use
#' for inner model when `method` is `"inner-outer".
#' If `NULL` (the default) `vars_inner` is the
#' [age][set_var_age()], [sex/gender][set_var_sexgender()],
#' and [time][set_var_time()] variables.
#' @param optimizer Which optimizer to use.
#' Current choices are `"multi"`,
#' `"nlminb"`, `"BFGS"`, and `"CG"`. Default
#' is `"multi"`. See below for details.
#' @param quiet Whether to suppress messages
#' from optimizer. Default is `TRUE`.
#' @param max_jitter Maximum quantity to add to
#' diagonal of precision matrix, if Cholesky
#' factorization is failing. Default is
#' 0.0001.
#' @param start_oldpar Whether the optimizer should start
#' at previous estimates. Used only
#' when `fit()` is being called on a fitted
#' model. Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [augment()] Extract values for rates,
#'   probabilities, or means, together
#'   with original data
#' - [components()] Extract values for hyper-parameters
#' - [dispersion()] Extract values for dispersion
#' - [forecast()] Forecast, based on a model
#' - [report_sim()] Simulation study of a model
#' - [unfit()] Reset a model
#' - [is_fitted()] Check if a model has been fitted
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#'
#' ## examine unfitted model
#' mod
#'
#' ## fit model
#' mod <- fit(mod)
#'
#' ## examine fitted model
#' mod
#'
#' ## extract rates
#' aug <- augment(mod)
#' aug
#'
#' ## extract hyper-parameters
#' comp <- components(mod)
#' comp
#' @export
fit.bage_mod <- function(object,
                         method = c("standard", "inner-outer"),
                         vars_inner = NULL,
                         optimizer = c("multi", "nlminb", "BFGS", "CG"),
                         quiet = TRUE,
                         max_jitter = 1e-4,
                         start_oldpar = FALSE,
                         ...) {
  check_old_version(x = object, nm_x = "object")
  check_mod_has_obs(object)
  method <- match.arg(method)
  optimizer <- match.arg(optimizer)
  check_flag(x = quiet, nm_x = "quiet")
  check_number(x = max_jitter, nm_x = "max_jitter")
  if (max_jitter < 0)
    cli::cli_abort("{.arg max_jitter} is negative.")
  check_flag(x = start_oldpar, nm_x = "start_oldpar")
  check_has_no_dots(...)
  if (method == "standard") {
    aggregate <- can_aggregate(object)
    if (!aggregate) {
      formula <- object$formula
      data <- object$data
      formula_covariates <- object$formula_covariates
      warn_not_aggregating(formula = formula,
                           data = data,
                           formula_covariates = formula_covariates,
                           always = FALSE)
    }
    fit_default(object,
                optimizer = optimizer,
                quiet = quiet,
                start_oldpar = start_oldpar,
                aggregate = aggregate)
  }
  else if (method == "inner-outer")
    fit_inner_outer(mod = object,
                    optimizer = optimizer,
                    quiet = quiet,
                    start_oldpar = start_oldpar,
                    vars_inner = vars_inner)
  else
    cli::cli_abort("Internal error: Unexpected value for {.arg method}.") ## nocov
}


## 'forecast' -----------------------------------------------------------------

#' @importFrom generics forecast
#' @export
generics::forecast

## HAS_TESTS
#' Use a Model to Make a Forecast
#'
#' Forecast rates, probabilities, means, and
#' other model parameters.
#'
#' @section How the forecasts are constructed:
#'
#' Internally, the steps involved in a forecast are:
#'
#' 1. Forecast time-varying main effects and interactions,
#'    e.g. a time main effect, or an age-time interaction.
#' 2. Combine forecasts for the time-varying main effects and
#'    interactions with non-time-varying parameters, e.g.
#'    age effects or dispersion.
#' 3. Use the combined parameters to generate values for
#'    rates, probabilities or means.
#' 4. Optionally, generate values for the outcome variable.
#'
#' `forecast()` generates values for the outcome variable when,
#' - `output` is `"augment"`,
#' - a value has been supplied for `newdata`,
#' - `newdata` included a value for the exposure,
#'   size, or weights variable (except if `exposure = 1`
#'   or `weights = 1` in the original call to
#'   [mod_pois()] or [mod_norm()]).
#'
#' [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#' gives more details on the internal calculations in forecasting.
#'
#' @section Output format:
#'
#' When `output` is `"augment"` (the default),
#' the return value from `forecast()`
#' looks like output from function [augment()]. When `output` is
#' `"components"`, the return value looks like output
#' from [components()].
#'
#' When `include_estimates` is `FALSE` (the default),
#' the output of `forecast()` excludes values for
#' time-varying parameters for the period covered by the data.
#' When `include_estimates` is `TRUE`, the output
#' includes these values.
#' Setting `include_estimates` to `TRUE` can be helpful
#' when creating graphs that combine estimates and forecasts.
#'
#' @section Forecasting with covariates:
#'
#' Models that contain [covariates][set_covariates()] can be used
#' in forecasts, provided that
#' - all coefficients (the \eqn{\zeta_p}) are estimated
#'   from historical data via [fit()], and
#' - if any covariates (the columns of \eqn{\pmb{Z}})
#'   are time-varying, then future values for these
#'   covariates are supplied via the `newdata` argument.
#'
#' @section Forecasting with data models:
#'
#' Models that contain [data models][datamods] can be used
#' in forecasts, provided that
#' - the data models have no time-varying parameters, or
#' - future values for time-varying parameters are supplied
#'   when the data model is first specified.
#'
#' For examples, see the [Data Models](https://bayesiandemography.github.io/bage/articles/vig10_datamod.html)
#'   vignette.
#'
#' @section Fitted and unfitted models:
#'
#' `forecast()` is typically used with a
#' [fitted][fit()] model, i.e. a model in which parameter
#' values have been estimated from the data.
#' The resulting forecasts reflect data and priors.
#'
#' `forecast()` can, however, be used with an
#' unfitted model. In this case, the forecasts
#' are based entirely on the priors. See below for
#' an example. Experimenting with forecasts
#' based entirely on the priors can be helpful for
#' choosing an appropriate model.
#'
#' @section Warning:
#'
#' The interface for `forecast()` has not been finalised.
#'
#' @param object A `bage_mod` object,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param newdata Data frame with data for
#' future periods.
#' @param labels Labels for future values.
#' @param output Type of output returned
#' @param include_estimates Whether to
#' include historical estimates along
#' with the forecasts. Default is `FALSE`.
#' @param quiet Whether to suppress messages.
#' Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @returns
#' A [tibble][tibble::tibble-package].
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [bage::fit()] Fit a model
#' - [augment()] Extract values for rates,
#'   probabilities, or means, together
#'   with original data
#' - [components()] Extract values for hyper-parameters
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify and fit model
#' mod <- mod_pois(injuries ~ age * sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   fit()
#' mod
#'
#' ## forecasts
#' mod |>
#'   forecast(labels = 2019:2024)
#'
#' ## combined estimates and forecasts
#' mod |>
#'   forecast(labels = 2019:2024,
#'            include_estimates = TRUE)
#'
#' ## hyper-parameters
#' mod |>
#'   forecast(labels = 2019:2024,
#'            output = "components")
#'
#' ## hold back some data and forecast
#' library(dplyr, warn.conflicts = FALSE)
#' data_historical <- nzl_injuries |>
#'   filter(year <= 2015)
#' data_forecast <- nzl_injuries |>
#'   filter(year > 2015) |>
#'   mutate(injuries = NA)
#' mod_pois(injuries ~ age * sex + ethnicity + year,
#'          data = data_historical,
#'          exposure = popn) |>
#'   fit() |>
#'   forecast(newdata = data_forecast)
#'
#' ## forecast using GDP per capita in 2023 as a covariate
#' mod_births <- mod_pois(births ~ age * region + time,
#'                        data = kor_births,
#'                        exposure = popn) |>
#'   set_covariates(~ gdp_pc_2023) |>
#'   fit()
#' mod_births |>
#'   forecast(labels = 2024:2025)
#' @export
forecast.bage_mod <- function(object,
                              newdata = NULL,
                              labels = NULL,
                              output = c("augment", "components"),
                              include_estimates = FALSE,
                              quiet = FALSE,
                              ...) {
  ## Note that time variable cannot be a covariate,
  ## since the time variable must be the "along" variable,
  ## and hence must be included in 'formula',
  ## and hence cannot be included in 'formula_covariates'.
  check_old_version(x = object, nm_x = "object")
  nm_offset_data <- object$nm_offset_data
  nm_offset_mod <- get_nm_offset_mod(object)
  error_offset_formula_used(nm_offset_data = nm_offset_data,
                            nm_offset_mod = nm_offset_mod,
                            nm_fun = "forecast")
  data_est <- object$data
  priors <- object$priors
  dn_terms_est <- object$dimnames_terms
  var_time <- object$var_time
  var_age <- object$var_age
  var_sexgender <- object$var_sexgender
  output <- match.arg(output)
  check_flag(x = quiet, nm_x = "quiet")
  check_flag(x = include_estimates, nm_x = "include_estimates")
  if (is.null(var_time))
    cli::cli_abort(c("Can't forecast when time variable not identified.",
                     i = "Use {.fun set_var_time} to identify time variable?"))
  check_along_is_time(object)
  if (is_not_testing_or_snapshot())
    cli::cli_progress_message("{.fun components} for past values...") # nocov
  quiet_comp <- quiet || identical(output, "augment")
  comp_est <- components(object, quiet = quiet_comp)
  has_newdata <- !is.null(newdata)
  has_labels <- !is.null(labels)
  if (has_newdata && has_labels)
    cli::cli_abort(c("Values supplied for {.arg newdata} and for {.arg labels}.",
                     i = paste("Please supply a value for {.arg newdata}",
                               "or for {.arg labels} but not for both.")))
  if (!has_newdata && !has_labels)
    cli::cli_abort("No value supplied for {.arg newdata} or for {.arg labels}.")
  if (has_newdata) {
    data_forecast <- make_data_forecast_newdata(mod = object,
                                                newdata = newdata)
    labels <- unique(data_forecast[[var_time]])
  }
  if (has_labels)
    data_forecast <- make_data_forecast_labels(mod = object,
                                               labels_forecast = labels)
  nms_data_forecast <- names(data_forecast)
  seed_forecast_components <- object$seed_forecast_components
  if (is_not_testing_or_snapshot())
    cli::cli_progress_message("{.fun components} for future values...") # nocov
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_forecast_components) ## set pre-determined seed
  if (!is.null(nm_offset_data) && (nm_offset_data %in% nms_data_forecast)) {
    offset_forecast <- data_forecast[[nm_offset_data]]
    has_offset_forecast <- !all(is.na(offset_forecast))
  }
  else
    has_offset_forecast <- FALSE
  has_datamod_outcome <- has_datamod_outcome(object)
  is_forecast_obs <- has_offset_forecast && has_datamod_outcome
  comp_forecast <- forecast_components(mod = object,
                                       components_est = comp_est,
                                       data_forecast = data_forecast,
                                       labels_forecast = labels,
                                       has_newdata = has_newdata,
                                       is_forecast_obs = is_forecast_obs)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  dn_terms_forecast <- make_dimnames_terms_forecast(dimnames_terms = dn_terms_est,
                                                    var_time = var_time,
                                                    labels_forecast = labels,
                                                    time_only = FALSE)
  if (output == "augment") {
    comp_comb <- vctrs::vec_rbind(comp_est, comp_forecast)
    if (is_not_testing_or_snapshot())
      cli::cli_progress_message("{.fun augment} for future values...") # nocov
    linpred_forecast <- make_linpred_from_components(mod = object,
                                                     components = comp_comb,
                                                     data = data_forecast,
                                                     dimnames_terms = dn_terms_forecast)
    seed_forecast_augment <- object$seed_forecast_augment
    seed_restore <- make_seed() ## create randomly-generated seed
    set.seed(seed_forecast_augment) ## set pre-determined seed
    ans <- forecast_augment(mod = object,
                            data_forecast = data_forecast,
                            components_forecast = comp_forecast,
                            linpred_forecast = linpred_forecast,
                            has_offset_forecast,
                            has_newdata = has_newdata)
    set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
    if (include_estimates) {
      if (is_not_testing_or_snapshot())
        cli::cli_progress_message("{.fun augment} for past values...") # nocov
      augment_est <- augment(x = object, quiet = quiet)
      ans <- vctrs::vec_rbind(augment_est, ans)
    }
  }
  else if (output == "components") {
    dn_terms_forecast_time <- make_dimnames_terms_forecast(dimnames_terms = dn_terms_est,
                                                           var_time = var_time,
                                                           labels_forecast = labels,
                                                           time_only = TRUE)
    ans <- infer_trend_cyc_seas_err_forecast(components = comp_forecast,
                                             priors = priors,
                                             dimnames_terms = dn_terms_forecast_time,
                                             var_time = var_time,
                                             var_age = var_age)
    if (include_estimates) {
      ans <- vctrs::vec_rbind(comp_est, ans)
      ans <- sort_components(components = ans, mod = object)
    }
  }
  else
    cli::cli_abort("Internal error: Unexpected value for {.arg output}.") ## nocov
  ans
}


## 'forecast_augment' ---------------------------------------------------------

#' Forecast Contents of 'augment'
#'
#' @param mod Object of class 'bage_mod'
#' @param data_forecast Data frame with
#' values of classifying variables for
#' future time periods
#' @param components_forecast Tibble with
#' values for hyperparameters
#' @param linpred_forecast Linear predictor for future
#' time periods
#' @param has_offset_forecast Whether user supplied offset
#' for future values
#' @param has_newdata Whether user supplied 'newdata'
#' argument
#'
#' @returns A tibble.
#'
#' @noRd
forecast_augment <- function(mod,
                             data_forecast,
                             components_forecast,
                             linpred_forecast,
                             has_offset_forecast,
                             has_newdata) {
  UseMethod("forecast_augment")
}

## HAS_TESTS
#' @export
forecast_augment.bage_mod <- function(mod,
                                      data_forecast,
                                      components_forecast,
                                      linpred_forecast,
                                      has_offset_forecast,
                                      has_newdata) {
  ## extract values
  outcome_est <- mod$outcome
  datamod <- mod$datamod
  confidential <- mod$confidential
  seed_augment <- mod$seed_augment
  nm_distn <- nm_distn(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_outcome_data_true <- paste0(".", nm_outcome_data)
  nm_offset_data <- get_nm_offset_data(mod)
  nm_offset_data_true <- paste0(".", nm_offset_data)
  has_offset_est <- has_varying_offset(mod)
  has_disp <- has_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  var_time <- mod$var_time
  ## prepare inputs
  has_confidential <- has_confidential(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_missing_outcome_est <- anyNA(outcome_est)
  blank <- rep(NA_real_, times = nrow(data_forecast))
  ans <- data_forecast
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## forecast fitted and (if has disp) expected
  if (has_disp) {
    expected <- inv_transform(linpred_forecast)
    disp <- get_disp(mod)
    fitted <- draw_fitted(nm_distn = nm_distn,
                          expected = expected,
                          disp = disp)
  }
  else {
    disp <- NULL
    fitted <- inv_transform(linpred_forecast)
  }
  ## If offset present, forecast outcome.
  ## Note that offset cannot be supplied
  ## if there is a data model for the offset.
  if (has_offset_forecast) {
    offset_forecast <- data_forecast[[nm_offset_data]]
    outcome_true <- draw_outcome_true(nm_distn = nm_distn,
                                      offset = offset_forecast,
                                      fitted = fitted,
                                      disp = NULL)
    if (has_datamod_outcome || has_confidential) {
      outcome <- outcome_true
      if (has_datamod_outcome)
        outcome <-
          forecast_outcome_obs_given_true(datamod = datamod,
                                          data_forecast = data_forecast,
                                          components_forecast = components_forecast,
                                          fitted <- fitted,
                                          outcome_true = outcome_true,
                                          offset = offset_forecast,
                                          has_newdata = has_newdata)
      if (has_confidential)
        outcome <- draw_outcome_confidential(confidential = confidential,
                                             outcome_obs = outcome)
      ans[[nm_outcome_data]] <- outcome
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = outcome_true,
                          nm_x = nm_outcome_data_true)
    }
    else {
      ## No data model or confidentialization
      if (has_missing_outcome_est) {
        ans[[nm_outcome_data]] <- blank
        ans <- insert_after(df = ans,
                            nm_after = nm_outcome_data,
                            x = outcome_true,
                            nm_x = nm_outcome_data_true)
      }
      else
        ans[[nm_outcome_data]] <- outcome_true
    }
  }
  else {
    ## No offset, so do not forecast outcome
    ans[[nm_outcome_data]] <- blank
    if (has_datamod_outcome || has_confidential || has_missing_outcome_est)
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = blank,
                          nm_x = nm_outcome_data_true)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## finish object and return
  if (has_offset_est)
    ans$.observed <- blank
  ans$.fitted <- fitted
  if (has_disp)
    ans$.expected <- expected
  ans
}


## HAS_TESTS
## assume no confidentialization
#' @export
forecast_augment.bage_mod_norm <- function(mod,
                                           data_forecast,
                                           components_forecast,
                                           linpred_forecast,
                                           has_offset_forecast,
                                           has_newdata) {
  ## extract values
  outcome_est <- mod$outcome
  datamod <- mod$datamod
  seed_augment <- mod$seed_augment
  nm_distn <- nm_distn(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_outcome_data_true <- paste0(".", nm_outcome_data)
  nm_offset_data <- get_nm_offset_data(mod)
  has_offset_est <- has_varying_offset(mod)
  fun_orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  fun_orig_scale_disp <- get_fun_orig_scale_disp(mod)
  var_time <- mod$var_time
  ## prepare inputs
  has_datamod_outcome <- has_datamod(mod)
  has_imputed_outcome_est <- anyNA(outcome_est)
  blank <- rep(NA_real_, times = nrow(data_forecast))
  ans <- data_forecast
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## derive fitted
  fitted_orig_scale <- fun_orig_scale_linpred(linpred_forecast)
  ## if offset present, forecast outcome
  if (has_offset_forecast)  {
    offset_forecast <- data_forecast[[nm_offset_data]]
    disp <- get_disp(mod)
    disp_orig_scale <- fun_orig_scale_disp(disp)
    outcome_true <- draw_outcome_true(nm_distn = nm_distn,
                                      offset = offset_forecast,
                                      fitted = fitted_orig_scale,
                                      disp = disp_orig_scale)
    if (has_datamod_outcome) {
      outcome_obs <-
        forecast_outcome_obs_given_true(datamod = datamod,
                                        data_forecast = data_forecast,
                                        components_forecast = components_forecast,
                                        fitted = fitted_orig_scale,
                                        outcome_true = outcome_true,
                                        offset = offset_forecast,
                                        has_newdata = has_newdata)
      ans[[nm_outcome_data]] <- outcome_obs
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = outcome_true,
                          nm_x = nm_outcome_data_true)
    }
    else {
      if (has_imputed_outcome_est) {
        ans[[nm_outcome_data]] <- blank
        ans <- insert_after(df = ans,
                            nm_after = nm_outcome_data,
                            x = outcome_true,
                            nm_x = nm_outcome_data_true)
      }
      else
        ans[[nm_outcome_data]] <- outcome_true
    }
  }
  else {
    ## no offset, so do not forecast outcome
    ans[[nm_outcome_data]] <- blank
    if (has_datamod_outcome || has_imputed_outcome_est)
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = blank,
                          nm_x = nm_outcome_data_true)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## finish object and return
  ans$.fitted <- fitted_orig_scale
  ans
}


## 'get_fun_ag_offset' --------------------------------------------------------

#' Get Function to Use for Aggregating
#' Values for Offset
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A function
#'
#' @noRd
get_fun_ag_offset <- function(mod) {
  UseMethod("get_fun_ag_offset")
}

## HAS_TESTS
#' @export
get_fun_ag_offset.bage_mod_pois <- function(mod) {
  has_varying_offset <- has_varying_offset(mod)
  if (has_varying_offset)
    return(sum)
  else
    return(function(x) 1)
}

## HAS_TESTS
#' @export
get_fun_ag_offset.bage_mod_binom <- function(mod) {
  sum
}


## HAS_TESTS
#' @export
get_fun_ag_offset.bage_mod_norm <- function(mod) {
  function(x) (length(x)^2) / sum(1 / x)
}


## 'get_fun_ag_outcome' --------------------------------------------------------

#' Get Function to Use for Aggregating
#' Values for Outcome
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A function
#'
#' @noRd
get_fun_ag_outcome <- function(mod) {
  UseMethod("get_fun_ag_outcome")
}

## HAS_TESTS
#' @export
get_fun_ag_outcome.bage_mod <- function(mod) sum

## HAS_TESTS
#' @export
get_fun_ag_outcome.bage_mod_norm <- function(mod) mean


## 'get_fun_inv_transform' ----------------------------------------------------

#' Get function to calculate inverse tranformation
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A function
#'
#' @noRd
get_fun_inv_transform <- function(mod) {
    UseMethod("get_fun_inv_transform")
}

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_pois <- function(mod) exp

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_binom <- function(mod)
    function(x) 1 / (1 + exp(-x))

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_norm <- function(mod) identity


## 'get_fun_orig_scale_disp' --------------------------------------------------

#' Get Function to put Dispersion on to Original Scale,
#' in Normal Models
#'
#' Get function to scale dispersion
#' (ie standard deviation in Normal model).
#' The scaling consists of multiplying by the
#' square root of the mean of the weights, and
#' the standard deviation of the original outcome.
#' Applied only to the normal model.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A function
#'
#' @noRd
get_fun_orig_scale_disp <- function(mod) {
    UseMethod("get_fun_orig_scale_disp")
}

## HAS_TESTS
#' @export
get_fun_orig_scale_disp.bage_mod_norm <- function(mod) {
    mean <- mod$offset_mean
    sd <- mod$outcome_sd
    function(x) sqrt(mean) * sd * x
}


## 'get_fun_orig_scale_linpred' -----------------------------------------------

#' Get Function to Put Linear Predictor on to Original Scale,
#' in Normal Models
#'
#' Get function to scale and shift linear predictor.
#' The scaling consists of multiplying by the sd
#' of the original outcome, and then adding the
#' mean. Applied only to the normal model.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A function
#'
#' @noRd
get_fun_orig_scale_linpred <- function(mod) {
    UseMethod("get_fun_orig_scale_linpred")
}

## HAS_TESTS
#' @export
get_fun_orig_scale_linpred.bage_mod_norm <- function(mod) {
    mean <- mod$outcome_mean
    sd <- mod$outcome_sd
    function(x) x * sd + mean
}


## 'get_fun_orig_scale_offset' ------------------------------------------------

#' Get Function to Put Offset (Weights)
#' on to Original Scale, in Normal Models
#'
#' Get function to scale offset.
#' The scaling consists of multiplying by the mean
#' of the original offset.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A function
#'
#' @noRd
get_fun_orig_scale_offset <- function(mod) {
    UseMethod("get_fun_orig_scale_offset")
}

## HAS_TESTS
#' @export
get_fun_orig_scale_offset.bage_mod_norm <- function(mod) {
    mean <- mod$offset_mean
    function(x) x * mean
}


## 'get_nm_offset_data' --------------------------------------------------------

#' Name of Offset Used in Input Data
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
get_nm_offset_data <- function(mod) {
    UseMethod("get_nm_offset_data")
}

## HAS_TESTS
#' @export
get_nm_offset_data.bage_mod <- function(mod) mod$nm_offset_data


## 'get_nm_offset_mod' --------------------------------------------------------

#' Name of Offset Used in Describing Model
#'
#' "exposure", "size", or "weights"
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @seealso get_nm_offset_data
#'
#' @noRd
get_nm_offset_mod <- function(mod) {
    UseMethod("get_nm_offset_mod")
}

## HAS_TESTS
#' @export
get_nm_offset_mod.bage_mod_pois <- function(mod) "exposure"

## HAS_TESTS
#' @export
get_nm_offset_mod.bage_mod_binom <- function(mod) "size"

## HAS_TESTS
#' @export
get_nm_offset_mod.bage_mod_norm <- function(mod) "weights"


## 'get_nm_outcome_data' ------------------------------------------------------

#' Get the Name of the Outcome Variable Used in the Input Data
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A string
#'
#' @noRd
get_nm_outcome_data <- function(mod) {
    UseMethod("get_nm_outcome_data")
}

## HAS_TESTS
#' @export
get_nm_outcome_data.bage_mod <- function(mod) {
  formula <- mod$formula
  ans <- formula[[2L]]
  ans <- deparse1(ans)
  ans
}


## 'has_confidential' ---------------------------------------------------------

#' Test Whether Model Includes Confidential
#'
#' @param mod A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_confidential <- function(mod) {
    UseMethod("has_confidential")
}

## HAS_TESTS
#' @export
has_confidential.bage_mod <- function(mod) {
  confidential <- mod$confidential
  !is.null(confidential)
}


## 'has_covariates' -----------------------------------------------------------

#' Test Whether Model Includes Covariates
#'
#' @param mod A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_covariates <- function(mod) {
    UseMethod("has_covariates")
}

## HAS_TESTS
#' @export
has_covariates.bage_mod <- function(mod) {
  formula_covariates <- mod$formula_covariates
  !is.null(formula_covariates)
}


## 'has_datamod' --------------------------------------------------------------

#' Test Whether Model Includes Datamod
#'
#' @param mod A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_datamod <- function(mod) {
    UseMethod("has_datamod")
}

## HAS_TESTS
#' @export
has_datamod.bage_mod <- function(mod) {
  datamod <- mod$datamod
  !is.null(datamod)
}


## 'has_datamod' --------------------------------------------------------------

#' Test Whether Model Includes Datamod for Outcome
#'
#' @param mod A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_datamod_outcome <- function(mod) {
    UseMethod("has_datamod_outcome")
}

## HAS_TESTS
#' @export
has_datamod_outcome.bage_mod <- function(mod) {
  datamod <- mod$datamod
  inherits(datamod, "bage_datamod_outcome")
}


## 'has_datamod_param' --------------------------------------------------------

#' Test Whether Model Includes Datamod
#' with Parameters
#'
#' @param mod A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_datamod_param <- function(mod) {
    UseMethod("has_datamod_param")
}

## HAS_TESTS
#' @export
has_datamod_param.bage_mod <- function(mod) {
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    param <- make_datamod_param(datamod)
    length(param) > 0L
  }
  else
    FALSE
}


## 'has_disp' ----------------------------------------------------------------

#' Test whether a model includes a dispersion parameter
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_disp <- function(mod) {
    UseMethod("has_disp")
}

## HAS_TESTS
#' @export
has_disp.bage_mod <- function(mod) {
    mean_disp <- mod$mean_disp
    mean_disp > 0L
}



## 'has_varying_offset' ---------------------------------------------------------------

#' Test Whether a Model Includes an Offset
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_varying_offset <- function(mod) {
    UseMethod("has_varying_offset")
}

## HAS_TESTS
#' @export
has_varying_offset.bage_mod <- function(mod) {
  offset <- mod$offset
  nm_offset_data <- mod$nm_offset_data
  if (is.null(offset))
    cli::cli_abort("Internal error: offset is NULL")
  if (all(is.na(offset)))
    cli::cli_abort("Internal error: offset all NA")
  is_all_ones <- all(abs(offset - 1) < 1e-6, na.rm = TRUE)
  has_nm_offset_data <- !is.null(nm_offset_data)
  if (is_all_ones && !has_nm_offset_data)
    FALSE
  else if (!is_all_ones && !has_nm_offset_data)
    cli::cli_abort("Internal error: offset not all ones, but no nm_offset_data")
  else if (is_all_ones && has_nm_offset_data)
    cli::cli_abort("Internal error: offset all ones, but has nm_offset_data")
  else
    TRUE
}

## HAS_TESTS
#' @export
has_varying_offset.bage_mod_binom <- function(mod) {
  offset <- mod$offset
  nm_offset_data <- mod$nm_offset_data
  if (is.null(offset))
    cli::cli_abort("Internal error: offset is NULL")
  if (all(is.na(offset)))
    cli::cli_abort("Internal error: offset all NA")
  if (is.null(nm_offset_data))
    cli::cli_abort("Internal error: nm_offset_data is NULL")
  TRUE
}


## 'is_fitted' ----------------------------------------------------------------

#' Test Whether a Model has Been Fitted
#'
#' Test whether [fit()][fit.bage_mod] has been
#' called on a model object.
#'
#' @param x An object of class `"bage_mod"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to specify a model
#' - [bage::fit()] to fit a model
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' is_fitted(mod)
#' mod <- fit(mod)
#' is_fitted(mod)
#' @export
is_fitted <- function(x) {
  UseMethod("is_fitted")
}

## HAS_TESTS
#' @export
is_fitted.bage_mod <- function(x)
  !is.null(x$draws_effectfree)


## 'make_disp_obs' ------------------------------------------------------------

#' Make Dispersion Used in Model for Observed Outcome
#'
#' Make dispersion in
#' model of observed outcome that takes
#' account of measurement errors
#'
#' @param mod Object of class 'bage_mod'
#' @param components Tibble with estimates of parameters
#'
#' @returns An rvec
#'
#' @noRd
make_disp_obs <- function(mod, components) {
  UseMethod("make_disp_obs")
}

## HAS_TESTS
#' @export
make_disp_obs.bage_mod_pois <- function(mod, components) {
  datamod <- mod$datamod
  if (inherits(datamod, "bage_datamod_exposure")) {
    disp <- get_datamod_disp(datamod)
    ans <- disp / (3 * disp + 1)
  }
  else {
    outcome <- mod$outcome
    n_outcome <- length(outcome)
    has_disp <- has_disp(mod)
    if (has_disp) {
      disp <- get_disp(mod)
      ans <- rep.int(disp, times = n_outcome)
    }
    else
      ans <- NULL
  }
  ans
}

## HAS_TESTS
#' @export
make_disp_obs.bage_mod_binom <- function(mod, components) {
  has_disp <- has_disp(mod)
  if (has_disp) {
    disp <- get_disp(mod)
    outcome <- mod$outcome
    n_outcome <- length(outcome)
    ans <- rep.int(disp, times = n_outcome)
  }
  else
    ans <- NULL
  ans
}


## 'make_expected_obs' --------------------------------------------------------

#' Make Expected Values Used in Model for Observed Outcome
#'
#' Make expected value for rate/probability/mean in
#' model of observed outcome that takes
#' account of measurement errors
#'
#' @param mod Object of class 'bage_mod'
#' @param components Data frame with components
#' @param expected Expected value for rate/prob/mean that does
#' not account for measurement errors. An rvec.
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs <- function(mod, components, expected) {
  UseMethod("make_expected_obs")
}

## HAS_TESTS
#' @export
make_expected_obs.bage_mod_pois <- function(mod, components, expected) {
  datamod <- mod$datamod
  if (is.null(datamod)) {
    ans <- expected
  }
  else {
    if (inherits(datamod, "bage_datamod_exposure")) {
      ans <- make_expected_obs_exposure(datamod = datamod,
                                        expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_miscount")) {
      ans <- make_expected_obs_miscount(datamod = datamod,
                                        components = components,
                                        expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_noise")) {
      ans <- make_expected_obs_noise(expected)
    }
    else if (inherits(datamod, "bage_datamod_overcount")) {
      ans <- make_expected_obs_overcount(datamod = datamod,
                                         components = components,
                                         expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_undercount")) {
      ans <- make_expected_obs_undercount(datamod = datamod,
                                          components = components,
                                          expected = expected)
    }
    else {
      cli::cli_abort(paste("Internal error: Can't handle data model",
                           "with class {.cls {class(datamod)}}"))
    }
  }
  ans
}

## HAS_TESTS
#' @export
make_expected_obs.bage_mod_binom <- function(mod, components, expected) {
  datamod <- mod$datamod
  if (is.null(datamod)) {
    expected
  }
  else if (inherits(datamod, "bage_datamod_undercount")) {
    make_expected_obs_undercount(datamod = datamod,
                                 components = components,
                                 expected = expected)
  }
  else {
    cli::cli_abort(paste("Internal error: Can't handle data model",
                         "with class {.cls {class(datamod)}}"))
  }
}



## 'make_i_lik' ---------------------------------------------------------------

## HAS_TESTS
#' Make 'i_lik' Index used by TMB
#'
#' Consists of 6 digits
#' - First 2 digits give identity of model
#'     - Poisson-with-disp, Poisson-without-disp, etc
#' - Second 2 digits give identify of data model
#'     - bage_datamod_exposure, bage_datamod_miscount, etc
#'     - if 0, then no data model
#' - Third 2 digits give identity of confidentialization process
#'     - bage_confidential_rr3, etc
#'     - if 0, then no confidentialization
#'
#' Create when 'fit' is called, since index
#' can be changed after 'mod' object is
#' constructed.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer scalar
#'
#' @noRd
make_i_lik <- function(mod) {
  UseMethod("make_i_lik")
}

## HAS_TESTS
#' @export
make_i_lik.bage_mod <- function(mod) {
  ans <- make_i_lik_part(mod)
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    ans <- ans + make_i_lik_part(datamod)
  }
  has_confidential <- has_confidential(mod)
  if (has_confidential) {
    confidential <- mod$confidential
    ans <- ans + make_i_lik_part(confidential)
  }
  ans
}


## 'make_i_lik_part' -----------------------------------------------------------

## HAS_TESTS
#' @export
make_i_lik_part.bage_mod_pois <- function(x) {
  has_disp <- has_disp(x)
  if (has_disp)
    100000L
  else
    200000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_mod_binom <- function(x) {
  has_disp <- has_disp(x)
  if (has_disp)
    300000L
  else
    400000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_mod_norm <- function(x) {
  500000L
}


## 'make_mod_disp' -----------------------------------------------------------

#' Make the Model Used to Estimate Dispersion as Part of the Inner-Outer Fit Procedure
#'
#' @param mod The model being fitted. Object of class 'bage_mod'
#'
#' @returns An object of class 'bage_mod'
#'
#' @noRd
make_mod_disp <- function(mod) {
  UseMethod("make_mod_disp")
}

## HAS_TESTS
#' @export
make_mod_disp.bage_mod_pois <- function(mod) {
  nrow_max <- 10000L
  n_term <- length(mod$dimnames_terms)
  use_term <- rep(c(TRUE, FALSE), times = c(1L, n_term - 1L))
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  nrow_data <- nrow(mod$data)
  is_reduce_rows <- nrow_data > nrow_max
  if (is_reduce_rows) {
    rows <- sample(nrow_data, size = nrow_max)
    ans$data <- ans$data[rows, , drop = FALSE]
    ans$outcome <- ans$outcome[rows]
    ans$offset <- ans$offset[rows]
  }
  else
    rows <- NULL
  linpred <- make_linpred_from_stored_draws(mod = mod,
                                            point = TRUE,
                                            rows = rows)
  mu <- exp(linpred)
  ans$offset <- ans$offset * mu
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}

## HAS_TESTS
#' @export
make_mod_disp.bage_mod_binom <- function(mod) {
  nrow_max <- 10000L
  point_est <- make_point_est_effects(mod)
  i_intercept <- match("(Intercept)", names(point_est), nomatch = 0L)
  if (i_intercept > 0L)
    point_est <- point_est[-i_intercept]
  ans <- set_priors_known(mod = mod, prior_values = point_est)
  nrow_data <- nrow(mod$data)
  if (nrow_data > nrow_max) {
    i_keep <- sample(nrow_data, size = nrow_max)
    ans$data <- ans$data[i_keep, , drop = FALSE]
    ans$outcome <- ans$outcome[i_keep]
    ans$offset <- ans$offset[i_keep]
  }
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}

## HAS_TESTS
#' @export
make_mod_disp.bage_mod_norm <- function(mod) {
  nrow_max <- 10000L
  n_term <- length(mod$dimnames_terms)
  use_term <- rep(c(TRUE, FALSE), times = c(1L, n_term - 1L))
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  nrow_data <- nrow(mod$data)
  is_reduce_rows <- nrow_data > nrow_max
  if (is_reduce_rows) {
    rows <- sample(nrow_data, size = nrow_max)
    ans$data <- ans$data[rows, , drop = FALSE]
    ans$outcome <- ans$outcome[rows]
    ans$offset <- ans$offset[rows]
  }
  else
    rows <- NULL
  linpred <- make_linpred_from_stored_draws(mod = mod,
                                            point = TRUE,
                                            rows = rows)
  ans$outcome <- ans$outcome - linpred
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}


## 'make_mod_inner' -----------------------------------------------------------

#' Make the Inner Model as Part of the Inner-Outer Fit Procedure
#'
#' @param mod The model being fitted. Object of class 'bage_mod'
#' @param use_term Logical vector identifying terms to
#' be used in inner and outer models
#'
#' @returns An object of class 'bage_mod'
#'
#' @noRd
make_mod_inner <- function(mod, use_term) {
  UseMethod("make_mod_inner")
}

## HAS_TESTS
#' @export
make_mod_inner.bage_mod <- function(mod, use_term) {
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  ans <- remove_covariates(ans)
  ans$mean_disp <- 0
  ans
}

## HAS_TESTS
#' @export
make_mod_inner.bage_mod_norm <- function(mod, use_term) {
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  ans <- remove_covariates(ans)
  ans
}


## 'make_mod_outer' -----------------------------------------------------------

#' Make the Outer Model as Part of the Inner-Outer Fit Procedure
#'
#' @param mod The model being fitted. Object of class 'bage_mod'
#' @param mod_inner The 'inner' model, with the variables
#' picked out by vars_inner/use_term. Object of class 'bage_mod'
#' @param use_term Logical vector identifying terms to
#' be used in inner and outer models
#'
#' @returns An object of class 'bage_mod'
#'
#' @noRd
make_mod_outer <- function(mod, mod_inner, use_term) {
  UseMethod("make_mod_outer")
}

## HAS_TESTS
#' @export
make_mod_outer.bage_mod_pois <- function(mod, mod_inner, use_term) {
  linpred_inner <- make_linpred_from_stored_draws(mod = mod_inner,
                                                  point = TRUE,
                                                  rows = NULL)
  mu_inner <- exp(linpred_inner)
  use_term <- !use_term
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  ans$offset <- ans$offset * mu_inner
  ans$mean_disp <- 0
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}

## HAS_TESTS
#' @export
make_mod_outer.bage_mod_binom <- function(mod, mod_inner, use_term) {
  point_est_inner <- make_point_est_effects(mod_inner)
  ans <- set_priors_known(mod = mod, prior_values = point_est_inner)
  ans$mean_disp <- 0
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}

## HAS_TESTS
#' @export
make_mod_outer.bage_mod_norm <- function(mod, mod_inner, use_term) {
  linpred_inner <- make_linpred_from_stored_draws(mod = mod_inner,
                                                  point = TRUE,
                                                  rows = NULL)
  use_term <- !use_term
  ans <- reduce_model_terms(mod = mod, use_term = use_term)
  ans$outcome <- ans$outcome - linpred_inner
  ans$nm_offset_data <- "offset_inner_outer"
  ans
}


## 'make_sd_obs' --------------------------------------------------------------

#' Make Standard Deviations Used in Data Models
#'
#' Make sd for data models
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An rvec
#'
#' @noRd
make_sd_obs <- function(mod) {
  UseMethod("make_sd_obs")
}

## HAS_TESTS
#' @export
make_sd_obs.bage_mod <- function(mod) {
  datamod <- mod$datamod
  if (inherits(datamod, "bage_datamod_noise"))
    get_datamod_sd(datamod)
  else
    NULL
}

## HAS_TESTS
#' @export
make_sd_obs.bage_mod_binom <- function(mod) {
  NULL
}


## 'model_descr' --------------------------------------------------------------

#' Name of distribution used in printing
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
model_descr <- function(mod) {
    UseMethod("model_descr")
}

## HAS_TESTS
#' @export
model_descr.bage_mod_pois <- function(mod) "Poisson"

## HAS_TESTS
#' @export
model_descr.bage_mod_binom <- function(mod) "binomial"

## HAS_TESTS
#' @export
model_descr.bage_mod_norm <- function(mod) "normal"


## 'n_draw' -------------------------------------------------------------------

#' Get the Number of Draws for a  Model Object
#'
#' Get the value of `n_draw` for a model object.
#' `n_draw` controls the number
#' of posterior draws that are generated
#' by functions such as [bage::augment()]
#' and [bage::components()].
#'
#' @param x An object of class `"bage_mod"`,
#' created using [mod_pois()], [mod_binom()],
#' or [mod_norm()].
#'
#' @return An integer
#' 
#' @method n_draw bage_mod
#'
#' @seealso
#' - [set_n_draw()] Modify the value of `n_draw`
#' - [mod_pois()],[mod_binom()],[mod_norm()] Create a model object
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' n_draw(mod)
#' mod <- mod |>
#'   set_n_draw(n_draw = 5000)
#' n_draw(mod)
#' @export
n_draw.bage_mod <- function(x) {
  x$n_draw
}


## 'nm_distn' -----------------------------------------------------------------

#' Name of distribution used internally
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A string
#'
#' @noRd
nm_distn <- function(mod) {
    UseMethod("nm_distn")
}

## HAS_TESTS
#' @export
nm_distn.bage_mod_pois <- function(mod) "pois"

## HAS_TESTS
#' @export
nm_distn.bage_mod_binom <- function(mod) "binom"

## HAS_TESTS
#' @export
nm_distn.bage_mod_norm <- function(mod) "norm"


## 'print' --------------------------------------------------------------------

#' Printing a Model
#'
#' @description
#' After calling a function such as [mod_pois()] or
#' [set_prior()] it is good practice to print the
#' model object at the console, to check the model's
#' structure. The output from `print()` has
#' the following components:
#'
#' - A header giving the class of the model
#'   and noting whether the model has been fitted.
#' - A [formula][stats::formula()] giving the
#'   outcome variable and terms for the model.
#' - A table giving the number of parameters, and
#'   (fitted models only) the standard
#'   deviation across those parameters,
#'   a measure of the term's importance.
#'   See [priors()] and [tidy()].
#' - Values for other model settings. See [set_disp()],
#'   [set_var_age()], [set_var_sexgender()], [set_var_time()],
#'   [set_n_draw()]
#' - Details on computations (fitted models only).
#'   See [computations()].
#'
#' @param x Object of class `"bage_mod"`, typically
#' created with [mod_pois()], [mod_binom()],
#' or [mod_norm()].
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns `x`, invisibly.
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit.bage_mod()][fit()] and [is_fitted()] Model fitting
#' - [augment()] Extract values for rates,
#'   probabilities, or means, together
#'   with original data
#' - [components()] Extract values for hyper-parameters
#' - [dispersion()] Extract values for dispersion
#' - [priors] Overview of priors for model terms
#' - [tidy.bage_mod()][tidy()] Number of parameters,
#'   and standard deviations
#' - [set_disp()] Dispersion
#' - [set_var_age()], [set_var_sexgender()], [set_var_time()]
#'    Age, sex/gender and time variables
#' - [set_n_draw()] Model draws
#'
#' @rdname print.bage_mod
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#'
#' ## print unfitted model
#' mod
#'
#' mod <- fit(mod)
#'
#' ## print fitted model
#' mod
#' @export
print.bage_mod <- function(x, ...) {
  check_old_version(x = x, nm_x = "x")
  nchar_offset <- 10L
  ## calculations
  formula <- x$formula
  priors <- x$priors
  n_draw <- x$n_draw
  data <- x$data
  nm_offset_data <- get_nm_offset_data(x)
  var_age <- x$var_age
  var_sexgender <- x$var_sexgender
  var_time <- x$var_time
  has_covariates <- has_covariates(x)
  if (has_covariates)
    formula_covariates <- x$formula_covariates
  mean_disp <- x$mean_disp
  has_datamod <- has_datamod(x)
  if (has_datamod)
    datamod <- x$datamod
  has_confidential <- has_confidential(x)
  if (has_confidential)
    confidential <- x$confidential
  vars_inner <- x$vars_inner
  optimizer <- x$optimizer
  computations <- x$computations
  is_fitted <- is_fitted(x)
  str_title <- sprintf("    ------ %s %s model ------",
                       if (is_fitted) "Fitted" else "Unfitted",
                       model_descr(x))
  nchar_response <- nchar(as.character(formula[[2L]]))
  formula_text <- strwrap(deparse1(formula),
                          width = 65L,
                          indent = 3L,
                          exdent = nchar_response + 7L)
  has_varying_offset <- has_varying_offset(x)
  if (has_varying_offset) {
    nm_offset_mod <- get_nm_offset_mod(x)
    nm_offset_data <- get_nm_offset_data(x)
    str_offset <- sprintf("% *s: %s",
                          nchar_offset + 15L,
                          nm_offset_mod,
                          nm_offset_data)
  }
  if (has_datamod) {
    datamod_descr <- datamod_descr(datamod)
    str_datamod <- sprintf("% *s: %s",
                           nchar_offset + 15L,
                           "data model",
                           datamod_descr)
  }
  if (has_confidential) {
    str_call_confidential <- str_call_confidential(confidential)
    str_confidential <- sprintf("% *s: %s",
                                nchar_offset + 15L,
                                "confidentialization",
                                str_call_confidential)
  }
  terms <- tidy(x)
  terms <- as.data.frame(terms)
  terms$along[is.na(terms$along)] <- "-"
  if (is_fitted) {
    is_na_std_dev <- is.na(terms$std_dev)
    terms$std_dev <- sprintf("%0.2f", terms$std_dev)
    terms$std_dev[is_na_std_dev] <- "-"
  }
  settings <- data.frame(n_draw = n_draw)
  for (nm in c("var_time", "var_age", "var_sexgender")) {
    val <- get(nm)
    if (!is.null(val)) {
      tmp <- data.frame(val)
      names(tmp) <- nm
      settings <- cbind(settings, tmp)
    }
  }
  has_optimizer <- !is.null(optimizer)
  if (has_optimizer)
    settings <- cbind(settings,
                      data.frame(optimizer = optimizer))
  is_inner_outer <- !is.null(vars_inner)
  if (is_inner_outer)
    settings <- cbind(settings,
                      data.frame(method = "inner-outer"))
  if (is_fitted) {
    computations <- as.data.frame(computations)
    computations$time_total <- sprintf("%0.2f", computations$time_total)
    computations$time_max <- sprintf("%0.2f",computations$time_max)
    computations$time_draw <- sprintf("%0.2f",computations$time_draw)
    computations$message <- paste0("  ", computations$message)
  }
  is_inner_outer <- is_fitted && !is.null(vars_inner)
  ## printing
  cat("\n")
  cat(str_title)
  cat("\n\n")
  cat(paste(formula_text, collapse = "\n"))
  cat("\n\n")
  if (has_varying_offset) {
    cat(str_offset)
    cat("\n")
  }
  if (has_datamod) {
    cat(str_datamod)
    cat("\n")
  }
  if (has_confidential) {
    cat(str_confidential)
    cat("\n")
  }
  cat("\n")
  print(terms, row.names = FALSE)
  cat("\n")
  if (has_covariates)
    cat(sprintf(" covariates: %s\n\n", deparse1(formula_covariates)))
  if (mean_disp > 0)
    cat(sprintf(" disp: mean = %s\n\n", mean_disp))
  print(settings, row.names = FALSE)
  if (is_fitted) {
    cat("\n")
    print(computations, row.names = FALSE)
  }
  cat("\n")
  ## return
  invisible(x)
}


## 'remove_covariates' --------------------------------------------------------

## HAS_TESTS
#' Remove Covariates from a Model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
remove_covariates <- function(mod) {
  UseMethod("remove_covariates")
}

## HAS_TESTS
#' @export
remove_covariates.bage_mod <- function(mod) {
  mod["formula_covariates"] <- list(NULL)
  mod["covariates_nms"] <- list(NULL)
  mod
}


## 'replicate_data' -----------------------------------------------------------

#' Create Replicate Data
#'
#' Use a fitted model to create replicate datasets,
#' typically as a way of checking a model.
#'
#' Use `n` draws from the posterior distribution
#' for model parameters to generate `n` simulated datasets.
#' If the model is working well, these simulated
#' datasets should look similar to the actual dataset.
#'
#' @section The `condition_on` argument:
#'
#' With Poisson and binomial models that include
#' dispersion terms (which is the default), there are
#' two options for constructing replicate data.
#'
#' - When `condition_on` is `"fitted"`,
#' the replicate data is created by (i) drawing values
#' from the posterior distribution for rates or probabilities
#' (the \eqn{\gamma_i} defined in [mod_pois()]
#' and [mod_binom()]), and (ii)  conditional on these
#' rates or probabilities, drawing values for the
#' outcome variable.
#' - When `condition_on` is `"expected"`,
#' the replicate data is created by (i) drawing
#' values from hyper-parameters governing
#' the rates or probabilities
#' (the \eqn{\mu_i} and \eqn{\xi} defined
#' in [mod_pois()] and [mod_binom()]),
#' then (ii) conditional on these hyper-parameters,
#' drawing values for the rates or probabilities,
#' and finally (iii) conditional on these
#' rates or probabilities, drawing values for the
#' outcome variable. The `"expected" option
#' is only possible in Poisson and binomial models,
#' and only when dispersion is non-zero.
#'
#' The default for `condition_on` is `"expected"`,
#' in cases where it is feasible.
#' The `"expected"` option
#' provides a more severe test for
#' a model than the `"fitted"` option,
#' since "fitted" values are weighted averages
#' of the "expected" values and the original
#' data.
#'
#' @section Data models for outcomes:
#'
#' If a [data model][datamods] has been provided for
#' the outcome variable, then creation of replicate
#' data will include a step where errors are added
#' to outcomes. For instance, the a [rr3][set_datamod_outcome_rr3()]
#' data model is used, then `replicate_data()` rounds
#' the outcomes to base 3.
#'
#' @param x A fitted model, typically created by
#' calling [mod_pois()], [mod_binom()], or [mod_norm()],
#' and then [fit()].
#' @param condition_on Parameters to condition
#' on. Either `"expected"` or `"fitted"`. See
#' details.
#' @param n Number of replicate datasets to create.
#' Default is 19.
#'
#' @returns A [tibble][tibble::tibble-package]
#' with the following structure:
#'
#' |`.replicate`     | data                           |
#' |-----------------|--------------------------------|
#' |`"Original"`     | Original data supplied to [mod_pois()], [mod_binom()], [mod_norm()] |
#' |`"Replicate 1"`  | Simulated data. |
#' |`"Replicate 2"`  | Simulated data. |
#' |\dots            | \dots           |
#' |`"Replicate <n>"`| Simulated data. |
#'
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit()] Fit model.
#' - [augment()] Extract values for rates,
#'   probabilities, or means, together
#'   with original data
#' - [components()] Extract values for hyper-parameters
#' - [dispersion()] Extract values for dispersion
#' - [forecast()] Forecast, based on a model
#' - [report_sim()] Simulation study of model.
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = 1) |>
#'   fit()
#'
#' rep_data <- mod |>
#'   replicate_data()
#'
#' library(dplyr)
#' rep_data |>
#'   group_by(.replicate) |>
#'   count(wt = injuries)
#'
#' ## when the overall model includes an rr3 data model,
#' ## replicate data are rounded to base 3
#' mod_pois(injuries ~ age:sex + ethnicity + year,
#'          data = nzl_injuries,
#'          exposure = popn) |>
#'   set_datamod_outcome_rr3() |>
#'   fit() |>
#'   replicate_data()
#' @export
replicate_data <- function(x, condition_on = NULL, n = 19) {
    UseMethod("replicate_data")
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_pois <- function(x, condition_on = NULL, n = 19) {
  check_old_version(x = x, nm_x = "x")
  has_disp <- has_disp(x)
  if (is.null(condition_on)) {
    if (has_disp)
      condition_on <- "expected"
    else
      condition_on <- "fitted"
  }
  else {
    condition_on <- match.arg(condition_on, choices = c("expected", "fitted"))
    if (condition_on == "expected")
      check_has_disp_if_condition_on_expected(x)
  }
  poputils::check_n(n = n,
                    nm_n = "n",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_is_fitted(x = x, nm_x = "x")
  data <- x$data
  outcome <- x$outcome
  offset <- x$offset
  datamod <- x$datamod
  confidential <- x$confidential
  nm_distn <- nm_distn(x)
  has_datamod <- has_datamod(x)
  has_datamod_outcome <- has_datamod_outcome(x)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  has_confidential <- has_confidential(x)
  nm_outcome_data <- get_nm_outcome_data(x)
  n_obs <- nrow(data)
  x <- set_n_draw(x, n_draw = n)
  if (has_datamod)
    components <- components(x)
  augment <- augment(x, quiet = TRUE)
  if (has_disp) {
    expected <- augment$.expected
    disp <- get_disp(x)
  }
  else 
    expected <- augment$.fitted
  if (condition_on == "fitted")
    fitted <- augment$.fitted
  else if (condition_on == "expected") {
    shape <- 1 / disp
    rate <- 1 / (expected * disp)
    fitted <- rvec::rgamma_rvec(n = n_obs,
                                shape = shape,
                                rate = rate)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'condition_on'.") ## nocov
  if (has_datamod_offset) { ## implies 'has_disp' is FALSE
    offset <- draw_offset_true_given_obs(datamod = datamod,
                                         nm_distn = nm_distn,
                                         components = components,
                                         outcome = outcome,
                                         offset_obs = offset,
                                         expected = expected)
  }
  lambda <- fitted * offset
  y_rep <- rpois_guarded(lambda = lambda)
  if (has_datamod_outcome) {
    y_rep <- draw_outcome_obs_given_true(datamod = datamod,
                                         components = components,
                                         outcome_true = y_rep,
                                         offset = offset,
                                         fitted = fitted)
  }
  if (has_confidential) {
    y_rep <- draw_outcome_confidential(confidential = confidential,
                                       outcome_obs = y_rep)
  }
  outcome_rep <- c(outcome, as.numeric(y_rep))
  ans <- make_copies_repdata(data = data, n = n)
  ans[[nm_outcome_data]] <- outcome_rep
  ans
}

## HAS_TESTS
## assume any data model applies only to outcome
#' @export
replicate_data.bage_mod_binom <- function(x, condition_on = NULL, n = 19) {
  check_old_version(x = x, nm_x = "x")
  has_disp <- has_disp(x)
  if (is.null(condition_on)) {
    if (has_disp)
      condition_on <- "expected"
    else
      condition_on <- "fitted"
  }
  else {
    condition_on <- match.arg(condition_on, choices = c("expected", "fitted"))
    if (condition_on == "expected")
      check_has_disp_if_condition_on_expected(x)
  }
  poputils::check_n(n = n,
                    nm_n = "n",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_is_fitted(x = x, nm_x = "x")
  data <- x$data
  outcome <- x$outcome
  offset <- x$offset
  datamod <- x$datamod
  confidential <- x$confidential
  has_datamod <- has_datamod(x)
  has_confidential <- has_confidential(x)
  nm_outcome_data <- get_nm_outcome_data(x)
  has_disp <- has_disp(x)
  n_obs <- nrow(data)
  x <- set_n_draw(x, n_draw = n)
  if (has_datamod)
    components <- components(x)
  augment <- augment(x, quiet = TRUE)
  if (has_disp) {
    expected <- augment$.expected
    disp <- get_disp(x)
  }
  else 
    expected <- augment$.fitted
  if (condition_on == "fitted")
    fitted <- augment$.fitted
  else if (condition_on == "expected") {
    shape1 <- expected / disp
    shape2 <- (1 - expected) / disp
    fitted <- rvec::rbeta_rvec(n = n_obs,
                               shape1 = shape1,
                               shape2 = shape2)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'condition_on'.") ## nocov
  y_rep <- rbinom_guarded(size = offset, prob = fitted)
  if (has_datamod) {
    y_rep <- draw_outcome_obs_given_true(datamod = datamod,
                                         components = components,
                                         outcome_true = y_rep,
                                         offset = offset,
                                         fitted = fitted)
  }
  if (has_confidential) {
    y_rep <- draw_outcome_confidential(confidential = confidential,
                                       outcome_obs = y_rep)
  }
  outcome_rep <- c(outcome, as.numeric(y_rep))
  ans <- make_copies_repdata(data = data, n = n)
  ans[[nm_outcome_data]] <- outcome_rep
  ans
}

## HAS_TESTS
## assume no confidentialization
#' @export
replicate_data.bage_mod_norm <- function(x, condition_on = NULL, n = 19) {
  check_old_version(x = x, nm_x = "x")
  if (!is.null(condition_on))
    cli::cli_warn(c("Ignoring value for {.arg condition_on}.",
                    i = paste("No need to choose which values to condition on",
                              "when {.arg x} created with {.fun mod_norm}.")))
  poputils::check_n(n = n,
                    nm_n = "n",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_is_fitted(x = x, nm_x = "x")
  data <- x$data
  formula <- x$formula
  outcome <- x$outcome
  offset <- x$offset
  datamod <- x$datamod
  nm_outcome_data <- get_nm_outcome_data(x)
  has_datamod <- has_datamod(x)
  fun_orig_scale_linpred <- get_fun_orig_scale_linpred(x)
  fun_orig_scale_disp <- get_fun_orig_scale_disp(x)
  fun_orig_scale_offset <- get_fun_orig_scale_offset(x)
  x <- set_n_draw(x, n_draw = n)
  disp <- get_disp(x)
  augment <- augment(x, quiet = TRUE)
  n_obs <- nrow(data)
  fitted <- augment$.fitted
  outcome_orig_scale <- fun_orig_scale_linpred(outcome)
  disp_orig_scale <- fun_orig_scale_disp(disp)
  offset_orig_scale <- fun_orig_scale_offset(offset)
  sd <- disp_orig_scale / sqrt(offset_orig_scale)
  y_rep <- rvec::rnorm_rvec(n = n_obs, mean = fitted, sd = sd)
  if (has_datamod) {
    components <- components(x, quiet = TRUE)
    y_rep <- draw_outcome_obs_given_true(datamod = datamod,
                                         components = components,
                                         outcome_true = y_rep,
                                         offset = offset,
                                         fitted = fitted)
  }
  outcome_rep <- c(outcome_orig_scale, as.numeric(y_rep))
  ans <- make_copies_repdata(data = data, n = n)
  ans[[nm_outcome_data]] <- outcome_rep
  ans
}


## 'tidy' ---------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

## HAS_TESTS
#' Summarize Terms from a Fitted Model
#'
#' Summarize the intercept, main effects, and interactions
#' from a fitted model.
#'
#' The [tibble][tibble::tibble-package] returned by `tidy()`
#' contains the following columns:
#'
#' - `term` Name of the intercept, main effect, or interaction
#' - `prior` Specification for prior
#' - `n_par` Number of parameters
#' - `n_par_free` Number of free parameters
#' - `std_dev` Standard deviation for point estimates.
#'
#' With some priors, the number of free parameters is less than
#' the number of parameters for that term. For instance, an [SVD()]
#' prior might use three vectors to represent 101 age groups
#' so that the number of parameters is 101, but the number of
#' free parameters is 3.
#'
#' `std_dev` is the standard deviation across elements of a
#' term, based on point estimates of those elements.
#' For instance, if the point
#' estimates for a term with three elements are
#' 0.3, 0.5, and 0.1,  then the value for `std_dev` is

#' ```
#' sd(c(0.3, 0.5, 0.1))
#' ```
#' `std_dev` is a measure of the contribution of a term to
#' variation in the outcome variable.
#'
#' @param x Object of class `"bage_mod"`, typically
#' created with [mod_pois()], [mod_binom()],
#' or [mod_norm()].
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package]
#'
#' @seealso
#' - [augment()] Extract values for rates,
#'   probabilities, or means, together
#'   with original data
#' - [components()] Extract values for hyper-parameters
#' - [dispersion()] Extract values for dispersion
#'
#' @references `std_dev` is modified from Gelman et al. (2014)
#' *Bayesian Data Analysis. Third Edition*. pp396--397.
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' mod <- fit(mod)
#' tidy(mod)
#' @export
tidy.bage_mod <- function(x, ...) {
  check_old_version(x = x, nm_x = "x")
  check_has_no_dots(...)
  priors <- x$priors
  dimnames_terms <- x$dimnames_terms
  along <- make_along_mod(x)
  n_par <- make_lengths_effect(dimnames_terms)
  n_par_free <- make_lengths_effectfree(x)
  term <- names(priors)
  prior <- vapply(priors, str_call_prior, "")
  ans <- tibble::tibble(term, prior, along, n_par, n_par_free)
  is_fitted <- is_fitted(x)
  if (is_fitted) {
    effectfree <- x$point_effectfree
    effects <- make_effects(mod = x, effectfree = effectfree)
    ans[["std_dev"]] <- vapply(effects, stats::sd, 0)
  }
  ans <- tibble::tibble(ans)
  ans
}
