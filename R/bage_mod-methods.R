
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Extract Data and Modelled Values
#'
#' Extract data and values from a model object.
#' The return value consists of the original
#' data and one or more columns of modelled values.
#'
#' @section Fitted vs unfitted models:
#'
#' `augment()` is typically called on a [fitted][fit()]
#' model. In this case, the modelled values are
#' draws from the joint posterior distribution for rates,
#' probabilities, or means.
#'
#' `augment()` can, however, be called on an
#' unfitted model. In this case, the modelled values
#' are draws from the joint prior distribution.
#' In other words, the modelled values are informed by
#' model priors, and by any `exposure`, `size`, or `weights`
#' arguments in the model, but not by the observed outcomes.
#'
#' @section The 'center' argument:
#'
#' TODO - WRITE THIS
#'
#' @param x An object of class `"bage_mod"`.
#' @param center Whether to center simulation draws.
#' Used only with unfitted models. See below for details.
#' @param quiet Whether to suppress messages.
#' Default is `FALSE`.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns
#' A [tibble][tibble::tibble-package], with the original
#' data plus the following columns:
#'
#' - `.observed` 'Direct' estimates of rates or
#' probabilities, ie counts divided by exposure orsize
#' (in Poisson and binomial models.)
#' - `.fitted` Draws of rates, probabilities,
#' or means.
#' - `.expected` Draws of expected values for
#' rates or probabilities (in Poisson
#' that include exposure, or in binomial models.)
#'
#' Uncertain quantities are represented using
#' [rvecs][rvec::rvec()].
#'
#' @seealso
#' - [components()] Extract values for hyper-parameters from a model
#' - [tidy()] Extract a one-line summary of a model
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit()] Fit a model
#' - [is_fitted()] See if a model has been fitted
#' 
#' @examples
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#'
#' ## look at prior distribution
#' mod |> augment()
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#'
#' ## look at posterior distribution
#' mod |> augment()
#' @export
augment.bage_mod <- function(x,
                             center = FALSE,
                             quiet = FALSE,
                             ...) {
  check_flag(x = center, nm_x = "center")
  check_flag(x = quiet, nm_x = "quiet")
  is_fitted <- is_fitted(x)
  if (is_fitted) {
    check_center_is_default(center = center, default = FALSE)
    ans <- draw_vals_augment_fitted(x)
  }
  else {
    if (!quiet)
      cli::cli_alert_info("Model not fitted, so values drawn straight from prior distribution.")
    ans <- draw_vals_augment_unfitted(x, center = center)
  }
  ans
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract Values for Hyper-Parameters
#'
#' Extract values for hyper-parameters
#' from a model object. Includes values for
#' main effects and interactions, and values
#' for any disperion or variance terms.
#' 
#' @section Fitted vs unfitted models:
#'
#' `components()` is typically called on a [fitted][fit()]
#' model. In this case, the modelled values are
#' draws from the joint posterior distribution for the
#' hyper-parameters in the model.
#'
#' `components()` can, however, be called on an
#' unfitted model. In this case, the modelled values
#' are draws from the joint prior distribution.
#' In other words, the modelled values are informed by
#' model priors, and by any `exposure`, `size`, or `weights`
#' argument in the model, but not by the observed outcomes.
#'
#' @inheritSection augment.bage_mod The 'center' argument
#'
#' @inheritParams augment.bage_mod
#' @param object An object of class `"bage_mod"`.
#' @param quiet Whether to suppress messages.
#' Default is `FALSE`.
#' @param ... Unused. Included for generic consistency only.
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
#' - [augment()] Extract data and values for rates,
#'   means, or probabilities
#' - [tidy()] Extract a one-line summary of a model
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [fit()] Fit a model
#' - [is_fitted()] See if a model has been fitted
#'
#' @examples
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#'
#' ## look at prior distribution
#' mod |> components()
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#'
#' ## look at posterior distribution
#' mod |> components() ## posterior distribution
#' @export
components.bage_mod <- function(object,
                                center = FALSE,
                                quiet = FALSE,
                                ...) {
  check_flag(x = center, nm_x = "center")
  check_flag(x = quiet, nm_x = "quiet")
  is_fitted <- is_fitted(object)
  if (is_fitted) {
    check_center_is_default(center = center, default = FALSE)
    ans <- draw_vals_components_fitted(object)
  }
  else {
    if (!quiet)
      cli::cli_alert_info("Model not fitted, so values drawn straight from prior distribution.")
    n_draw <- object$n_draw
    ans <- draw_vals_components_unfitted(mod = object,
                                         n_sim = n_draw,
                                         center = center)
  }
  ans <- sort_components(components = ans,
                         mod = object)
  ans
}


## 'draw_vals_augment_fitted' -------------------------------------------------

#' Draw '.fitted' and Possibly '.expected' from Fitted Model
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
draw_vals_augment_fitted <- function(mod) {
  UseMethod("draw_vals_augment_fitted")
}

## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod <- function(mod) {
  ans <- mod$data
  ans$.observed <- make_observed(mod)
  linpred <- mod$draws_linpred
  linpred <- Matrix::as.matrix(linpred)
  inv_transform <- get_fun_inv_transform(mod)
  has_disp <- has_disp(mod)
  if (has_disp) {
    expected <- inv_transform(linpred)
    disp <- mod$draws_disp
    disp <- matrix(disp, nrow = 1L)
    expected <- rvec::rvec_dbl(expected)
    disp <- rvec::rvec_dbl(disp)
    seed_augment <- mod$seed_augment
    seed_restore <- make_seed() ## create randomly-generated seed
    set.seed(seed_augment) ## set pre-determined seed
    ans$.fitted <- make_par_disp(x = mod,
                                 meanpar = expected,
                                 disp = disp)
    set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
    ans$.expected <- expected
  }
  else
    ans$.fitted <- inv_transform(linpred)
  ans
}

## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod_norm <- function(mod) {
  ans <- mod$data
  linpred <- mod$draws_linpred
  linpred <- Matrix::as.matrix(linpred)
  scale_outcome <- get_fun_scale_outcome(mod)
  .fitted <- scale_outcome(linpred)
  .fitted <- rvec::rvec_dbl(.fitted)
  ans$.fitted <- .fitted
  ans
}


## 'draw_vals_augment_unfitted' --------------------------------------------------------

#' Draw '.fitted' and Possibly '.expected' from Unfitted Model
#'
#' @param mod Object of class 'bage_mod'
#' @param center Whether to center simulation draws
#'
#' @returns Named list
#'
#' @noRd
draw_vals_augment_unfitted <- function(mod, center) {
  UseMethod("draw_vals_augment_unfitted")
}

## HAS_TESTS
#' @export
draw_vals_augment_unfitted.bage_mod <- function(mod, center) {
  n_draw <- mod$n_draw
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_draw,
                                                   center = center)
  inv_transform <- get_fun_inv_transform(mod)
  has_disp <- has_disp(mod)
  nm_outcome <- get_nm_outcome(mod)
  offset <- mod$offset
  vals_linpred <- make_linpred_effect(mod = mod,
                                      components = vals_components)
  seed_augment <- mod$seed_augment
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  if (has_disp) {
    vals_expected <- inv_transform(vals_linpred)
    is_disp <- vals_components$component == "disp"
    vals_disp <- vals_components$.fitted[is_disp]
    vals_fitted <- draw_vals_fitted(mod = mod,
                                    vals_expected = vals_expected,
                                    vals_disp = vals_disp)
  }
  else
    vals_fitted <- inv_transform(vals_linpred)
  vals_outcome <- draw_vals_outcome(mod = mod,
                                    vals_fitted = vals_fitted)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  vals_observed <- vals_outcome / offset
  ans <- mod$data
  ans[[nm_outcome]] <- vals_outcome
  ans$.observed <- vals_observed
  ans$.fitted <- vals_fitted
  if (has_disp)
    ans$.expected <- vals_expected
  ans
}

## HAS_TESTS
#' @export
draw_vals_augment_unfitted.bage_mod_norm <- function(mod, center) {
  n_draw <- mod$n_draw
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_draw,
                                                   center = center)
  scale_outcome <- get_fun_scale_outcome(mod)
  nm_outcome <- get_nm_outcome(mod)
  vals_linpred <- make_linpred_effect(mod = mod,
                                      components = vals_components)
  vals_fitted <- scale_outcome(vals_linpred)
  is_disp <- vals_components$component == "disp"
  vals_disp <- vals_components$.fitted[is_disp]
  seed_augment <- mod$seed_augment
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  vals_outcome <- draw_vals_outcome(mod = mod,
                                    vals_fitted = vals_fitted,
                                    vals_disp = vals_disp)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ans <- mod$data
  ans[[nm_outcome]] <- vals_outcome
  ans$.fitted <- vals_fitted
  ans
}


## 'draw_vals_fitted' ---------------------------------------------------------

#' Draw Values for '.fitted' Variable in 'augment'
#' when 'disp' non-NULL
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_expected Backtransformed linear predictor. An rvec.
#' @param vals_disp Dispersion. An rvec.
#'
#' @returns An rvec.
#'
#' @noRd
draw_vals_fitted <- function(mod, vals_expected, vals_disp) {
    UseMethod("draw_vals_fitted")
}

## HAS_TESTS
#' @export
draw_vals_fitted.bage_mod_pois <- function(mod, vals_expected, vals_disp)
  rvec::rgamma_rvec(n = length(vals_expected),
                    shape = 1 / vals_disp,
                    rate = 1 / (vals_disp * vals_expected))

## HAS_TESTS
#' @export
draw_vals_fitted.bage_mod_binom <- function(mod, vals_expected, vals_disp)
  rvec::rbeta_rvec(n = length(vals_expected),
                   shape1 = vals_expected / vals_disp,
                   shape2 = (1 - vals_expected) / vals_disp)


## 'draw_vals_outcome' --------------------------------------------------------

#' Draw Values for Outcome Variable
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_fitted '.fitted' variable from 'augment'. An rvec.
#' @param vals_disp Standard deviation. Only used with
#' normal model. An rvec
#'
#' @returns An rvec
#'
#' @noRd
draw_vals_outcome <- function(mod, vals_fitted, vals_disp) {
    UseMethod("draw_vals_outcome")
}

## HAS_TESTS
#' @export
draw_vals_outcome.bage_mod_pois <- function(mod, vals_fitted, vals_disp) {
  offset <- mod$offset
  n_val <- length(vals_fitted)
  n_draw <- rvec::n_draw(vals_fitted)
  is_not_na <- !is.na(offset)
  vals <- rvec::rpois_rvec(n = sum(is_not_na),
                           lambda = vals_fitted[is_not_na] * offset[is_not_na])
  na <- if (is.integer(vctrs::field(vals, "data"))) NA_integer_ else NA_real_
  ans <- rvec::rvec(matrix(na, nrow = n_val, ncol = n_draw))
  ans[is_not_na] <- vals
  ans
}

## HAS_TESTS
#' @export
draw_vals_outcome.bage_mod_binom <- function(mod, vals_fitted, vals_disp) {
  offset <- mod$offset
  n_val <- length(vals_fitted)
  n_draw <- rvec::n_draw(vals_fitted)
  is_not_na <- !is.na(offset)
  vals <- rvec::rbinom_rvec(n = sum(is_not_na),
                            size = offset[is_not_na],
                            prob = vals_fitted[is_not_na])
  na <- if (is.integer(vctrs::field(vals, "data"))) NA_integer_ else NA_real_
  ans <- rvec::rvec(matrix(na, nrow = n_val, ncol = n_draw))
  ans[is_not_na] <- vals
  ans
}

## HAS_TESTS
#' @export
draw_vals_outcome.bage_mod_norm <- function(mod, vals_fitted, vals_disp) {
  offset <- mod$offset
  n_val <- length(vals_fitted)
  n_draw <- rvec::n_draw(vals_fitted)
  ans <- rvec::rvec_dbl(matrix(NA_real_, nrow = n_val, ncol = n_draw))
  is_not_na <- !is.na(offset)
  ans[is_not_na] <- rvec::rnorm_rvec(n = sum(is_not_na),
                                     mean = vals_fitted[is_not_na],
                                     sd = vals_disp / sqrt(offset[is_not_na]))
  ans
}


## 'equation' -----------------------------------------------------------------

#' @importFrom generics equation
#' @export
generics::equation


## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

## HAS_TESTS
#' Fit a model
#'
#' @param object A `bage_mod` object,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param ... Not currently used.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to specify a model
#' - [augment()], [components()], and [tidy()] to examine
#'   output from a model.
#' - [forecast()] to forecast, based on a model
#' 
#' @examples
#' ## specify model
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
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
fit.bage_mod <- function(object, ...) {
  object <- unfit(object)
  ## data
  nm_distn <- nm_distn(object)
  outcome <- object$outcome
  offset <- object$offset
  terms_effect <- object$terms_effect
  is_in_lik <- make_is_in_lik(object)
  terms_effectfree <- make_terms_effectfree(object)
  uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(object)
  matrices_effectfree_effect <- make_matrices_effectfree_effect(object)
  uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(object)
  offsets_effectfree_effect <- make_offsets_effectfree_effect(object)
  matrices_effect_outcome <- object$matrices_effect_outcome
  i_prior <- make_i_prior(object)
  uses_hyper <- make_uses_hyper(object)
  terms_hyper <- make_terms_hyper(object)
  uses_hyperrand <- make_uses_hyperrand(object)
  terms_hyperrand <- make_terms_hyperrand(object)
  const <- make_const(object)
  terms_const <- make_terms_const(object)
  matrices_along_by <- choose_matrices_along_by(object)
  mean_disp <- object$mean_disp
  has_disp <- mean_disp > 0
  data <- list(nm_distn = nm_distn,
               outcome = outcome,
               offset = offset,
               is_in_lik = is_in_lik,
               terms_effect = terms_effect,
               terms_effectfree = terms_effectfree,
               uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
               matrices_effectfree_effect = matrices_effectfree_effect,
               uses_offset_effectfree_effect = uses_offset_effectfree_effect,
               offsets_effectfree_effect = offsets_effectfree_effect,
               matrices_effect_outcome = matrices_effect_outcome,
               i_prior = i_prior,
               uses_hyper = uses_hyper,
               terms_hyper = terms_hyper,
               uses_hyperrand = uses_hyperrand,
               terms_hyperrand = terms_hyperrand,
               consts = const, ## 'const' is reserved word in C
               terms_consts = terms_const,
               matrices_along_by = matrices_along_by,
               mean_disp = mean_disp)
  ## parameters
  effectfree <- make_effectfree(object)
  hyper <- make_hyper(object)
  hyperrand <- make_hyperrand(object)
  log_disp <- 0
  parameters <- list(effectfree = effectfree,   
                     hyper = hyper,
                     hyperrand = hyperrand,
                     log_disp = log_disp)
  ## MakeADFun
  map <- make_map(object)
  random <- make_random(object)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ## optimise
  stats::nlminb(start = f$par,
                objective = f$fn,
                gradient = f$gr,
                silent = TRUE)
  ## extract results
  if (has_random_effects)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
  else
    sdreport <- TMB::sdreport(f) 
  est <- as.list(sdreport, what = "Est")
  attr(est, "what") <- NULL
  is_fixed <- make_is_fixed(est = est, map = map)
  if (has_random_effects)
    prec <- sdreport$jointPrecision
  else
    prec <- solve(sdreport$cov.fixed) ## should be very low dimension
  R_prec <- tryCatch(chol(prec), error = function(e) e)
  if (is.matrix(R_prec))
    object$R_prec <- R_prec
  else
    object$scaled_eigen <- make_scaled_eigen(prec)
  object$est <- est
  object$is_fixed <- is_fixed
  if (object$store_draws)
    object <- make_stored_draws(object)
  object
}


## 'forecast' -----------------------------------------------------------------

#' @importFrom generics forecast
#' @export
generics::forecast

## HAS_TESTS
#' Use a Model to Make a Forecast
#'
#' Forecast rates, probabilities, or means, and
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
#'
#' See LINK TO VIGNETTE for more details.
#'
#' @section Output:
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
#' @section Using data in forecasts:
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
#' designing a model. See LINK TO WORKFLOW VIGNETTE
#' for details.
#'
#' @section Warning:
#'
#' The interface for `forecast()` has not been finalised
#' and is likely to change in the near future.
#'
#' @inheritParams components.bage_mod
#' @param labels Labels for future values. WARNING
#' this argument is only temporary.
#' @param output Type of output returned
#' @param include_estimates Whether to
#' include historical estimates along
#' with the forecasts. Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @returns
#' A [tibble][tibble::tibble-package].
#'
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to specify a model
#' - [bage::fit()] to fit a model
#'
#' @examples
#' ## specify and fit model
#' mod <- mod_pois(injuries ~ age * sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn) |>
#'   fit(mod)
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
#' ## forecast based on priors only
#' mod_unfitted <- mod_pois(injuries ~ age * sex + ethnicity + year,
#'                          data = injuries,
#'                          exposure = popn)
#' mod_unfitted |>
#'   forecast(labels = 2019:2024)
#' @export    
forecast.bage_mod <- function(object,
                              output = c("augment", "components"),
                              include_estimates = FALSE,
                              labels,
                              ...) {
  check_flag(x = include_estimates, nm_x = "include_estimates")
  output <- match.arg(output)
  var_time <- object$var_time
  if (is.null(var_time))
    cli::cli_abort(c("Can't forecast when time variable not identified.",
                     i = "Use {.fun set_var_time} to identify time variable?"))
  check_along_is_time(object)
  components_est <- components(object)
  seed_forecast_components <- object$seed_forecast_components
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_forecast_components) ## set pre-determined seed
  components_forecast <- forecast_components(mod = object,
                                             components_est = components_est,
                                             labels_forecast = labels)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  if (output == "augment") {
    seed_forecast_augment <- object$seed_forecast_augment
    seed_restore <- make_seed() ## create randomly-generated seed
    set.seed(seed_forecast_augment) ## set pre-determined seed
    ans <- forecast_augment(mod = object,
                            components_est = components_est,
                            components_forecast = components_forecast,
                            labels_forecast = labels)
    set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
    if (include_estimates) {
      augment_est <- augment(object)
      ans <- vctrs::vec_rbind(augment_est, ans)
    }
  }
  else if (output == "components") {
    ans <- components_forecast
    if (include_estimates) {
      term_est <- components_est$term
      level_est <- components_est$level
      is_time_varying <- make_is_time_varying(term = term_est,
                                              level = level_est,
                                              var_time = var_time)
      time_varying_est <- components_est[is_time_varying, ]
      ans <- vctrs::vec_rbind(time_varying_est, ans)
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
#' @param components_est Tibble with results
#' of call to 'components'.
#' @param components_forecast Tibble with results
#' of call to 'forecast_components'.
#' @param labels_forecast Vector
#' with labels for future time periods
#'
#' @returns A tibble.
#'
#' @noRd
forecast_augment <- function(mod,
                             components_est,
                             components_forecast,
                             labels_forecast) {
  UseMethod("forecast_augment")
}

## HAS_TESTS
#' @export
forecast_augment.bage_mod <- function(mod,
                                      components_est,
                                      components_forecast,
                                      labels_forecast) {
  formula <- mod$formula
  has_disp <- has_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  data_forecast <- make_data_forecast(mod = mod,
                                      labels_forecast = labels_forecast)
  is_effect <- components_forecast$component == "effect"
  effects <- components_forecast[is_effect, ]
  matrices_effect_outcome <- make_matrices_effect_outcome(formula = formula,
                                                          data = data_forecast)
  matrix_effect_outcome <- Reduce(Matrix::cbind2, matrices_effect_outcome)
  matrix_effect_outcome <- Matrix::as.matrix(matrix_effect_outcome)
  linpred <- matrix_effect_outcome %*% effects$.fitted
  if (has_disp) {
    expected <- inv_transform(linpred)
    is_disp <- components_est$component == "disp"
    disp <- components_est$.fitted[is_disp]
    fitted <- draw_vals_fitted(mod = mod,
                               vals_expected = expected,
                               vals_disp = disp)
  }
  else
    fitted <- inv_transform(linpred)
  ans <- data_forecast
  ans$.observed <- NA_real_
  ans$.fitted <- fitted
  if (has_disp)
    ans$.expected <- expected
  ans
}

## HAS_TESTS
#' @export
forecast_augment.bage_mod_norm <- function(mod,
                                           components_est,
                                           components_forecast,
                                           labels_forecast) {
  formula <- mod$formula
  scale_outcome <- get_fun_scale_outcome(mod)
  data_forecast <- make_data_forecast(mod = mod,
                                      labels_forecast = labels_forecast)
  is_effect <- components_forecast$component == "effect"
  effects <- components_forecast[is_effect, ]
  matrices_effect_outcome <- make_matrices_effect_outcome(formula = formula,
                                                          data = data_forecast)
  matrix_effect_outcome <- Reduce(Matrix::cbind2, matrices_effect_outcome)
  matrix_effect_outcome <- Matrix::as.matrix(matrix_effect_outcome)
  linpred <- matrix_effect_outcome %*% effects$.fitted
  fitted <- scale_outcome(linpred)
  ans <- data_forecast
  ans$.fitted <- fitted
  ans
}



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


## 'get_fun_scale_outcome' ----------------------------------------------------

#' Get function to scale outcome, if necessary
#'
#' Get function to scale outcome, if necessary.
#' The scaling consists of multiplying by the sd
#' of the original outcome, and then adding the
#' mean. Applied only to the normal model.
#' In other cases, the function returned is the
#' identity function.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
get_fun_scale_outcome <- function(mod) {
    UseMethod("get_fun_scale_outcome")
}

#' @export
get_fun_scale_outcome.bage_mod <- function(mod) identity

#' @export
get_fun_scale_outcome.bage_mod_norm <- function(mod) {
    mean <- mod$outcome_mean
    sd <- mod$outcome_sd
    function(x) x * sd + mean
}


## 'get_nm_outcome' -----------------------------------------------------------

#' Get the Name of the Outcome Variable
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A string
#'
#' @noRd
get_nm_outcome <- function(mod) {
    UseMethod("get_nm_outcome")
}

## HAS_TESTS
#' @export
get_nm_outcome.bage_mod <- function(mod) {
  formula <- mod$formula
  ans <- formula[[2L]]
  ans <- deparse1(ans)
  ans
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
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
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
  !is.null(x$est)


## 'make_par_disp' ---------------------------------------------------

#' Make Random Draws of '.fitted' in Models
#' with Dispersion term
#'
#' @param x Fitted object of class 'bage_mod'.
#' @param disp An rvec of length 1 with
#' posterior distribution for
#' dispersion term.
#'
#' @returns An rvec
#'
#' @noRd
make_par_disp <- function(x,
                                meanpar,
                                disp) {
    UseMethod("make_par_disp")
}

## HAS_TESTS
#' @export
make_par_disp.bage_mod_pois <- function(x,
                                        meanpar,
                                        disp) {
  outcome <- x$outcome
  offset <- x$offset
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  rvec::rgamma_rvec(n = length(outcome),
                    shape = outcome + 1 / disp,
                    rate = offset + 1 / (disp * meanpar))
}

## HAS_TESTS
#' @export
make_par_disp.bage_mod_binom <- function(x,
                                         meanpar,
                                         disp) {
  outcome <- x$outcome
  offset <- x$offset
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  rvec::rbeta_rvec(n = length(outcome),
                   shape1 = outcome + meanpar / disp,
                   shape2 = offset - outcome + (1 - meanpar) / disp)
}



## 'make_subseries' -----------------------------------------------------------



make_subseries.bage_prior_rwseasfix <- function(prior,
                                                hyperrand,
                                                effect,
                                                matrix_along_by) {
  NULL
}
  
  



## 'make_observed' ------------------------------------------------------------

#' Make direct estimates
#'
#' @param x A fitted 'bage_mod' object.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_observed <- function(x) {
    UseMethod("make_observed")
}
              
## HAS_TESTS
#' @export
make_observed.bage_mod <- function(x) {
    outcome <- x$outcome
    offset <- x$offset
    ans <- as.double(outcome / offset)
    ans
}

## HAS_TESTS
#' @export
make_observed.bage_mod_norm <- function(x) {
    cli::cli_abort(paste("Internal error: {.fun make_observed} called on object",  ## nocov
                         "of class {.cls {class(x)}}."))                           ## nocov
}


## 'model_descr' -----------------------------------------------------------------

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


## 'nm_distn' -----------------------------------------------------------------

#' Name of offset used in printing
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
nm_offset <- function(mod) {
    UseMethod("nm_offset")
}

## HAS_TESTS
#' @export
nm_offset.bage_mod_pois <- function(mod) "exposure"

## HAS_TESTS
#' @export
nm_offset.bage_mod_binom <- function(mod) "size"

## HAS_TESTS
#' @export
nm_offset.bage_mod_norm <- function(mod) "weights"


## 'print' --------------------------------------------------------------------

#' @export
print.bage_mod <- function(x, ...) {
    nchar_offset <- 15
    ## calculations
    formula <- x$formula
    priors <- x$priors
    n_draw <- x$n_draw
    data <- x$data
    vname_offset <- x$vname_offset
    var_age <- x$var_age
    var_sexgender <- x$var_sexgender
    var_time <- x$var_time
    mean_disp <- x$mean_disp
    is_fitted <- is_fitted(x)
    str_title <- sprintf("-- %s %s model --",
                         if (is_fitted) "Fitted" else "Unfitted",
                         model_descr(x))
    nms_priors <- names(priors)
    nchar_response <- nchar(as.character(formula[[2L]]))
    nchar_max <- max(nchar(nms_priors), nchar_response)
    padding_formula <- paste(rep(" ", nchar_max - nchar_response),
                             collapse = "")
    nms_priors <- sprintf("% *s", nchar_max, nms_priors)
    calls_priors <- vapply(priors, str_call_prior, "")
    str_priors <- paste(nms_priors, calls_priors, sep = " ~ ")
    str_priors <- paste(str_priors, collapse = "\n")
    str_disp <- sprintf("% *s: mean=%s", nchar_offset, "dispersion", mean_disp)
    has_offset <- !is.null(vname_offset)
    if (has_offset) {
        nm_offset <- nm_offset(x)
        nm_offset <- sprintf("% *s", nchar_offset, nm_offset)
        str_offset <- sprintf("%s: %s", nm_offset, vname_offset)
    }        
    ## printing
    cat(str_title)
    cat("\n\n")
    cat(padding_formula)
    cat(paste(deparse(formula), collapse = "\n"))
    cat("\n\n")
    cat(str_priors)
    cat("\n\n")
    cat(str_disp)
    cat("\n")
    if (has_offset) {
        cat(str_offset)
        cat("\n")
    }
    if (!is.null(var_age)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_age",
                    var_age))
        cat("\n")
    }
    if (!is.null(var_sexgender)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_sexgender",
                    var_sexgender))
        cat("\n")
    }
    if (!is.null(var_time)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_time",
                    var_time))
        cat("\n")
    }
    if (has_offset) {
        cat(sprintf("% *s: %d",
                    nchar_offset,
                    "n_draw",
                    n_draw))
        cat("\n")
    }
    ## return
    invisible(x)
}


#' Draw from the Prior Predictive Distribution of
#' a Model
#'
#' Draw from the prior predictive distribution
#' of a model, i.e., the 


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
#' @section: The `condition_on` argument
#'
#' With Poisson and binomial models that include
#' dispersion terms (which is the default), there are
#' two options for constructing replicate data.
#'
#' - When `condition_on` is `"par"`,
#' the replicate data is created by (i) drawing values
#' from the posterior distribution for rates or probabilities
#' (the \eqn{\gamma_i} defined in [mod_pois()]
#' and [mod_binom()]), and (ii)  conditional on these
#' rates or probabilities, drawing values for the 
#' outcome variable.
#' - When `condition_on` is `"meanpar"`,
#' the replicate data is created by (i) drawing
#' values from hyper-parameters governing
#' the rates or probabilities 
#' (the \eqn{\mu_i} and \eqn{\xi} defined
#' in [mod_pois()] and [mod_binom()]),
#' then (ii) conditional on these hyper-parameters,
#' drawing values for the rates or probabilties,
#' and finally (iii) conditional on these
#' rates or probabilities, drawing values for the 
#' outcome variable.
#'
#' The default for `condition_on` is `"meanpar"`.
#' The `"meanpar"` option
#' provides a more severe test for
#' a model than the `"par"` option,
#' since "par" values are weighted averages
#' of the "meanpar" values and the original
#' data.
#'
#' As described in [mod_norm()], normal models
#' have a slightly different structure from Poisson
#' and binomial models, and the distinction between
#' `"par"` and `"meanpar"` does not apply.
#'
#' @param x A fitted model, typically created by
#' calling [mod_pois()], [mod_binom()], or [mod_norm()],
#' and then [fit()].
#' @param condition_on Parameters to condition
#' on. Either `"meanpar"` or `"par"`. See
#' details.
#' @param n Number of replicate datasets to create.
#' Default is 19.
#'
#' @returns A tibble with the following structure:
#'
#' |`.replicate`     | data                           |
#' |-----------------|--------------------------------|
#' |`"Original"      | Original data supplied to [mod_pois()], [mod_binom()], [mod_norm()] |
#' |`"Replicate 1"`  | Original data, except that actual outcome replaced by simulated values. |
#' |`"Replicate 2"`  | Original data, except that actual outcome replaced by simulated values. |
#' |\dots            | \dots                          |
#' |`"Replicate <n>"`| Original data, except that actual outcome replaced by simulated values. |
#' 
#' 
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to create models
#' - [fit()] to fit models
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
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
#' @export
replicate_data <- function(x, condition_on = NULL, n = 19) {
    UseMethod("replicate_data")
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_pois <- function(x, condition_on = NULL, n = 19) {
    if (is.null(condition_on))
        condition_on <- "meanpar"
    else
        condition_on <- match.arg(condition_on, choices = c("meanpar", "par"))
    check_n(n = n,
            nm_n = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    outcome <- x$outcome
    offset <- x$offset
    nm_outcome <- get_nm_outcome(x)
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    n_obs <- nrow(data)
    if (condition_on == "par") {
        par <- aug$.fitted
        lambda <- offset * par
        y_rep <- rvec::rpois_rvec(n = n_obs,
                                  lambda = lambda)
    }
    else if (condition_on == "meanpar") {
        check_has_disp_if_condition_on_meanpar(x)
        meanpar <- aug$.expected
        comp <- components(x)
        disp <- comp[[".fitted"]][comp$component == "disp"]
        size <- 1 / disp
        mu <- offset * meanpar
        y_rep <- rvec::rnbinom_rvec(n = n_obs,
                                    size = size,
                                    mu = mu)
    }
    else
        cli::cli_abort("Internal error: Invalid value for 'condition_on'.") ## nocov
    outcome_rep <- c(outcome, as.numeric(y_rep))
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_binom <- function(x, condition_on = NULL, n = 19) {
    if (is.null(condition_on))
        condition_on <- "meanpar"
    else
        condition_on <- match.arg(condition_on, choices = c("meanpar", "par"))
    check_n(n = n,
            nm_n = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    formula <- x$formula
    outcome <- x$outcome
    offset <- x$offset
    nm_outcome <- get_nm_outcome(x)
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    n_obs <- nrow(data)
    if (condition_on == "par") {
        par <- aug$.fitted
        y_rep <- rvec::rbinom_rvec(n = n_obs,
                                   size = offset,
                                   prob = par)
    }
    else if (condition_on == "meanpar") {
        check_has_disp_if_condition_on_meanpar(x)
        meanpar <- aug$.expected
        comp <- components(x)
        disp <- comp[[".fitted"]][comp$component == "disp"]
        shape1 <- meanpar / disp
        shape2 <- (1 - meanpar) / disp
        prob <- rvec::rbeta_rvec(n = n_obs,
                                 shape1 = shape1,
                                 shape2 = shape2)
        y_rep <- rvec::rbinom_rvec(n = n_obs,
                                   size = offset,
                                   prob = prob)
    }
    else
        cli::cli_abort("Internal error: Invalid value for 'condition_on'.") ## nocov
    outcome_rep <- c(outcome, as.numeric(y_rep))
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_norm <- function(x, condition_on = NULL, n = 19) {
    if (!is.null(condition_on))
        cli::cli_warn(c("Ignoring value for {.arg condition_on}.",
                        i = paste("{.fun replicate_data} ignores argument {.arg condition_on}",
                                  "when model {.arg x} has a normal likelihood.")))
    check_n(n = n,
            nm_n = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    formula <- x$formula
    outcome <- x$outcome
    offset <- x$offset
    nm_outcome <- get_nm_outcome(x)
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    comp <- components(x)
    disp <- comp[[".fitted"]][comp$component == "disp"]
    n_obs <- nrow(data)
    par <- aug$.fitted
    y_rep <- rvec::rnorm_rvec(n = n_obs,
                              mean = par,
                              sd = disp / sqrt(offset))
    outcome_rep <- c(outcome, as.numeric(y_rep))
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
}


## 'tidy' ---------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

## HAS_TESTS
#' Main effects and interactions from a fitted model
#'
#' @param x A fitted `bage_mod` object.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @seealso [glimpse()] provides less detailed information,
#' and [augment()] provides more detailed.
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod <- fit(mod)
#' tidy(mod)
#' @export
tidy.bage_mod <- function(x, ...) {
    priors <- x$priors
    n <- x$lengths_effect
    terms <- x$terms_effect
    term <- names(priors)
    spec <- vapply(priors, str_call_prior, "")
    ans <- tibble::tibble(term, spec, n)
    is_fitted <- is_fitted(x)
    if (is_fitted) {
        effectfree <- x$est$effectfree
        matrix <- make_combined_matrix_effectfree_effect(x)
        offset <- make_combined_offset_effectfree_effect(x)
        effect <- matrix %*% effectfree + offset
        effect <- split(effect, terms)
        ans[["sd"]] <- vapply(effect, stats::sd, 0)
    }
    ans <- tibble::tibble(ans)
    ans
}

