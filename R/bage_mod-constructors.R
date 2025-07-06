
## HAS_TESTS
#' Specify a Poisson Model
#'
#' Specify a model where the outcome is drawn from a Poisson distribution.
#'
#' The model is hierarchical. The rates in the Poisson distribution
#' are described by a prior model formed from dimensions such
#' as age, sex, and time. The terms for these dimension themselves
#' have models, as described in [priors]. These priors all have defaults,
#' which depend on the type of term (eg an intercept, an age main effect,
#' or an age-time interaction.)
#'
#' @section Specifying exposure:
#'
#' The `exposure` argument can take three forms:
#'
#' - the name of a variable in `data`, with or without
#'   quote marks, eg `"population"` or `population`;
#' - the number `1`, in which case a pure "counts" model
#'   with no exposure, is produced; or
#' - a formula, which is evaluated with `data` as its
#'   environment (see below for example).
#' 
#' @section Mathematical details:
#'
#' The likelihood is
#' 
#' \deqn{y_i \sim \text{Poisson}(\gamma_i w_i)}
#'
#' where
#'
#' - subscript \eqn{i} identifies some combination of the
#'   classifying variables, such as age, sex, and time;
#' - \eqn{y_i} is an outcome, such as deaths;
#' - \eqn{\gamma_i} is rates; and
#' - \eqn{w_i} is exposure.
#'
#' In some applications, there is no obvious population at risk.
#' In these cases, exposure \eqn{w_i} can be set to 1
#' for all \eqn{i}.
#'
#' The rates \eqn{\gamma_i} are assumed to be drawn 
#' a gamma distribution
#'
#' \deqn{y_i \sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1})}
#'
#' where
#'
#' - \eqn{\mu_i} is the expected value for \eqn{\gamma_i}; and
#' - \eqn{\xi} governs dispersion (i.e. variation), with
#'   lower values implying less dispersion.
#'
#' Expected value \eqn{\mu_i} equals, on the log scale,
#' the sum of terms formed from classifying variables,
#'
#' \deqn{\log \mu_i = \sum_{m=0}^{M} \beta_{j_i^m}^{(m)}}
#'
#' where
#'
#' - \eqn{\beta^{0}} is an intercept;
#' - \eqn{\beta^{(m)}}, \eqn{m = 1, \dots, M}, is a main effect
#'   or interaction; and
#' - \eqn{j_i^m} is the element of \eqn{\beta^{(m)}} associated with
#'   cell \eqn{i}.
#'
#' The \eqn{\beta^{(m)}} are given priors, as described in [priors].
#'
#' \eqn{\xi} has an exponential prior with mean 1. Non-default
#' values for the mean can be specified with  [set_disp()].
#'
#' The model for \eqn{\mu_i}
#' can also include covariates,
#' as described in [set_covariates()].
#'
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing outcome,
#' predictor, and, optionally, exposure variables.
#' @param exposure Name of the exposure variable,
#' or a `1`, or a formula. See below for details.
#'
#' @returns An object of class `bage_mod_pois`.
#'
#' @seealso
#' - [mod_binom()] Specify binomial model
#' - [mod_norm()] Specify normal model
#' - [set_prior()] Specify non-default prior for term
#' - [set_disp()] Specify non-default prior for dispersion
#' - [fit()] Fit a model
#' - [augment()] Extract values for rates,
#'   together with original data
#' - [components()] Extract values for hyper-parameters
#' - [forecast()] Forecast parameters and outcomes
#' - [report_sim()] Check model using a simulation study
#' - [replicate_data()] Check model using replicate data
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig2_math.html)
#'   Detailed description of models
#'
#' @examples
#' ## specify a model with exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#'
#' ## specify a model without exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = 1)
#'
#' ## use a formula to specify exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = ~ pmax(popn, 1))
#' @export
mod_pois <- function(formula,
                     data,
                     exposure) {
  ## processing common to all models
  args <- mod_helper(formula = formula,
                     data = data,
                     n_draw = 1000L)
  ## input checks specific to Poisson
  check_response_nonneg(formula = formula,
                        data = data,
                        nm_distn = "Poisson")
  ## process 'exposure'
  exposure <- deparse1(substitute(exposure))
  exposure <- gsub("^\\\"|\\\"$", "", exposure)
  is_offset_specified <- !identical(exposure, "1")
  nm_offset_data <- if (is_offset_specified) exposure else NULL
  if (is_offset_specified) {
    check_offset_in_data(nm_offset_data = nm_offset_data,
                         nm_offset_mod = "exposure",
                         data = data)
    check_offset_nonneg(nm_offset_data = nm_offset_data,
                        nm_offset_mod = "exposure",
                        data = data)
    check_resp_zero_if_offset_zero(formula = formula,
                                   nm_offset_data = nm_offset_data,
                                   nm_offset_mod = "exposure",
                                   data = data)
    check_offset_not_in_formula(nm_offset_data = nm_offset_data,
                                nm_offset_mod = "exposure",
                                formula = formula)
    offset <- make_offset(nm_offset_data = nm_offset_data,
                          data = data)
  }
  else
    offset <- make_offset_ones(data)
  ## check for suspicious rates
  outcome <- args$outcome
  mult_high_rate <- 1000
  message_suspicious_rates(outcome = outcome,
                           exposure = offset,
                           mult_high_rate = mult_high_rate)
  ## create object and return
  ans <- c(args,
           list(offset = offset,
                nm_offset_data = nm_offset_data))
  class(ans) <- c("bage_mod_pois", "bage_mod")
  ans
}


## HAS_TESTS
#' Specify a Binomial Model
#'
#' Specify a model where the outcome is drawn from
#' a binomial distribution.
#'
#' The model is hierarchical. The probabilities in the binomial distribution
#' are described by a prior model formed from dimensions such
#' as age, sex, and time. The terms for these dimension themselves
#' have models, as described in [priors]. These priors all have defaults,
#' which depend on the type of term (eg an intercept, an age main effect,
#' or an age-time interaction.)
#'
#' @section Specifying size:
#'
#' The `size` argument can take two forms:
#'
#' - the name of a variable in `data`, with or without
#'   quote marks, eg `"population"` or `population`; or
#' - a formula, which is evaluated with `data` as its
#'   environment (see below for example).
#' 
#' @section Mathematical details:
#'
#' The likelihood is
#' 
#' \deqn{y_i \sim \text{binomial}(\gamma_i; w_i)}
#'
#' where
#'
#' - subscript \eqn{i} identifies some combination of the the
#'   classifying variables, such as age, sex, and time;
#' - \eqn{y_i} is a count, such of number of births,
#'   such as age, sex, and region;
#' - \eqn{\gamma_i} is a probability of 'success'; and
#' - \eqn{w_i} is the number of trials.
#'
#' The probabilities \eqn{\gamma_i} are assumed to be drawn 
#' a beta distribution
#'
#' \deqn{y_i \sim \text{Beta}(\xi^{-1} \mu_i, \xi^{-1} (1 - \mu_i))}
#'
#' where
#'
#' - \eqn{\mu_i} is the expected value for \eqn{\gamma_i}; and
#' - \eqn{\xi} governs dispersion (ie variance.)
#'
#' Expected value \eqn{\mu_i} equals, on a logit scale,
#' the sum of terms formed from classifying variables,
#'
#' \deqn{\text{logit} \mu_i = \sum_{m=0}^{M} \beta_{j_i^m}^{(m)}}
#'
#' where
#'
#' - \eqn{\beta^{0}} is an intercept;
#' - \eqn{\beta^{(m)}}, \eqn{m = 1, \dots, M}, is a main effect
#'   or interaction; and
#' - \eqn{j_i^m} is the element of \eqn{\beta^{(m)}} associated with
#'   cell \eqn{i}.
#'
#' The \eqn{\beta^{(m)}} are given priors, as described in [priors].
#'
#' \eqn{\xi} has an exponential prior with mean 1. Non-default
#' values for the mean can be specified with  [set_disp()].
#'
#' The model for \eqn{\mu_i}
#' can also include covariates,
#' as described in [set_covariates()].
#' 
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing the outcome
#' and predictor variables, and the number of trials.
#' @param size Name of the variable giving
#' the number of trials, or a formula.
#'
#' @returns An object of class `bage_mod`.
#'
#' @seealso
#' - [mod_pois()] Specify Poisson model
#' - [mod_norm()] Specify normal model
#' - [set_prior()] Specify non-default prior for term
#' - [set_disp()] Specify non-default prior for dispersion
#' - [fit()] Fit a model
#' - [augment()] Extract values for probabilities,
#'   together with original data
#' - [components()] Extract values for hyper-parameters
#' - [forecast()] Forecast parameters and outcomes
#' - [report_sim()] Check model using simulation study
#' - [replicate_data()] Check model using replicate data
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig2_math.html)
#'   Detailed descriptions of models
#'
#' @examples
#' mod <- mod_binom(oneperson ~ age:region + age:year,
#'                  data = nzl_households,
#'                  size = total)
#'
#' ## use formula to specify size
#' mod <- mod_binom(ncases ~ agegp + tobgp + alcgp,
#'                  data = esoph,
#'                  size = ~ ncases + ncontrols)
#' @export
mod_binom <- function(formula, data, size) {
  ## processing common to all models
  args <- mod_helper(formula = formula,
                     data = data,
                     n_draw = 1000L)
  ## input checks specific to binomial
  check_response_nonneg(formula = formula,
                        data = data,
                        nm_distn = "Binomial")
  ## process 'size'
  size <- deparse1(substitute(size))
  size <- gsub("^\\\"|\\\"$", "", size)
  nm_offset_data <- size
  check_offset_in_data(nm_offset_data = nm_offset_data,
                       nm_offset_mod = "size",
                       data = data)
  check_offset_nonneg(nm_offset_data = nm_offset_data,
                      nm_offset_mod = "size",
                      data = data)
  check_offset_not_in_formula(nm_offset_data = nm_offset_data,
                              nm_offset_mod = "size",
                              formula = formula)
  check_resp_zero_if_offset_zero(formula = formula,
                                 nm_offset_data = nm_offset_data,
                                 nm_offset_mod = "size",
                                 data = data)
  check_resp_le_offset(formula = formula,
                       nm_offset_data = nm_offset_data,
                       nm_offset_mod = "size",
                       data = data)
  offset <- make_offset(nm_offset_data = nm_offset_data,
                        data = data)
  ## create object and return
  ans <- c(args,
           list(offset = offset,
                nm_offset_data = nm_offset_data))
  class(ans) <- c("bage_mod_binom", "bage_mod")
  ans
}


## HAS_TESTS
#' Specify a Normal Model
#'
#' Specify a model where the outcome is drawn from
#' a normal distribution.
#' 
#' The model is hierarchical. The means in the normal distribution
#' are described by a prior model formed from dimensions such
#' as age, sex, and time. The terms for these dimension themselves
#' have models, as described in [priors]. These priors all have defaults,
#' which depend on the type of term (eg an intercept, an age main effect,
#' or an age-time interaction.)
#'
#' @section Scaling of outcome and weights:
#'
#' Internally, `mod_norm()` scales the outcome variable
#' to have mean 0 and standard deviation 1, and
#' scales the weights to have mean 1.
#' This scaling allows `mod_norm()` to use the
#' same menu of priors as [mod_pois()] and [mod_binom()].
#'
#' [augment()] always returns values on the
#' original scale, rather than the transformed scale.
#'
#' [components()] by default returns values on
#' the transformed scale. But if `original_scale` is
#' `TRUE`, it returns some types of values on the
#' original scale. See [components()] for details.
#' 
#' @section Specifying weights:
#'
#' The `weights` argument can take three forms:
#'
#' - the name of a variable in `data`, with or without
#'   quote marks, eg `"wt"` or `wt`;
#' - the number `1`, in which no weights are used; or
#' - a formula, which is evaluated with `data` as its
#'   environment (see below for example).
#'
#'
#' @section Mathematical details:
#'
#' The likelihood is
#'
#' \deqn{y_i \sim \text{N}(\gamma_i, w_i^{-1} \sigma^2)}
#' where
#' - subscript \eqn{i} identifies some combination of the
#'   classifying variables, such as age, sex, and time,
#' - \eqn{y_i} is the value of the outcome variable,
#' - \eqn{w_i} is a weight.
#' 
#' In some applications, \eqn{w_i} is set to 1
#' for all \eqn{i}.
#'
#' Internally, **bage** works with standardized
#' versions of \eqn{\gamma_i} and \eqn{\sigma^2}:
#'
#' \deqn{\mu_i = (\gamma_i - \bar{y}) / s}
#' \deqn{\xi^2 = \sigma^2 / (\bar{w} s^2)}
#' where
#' \deqn{\bar{y} = \sum_{i=1}^n y_i / n}
#' \deqn{s = \sqrt{\sum_{i=1}^n (y_i - \bar{y})^2 / (n-1)}}
#' \deqn{\bar{w} = \sum_{i=1}^n w_i / n}
#'
#' Mean parameter \eqn{\mu_i} is modelled as
#' the sum of terms formed
#' from classifying variables and covariates,
#'
#' \deqn{\mu_i = \sum_{m=0}^{M} \beta_{j_i^m}^{(m)}}
#'
#' where
#'
#' - \eqn{\beta^{0}} is an intercept;
#' - \eqn{\beta^{(m)}}, \eqn{m = 1, \dots, M}, is a main effect
#'   or interaction; and
#' - \eqn{j_i^m} is the element of \eqn{\beta^{(m)}} associated with
#'   cell \eqn{i},
#'
#' The \eqn{\beta^{(m)}} are given priors, as described in [priors].
#'
#' \eqn{\xi} has an exponential prior with mean 1. Non-default
#' values for the mean can be specified with [set_disp()].
#'
#' The model for \eqn{\mu_i}
#' can also include covariates,
#' as described in [set_covariates()].
#' 
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing outcome,
#' predictor, and, optionally, weights variables.
#' @param weights Name of the weights variable,
#' a `1`, or a formula. See below for details.
#'
#' @returns An object of class `bage_mod_norm`.
#'
#' @seealso
#' - [mod_pois()] Specify Poisson model
#' - [mod_binom()] Specify binomial model
#' - [set_prior()] Specify non-default prior for term
#' - [set_disp()] Specify non-default prior for standard deviation
#' - [fit()] Fit a model
#' - [augment()] Extract values for means,
#'   together with original data
#' - [components()] Extract values for hyper-parameters
#' - [forecast()] Forecast parameters and outcomes
#' - [report_sim()] Check model using a simulation study
#' - [replicate_data()] Check model using replicate data
#'   data for a model
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig2_math.html)
#'   Detailed description of models
#'
#' @examples
#' mod <- mod_norm(value ~ diag:age + year,
#'                 data = nld_expenditure,
#'                 weights = 1)
#'
#' ## use formula to specify weights
#' mod <- mod_norm(value ~ diag:age + year,
#'                 data = nld_expenditure,
#'                 weights = ~sqrt(value))
#' @export
mod_norm <- function(formula, data, weights) {
  ## processing common to all models
  args <- mod_helper(formula = formula,
                     data = data,
                     n_draw = 1000L)
  ## process 'weights'
  weights <- deparse1(substitute(weights))
  weights <- gsub("^\\\"|\\\"$", "", weights)
  is_offset_specified <- !identical(weights, "1")
  nm_offset_data <- if (is_offset_specified) weights else NULL
  if (is_offset_specified) {
    check_offset_in_data(nm_offset_data = nm_offset_data,
                         nm_offset_mod = "weights",
                         data = data)
    check_offset_nonneg(nm_offset_data = nm_offset_data,
                        nm_offset_mod = "weights",
                        data = data)
    check_offset_not_in_formula(nm_offset_data = nm_offset_data,
                                nm_offset_mod = "weights",
                                formula = formula)
    offset <- make_offset(nm_offset_data = nm_offset_data,
                          data = data)
  }
  else
    offset <- make_offset_ones(data)
  offset_mean <- mean(offset, na.rm = TRUE)
  offset <- offset / offset_mean
  ## process outcome
  outcome <- args[["outcome"]]
  n_obs <- sum(!is.na(outcome))
  if (n_obs == 0L)
    outcome_mean <- 0
  else
    outcome_mean <- mean(outcome, na.rm = TRUE)
  if (n_obs <= 1L)
    outcome_sd <- 1
  else
    outcome_sd <- stats::sd(outcome, na.rm = TRUE)
  outcome <- (outcome - outcome_mean) / outcome_sd
  args[["outcome"]] <- outcome
  ## create object and return
  ans <- c(args,
           list(offset = offset,
                nm_offset_data = nm_offset_data,
                offset_mean = offset_mean,
                outcome_mean = outcome_mean,
                outcome_sd = outcome_sd))
  class(ans) <- c("bage_mod_norm", "bage_mod")
  ans
}


## HAS_TESTS
#' Helper Function for Model Constructors
#'
#' Deals with tasks common across all
#' three models
#' 
#' @param formula An R formula
#' @param data A data frame
#' @param n_draw Positive integer
#'
#' @returns A named list
#'
#' @noRd
mod_helper <- function(formula, data, n_draw) {
  ## check individual inputs
  check_is_formula(formula)
  check_formula_has_response(formula)
  check_formula_has_intercept(formula)
  check_is_dataframe(x = data, nm_x = "data")
  ## check consistency between inputs
  check_formula_vnames_in_data(formula = formula,
                               data = data)
  ## process inputs
  data <- tibble(data)
  dimnames_terms <- make_dimnames_terms(formula = formula,
                                        data = data)
  outcome <- make_outcome(formula = formula,
                          data = data)
  var_age <- infer_var_age(formula)
  var_sexgender <- infer_var_sexgender(formula)
  var_time <- infer_var_time(formula)
  lengths_effect <- make_lengths_effect(dimnames_terms)
  priors <- make_priors(formula = formula,
                        var_age = var_age,
                        var_time = var_time,
                        lengths_effect = lengths_effect)
  seed_components <- make_seed()
  seed_augment <- make_seed()
  seed_forecast_components <- make_seed()
  seed_forecast_augment <- make_seed()
  ## create list of arguments and return
  list(formula = formula,
       data = data,
       outcome = outcome,
       datamod_outcome = NULL,
       priors = priors,
       dimnames_terms = dimnames_terms,
       var_age = var_age,
       var_sexgender = var_sexgender,
       var_time = var_time,
       mean_disp = 1,
       formula_covariates = NULL,
       covariates_nms = NULL,
       n_draw = n_draw,
       vars_inner = NULL,
       draws_effectfree = NULL,
       draws_hyper = NULL,
       draws_hyperrandfree = NULL,
       draws_coef_covariates = NULL,
       draws_disp = NULL,
       point_effectfree = NULL,
       point_hyper = NULL,
       point_hyperrandfree = NULL,
       point_coef_covariates = NULL,
       point_disp = NULL,
       seed_components = seed_components,
       seed_augment = seed_augment,
       seed_forecast_components = seed_forecast_components,
       seed_forecast_augment = seed_forecast_augment,
       optimizer = NULL,
       computations = NULL,
       oldpar = NULL)
}
