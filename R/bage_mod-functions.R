
## User-visible functions that look like methods, but technically are not

## 'set_confidential_rr3' -----------------------------------------------------

#' Specify RR3 Confidentialization
#'
#' Specify a confidentialization procedure
#' where the outcome variable
#' is randomly rounded to a multiple of 3.
#'
#' `set_confidential_rr3()` can only be used with
#' Poisson and binomial models (created with
#' [mod_pois()] and [mod_binom()].)
#'
#' Random rounding to base 3 (RR3) is a confidentialization
#' technique that is sometimes applied by statistical
#' agencies. The procedure for randomly-rounding
#' an integer value \eqn{n} is as follows:
#'
#' - If \eqn{n} is divisible by 3, leave it unchanged
#' - If dividing \eqn{n} by 3 leaves a remainder of 1, then
#'   round down (subtract 1) with probability 2/3,
#'   and round up (add 2) with probability 1/3.
#' - If dividing \eqn{n} by 3 leaves a remainder of 1,
#'   then round down (subtract 2)
#'   with probability 1/3, and round up (add 1)
#'   with probability 2/3.
#'
#' If `set_confidential_rr3()` is applied to
#' a fitted model, `set_confidential_rr3()`
#' [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [confidential] Overview of confidentialization procedures
#'   currently modeled in **bage**
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#'   model for rates, probabilities, or means
#'
#' @examples
#' ## 'injuries' variable in 'nzl_injuries' dataset
#' ## has been randomly rounded to base 3
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   set_confidential_rr3() |>
#'   fit()
#' @export
set_confidential_rr3 <- function(mod) {
  check_bage_mod(x = mod, nm_x = "mod")
  ## check is valid distribution
  valid_distn <- c("binom", "pois")
  nm_distn <- nm_distn(mod)
  if (!(nm_distn %in% valid_distn))
    cli::cli_abort(c("Outcome has {.val {nm_distn}} distribution.",
                     i = paste("RR3 can only be used with",
                               "{.val {valid_distn}} distributions.")))
  ## check that values for outcome all divisible by 3
  outcome <- mod$outcome
  is_base3 <- is.na(outcome) | ((outcome %% 3L) == 0L)
  n_not_base3 <- sum(!is_base3)
  if (n_not_base3 > 0L)
    cli::cli_abort(paste("Outcome variable has {cli::qty(n_not_base3)}",
                         "value{?s} not divisible by 3."))
  ## return
  mod$confidential <- new_bage_confidential_rr3()
  mod <- unfit(mod)
  mod
}


## 'set_covariates' -----------------------------------------------------------

## HAS_TESTS
#' Specify Covariates
#'
#' Add covariates to a model.
#'
#' If `set_covariates()` is applied to
#' a model that already has covariates,
#' `set_covariates()` deletes the
#' existing covariates.
#'
#' If `set_covariates()` is applied to
#' a fitted model, `set_covariates()` [unfits][unfit()]
#' the model, deleting existing estimates.
#' 
#' @section Covariate data:
#'
#' All variables contained in the `formula`
#' argument to `set_covariates()` should be in the
#' dataset supplied in the original call to
#' [mod_pois()], [mod_binom()], or [mod_norm()].
#'
#' `set_covariates()` processes the covariate data before
#' adding it to the model:
#' - All numeric variables are standardized, using
#'   `x <- scale(x)`.
#' - Categorical variables are converted to sets of indicator
#'   variables, using [treatment][stats::contr.treatment()] contrasts.
#'   For instance, variable `x` with categories
#'   `"high"`, `"medium"`, and `"low"`,
#'   is converted into two indicator variables, one called `xmedium` and one
#'   called `xlow`.
#'
#' @section Mathematical details:
#'
#' When a model includes covariates, the quantity
#'
#' \deqn{\pmb{Z} \pmb{\zeta}}
#'
#' is added to the linear predictor, where \eqn{\pmb{Z}}
#' is a matrix of standardized covariates, and \eqn{\pmb{\zeta}}
#' is a vector of coefficients. The elements of
#' \eqn{\pmb{\zeta}} have prior
#'
#' \deqn{\zeta_p \sim \text{N}(0, 1)}.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param formula A one-sided R [formula][stats::formula()],
#' specifying the covariates.
#'
#' @returns A modified version of `mod`
#'
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#'   model for rates, probabilities, or means
#'
#' @examples
#' ## create a COVID covariate
#' library(dplyr, warn.conflicts = FALSE)
#' births <- kor_births |>
#'   mutate(is_covid = time %in% 2020:2022)
#' mod <- mod_pois(births ~ age * region + time,
#'                 data = births,
#'                 exposure = popn) |>
#'   set_covariates(~ is_covid)
#' mod
#' @export
set_covariates <- function(mod, formula) {
  check_bage_mod(x = mod, nm_x = "mod")
  check_covariates_formula(formula = formula, mod = mod)
  data <- mod$data
  formula_mod <- mod$formula
  matrix_covariates <- make_matrix_covariates(formula = formula,
                                              data = data)
  covariates_nms <- colnames(matrix_covariates)
  if (has_covariates(mod))
    cli::cli_alert_warning("Model already has covariates. Deleting these.")
  mod$formula_covariates <- formula
  mod$covariates_nms <- covariates_nms
  mod <- unfit(mod)
  mod
}


## set_datamod_exposure' -----------------------------------------------------

#' Specify Exposure Data Model
#'
#' @description
#' 
#' Specify a data model for the exposure
#' variable in a Poisson model. The data model assumes
#' that, within each cell, observed exposure is drawn 
#' from an Inverse-Gamma distribution. In this model,
#'
#' E\[ expected exposure | true exposure \] = true exposure
#'
#' and
#'
#' sd\[ expected exposure | true exposure \] = `cv` \eqn{\times} true exposure
#'
#' where `cv` is a coefficient of variation parameter.
#' 
#' @details
#'
#' In the exposure data model, `cv`, the coefficient
#' of variation, does not depend on
#' true exposure. This implies that
#' errors do not fall, in relative terms,
#' as population rises. Unlike sampling errors,
#' measurement errors do not get averaged away
#' in large populations. 
#'
#' The exposure data model assumes that the exposure variable
#' is unbiased. If there is in fact evidence
#' of biases, then this evidence should be
#' used to create a de-biased version of the
#' variable (eg one where estimated biases
#' have been subtracted) to supply to
#' [mod_pois()].
#' 
#' `set_datamod_exposure()` can only be used
#' with a Poisson model for rates in which
#' the dispersion in the rates has been set to zero.
#' The dispersion in the rates can be set
#' explicitly to zero using [set_disp()],
#' though `set_datamod_exposure()` will also
#' do so.
#'
#' @section The `cv` argument:
#'
#' `cv` can be a single number, in which
#' case the same value is used for all cells.
#' `cv` can also be a data frame with a
#' with a variable called `"cv"` and
#' one or more columns with 'by' variables. 
#' For instance, a  `cv` of
#' ```
#' data.frame(sex = c("Female", "Male"),
#'            cv = c(0.01, 0.012))
#'```
#' implies that the coefficient of variation
#' is 0.01 for females and 0.012 for males.
#'
#' See below for an example where the coefficient
#' of variation is based on aggregated age groups.
#' 
#' @section Mathematical details:
#'
#' The model for observed exposure is
#'
#' \deqn{w_i^{\text{obs}} \sim \text{InvGamma}(2 + d_{g \lbrack i \rbrack }^{-1}, (1 + d_{g \lbrack i\rbrack }^{-1}) w_i^{\text{true}})}
#'
#' where
#' - \eqn{w_i^{\text{obs}}} is observed exposure for cell \eqn{i}
#'   (the `exposure` argument to [mod_pois()]);
#' - \eqn{w_i^{\text{true}}} is true exposure for cell \eqn{i}; and
#' - \eqn{d_{g\lbrack i\rbrack }} is the value for dispersion
#'   that is applied to cell \eqn{i}.
#'
#' `cv` is \eqn{\sqrt{d_g}}.
#'
#' @param mod An object of class `"bage_mod_pois"`,
#' created with [mod_pois()].
#' @param cv Coefficient of variation
#' for measurement errors in exposure.
#' A single number, or a data frame
#' with a variable called `"cv"`
#' and one or more 'by' variables.
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [set_disp()] Specify dispersion of rates
#' - [augment()] Original data plus estimated values,
#'   including estimates of true value for exposure
#' - [datamods] Data models implemented in `bage`
#' - [confidential] Confidentialization
#'   procedures modeled in `bage`
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify model
#' mod <- mod_pois(injuries ~ age * sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   set_disp(mean = 0) |>
#'   set_datamod_exposure(cv = 0.025)
#'
#' ## fit the model
#' mod <- mod |>
#'   fit()
#' mod
#'
#' ## examine results - note the new variable
#' ## '.popn' with estimates of the true
#' ## population
#' aug <- mod |>
#'   augment()
#'
#' ## allow different cv's for each sex
#' cv_sex <- data.frame(sex = c("Female", "Male"),
#'                      cv = c(0.03, 0.02))
#' mod <- mod |>
#'   set_datamod_exposure(cv = cv_sex)
#' mod
#'
#' ## our outcome variable is confidentialized,
#' ## so we recognize that in the model too
#' mod <- mod |>
#'   set_confidential_rr3()
#' mod
#'
#' ## now a model where everyone aged 0-49
#' ## receives one value for cv, and
#' ## everyone aged 50+ receives another
#' library(poputils) ## for 'age_upper()'
#' library(dplyr, warn.conflicts = FALSE)
#' nzl_injuries_age <- nzl_injuries |>
#'   mutate(age_group = if_else(age_upper(age) < 50,
#'                              "0-49",
#'                              "50+"))
#' cv_age <- data.frame(age_group = c("0-49", "50+"),
#'                      cv = c(0.05, 0.01))
#' mod <- mod_pois(injuries ~ age * sex + year,
#'                 data = nzl_injuries_age,
#'                 exposure = popn) |>
#'   set_disp(mean = 0) |>
#'   set_datamod_exposure(cv = cv_age)
#' @export
set_datamod_exposure <- function(mod, cv)  {
  nm_offset_data <- mod$nm_offset_data
  nm_offset_mod <- get_nm_offset_mod(mod)
  error_offset_formula_used(nm_offset_data = nm_offset_data,
                            nm_offset_mod = nm_offset_mod,
                            nm_fun = "set_datamod_exposure")
  ## preliminaries
  measure_vars_cv <- "cv"
  check_bage_mod(x = mod, nm_x = "mod")
  model_descr <- model_descr(mod)
  nm_distn <- nm_distn(mod)
  if (nm_distn != "pois") {
    cli::cli_abort(c("{.arg mod} is a {model_descr} model.",
                     i = paste("An exposure data model can only be used",
                               "with a Poisson model.")))
  }
  if (has_disp(mod)) {
    cli::cli_alert("Setting dispersion to zero. (Required for exposure data model.)")
    mod <- set_disp(mod, mean = 0)
  }
  if (!has_varying_offset(mod)) {
    cli::cli_abort(c("{.arg mod} does not include exposure.",
                     i = paste("An exposure data model can only be used",
                               "with a model with exposure.")))
  }
  data <- mod$data
  ## process 'cv', and create 'disp' 
  if (is.numeric(cv)) {
    check_number(x = cv, nm_x = "cv")
    if (cv <= 0)
      cli::cli_abort("{.arg cv} less than or equal to 0.")
    cv <- data.frame(cv = cv)
  }
  check_datamod_val(x = cv,
                    nm_x = "cv",
                    measure_vars = measure_vars_cv)
  nms_by <- setdiff(names(cv), measure_vars_cv)
  by_val_cv <- cv[nms_by]
  check_datamod_by_val(by_val = by_val_cv,
                       data = data,
                       nm_val = "cv",
                       nm_data = "data")
  check_positive(x = cv[[measure_vars_cv]],
                 nm_x = measure_vars_cv,
                 nm_df = "cv")
  cv <- make_datamod_measure(data = data,
                             by_val = by_val_cv,
                             measure = cv[[measure_vars_cv]])
  disp <- cv^2
  disp_levels <- make_datamod_levels(data = data,
                                     by_val = by_val_cv,
                                     nm_component = "cv")
  disp_matrix_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val_cv)
  ## construct datamod and add to 'mod'
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       cv_arg = cv,
                                       nms_by = nms_by)
  if (has_datamod(mod)) {
    datamod_old <- mod$datamod
    alert_replacing_existing_datamod(datamod_new = datamod,
                                     datamod_old = datamod_old)
  }
  mod$datamod <- datamod
  mod <- unfit(mod)
  mod
}


## set_datamod_miscount' -----------------------------------------------------

#' Specify Miscount Data Model
#'
#' Specify a data model for the outcome
#' in a Poisson model, where the outcome
#' is subject to undercount and
#' overcount.
#'
#' The miscount data model is essentially a combination
#' of the [undercount][set_datamod_undercount()]
#' and [overcount][set_datamod_overcount()]
#' data models. It assumes that
#' reported outcome is the sum of two quantities:
#'
#' 1. *Units from target population, undercounted*
#'   People or events belonging
#'   to the target population, in which
#'   each unit's inclusion probability
#'   is less than 1.
#' 2. *Overcount* People or events
#'   that do not belong to target population,
#'   or that are counted more than once.
#'    
#' If, for instance, a census
#' enumerates 91 people from a true population
#' of 100, but also mistakenly enumerates
#' a further 6 people, then
#' 
#' - the true value for the outcome variable is 100
#' - the value for the undercounted target population is 91,
#' - the value for the overcount is 6, and
#' - the observed value for the outcome variable is 91 + 6 = 97.
#'
#' @section The `prob` argument:
#'
#' The `prob` argument specifies a prior
#' distribution for the probability
#' that a person or event in the target
#' population is included in the
#' reported outcome. `prob` is a
#' data frame with a variable called `"mean"`,
#' a variable called `"disp"`, and, optionally,
#' one or more 'by' variables.
#' For instance, a  `prob` of
#' ```
#' data.frame(sex = c("Female", "Male"),
#'            mean = c(0.95, 0.92),
#'            disp = c(0.02, 0.015))
#'```
#' implies that the expected value for
#' the inclusion probability is 0.95 for females
#' and 0.92 for males, with slightly more
#' uncertainty for females than for males.
#'
#' @section The `rate` argument:
#'
#' The `rate` argument specifies a prior
#' distribution for the overcoverage
#' rate. `rate` is a
#' data frame with a variable called `"mean"`,
#' a variable called `"disp"`, and, optionally,
#' one or more 'by' variables.
#' For instance, a  `rate` of
#' ```
#' data.frame(mean = 0.03, disp = 0.1)
#'```
#' implies that the expected value for
#' the overcoverage rate is 0.03,
#' with a dispersion of 0.1. Since no 'by'
#' variables are included, the same
#' mean and dispersion values are
#' applied to all cells.
#' 
#' @section Mathematical details:
#'
#' The model for the observed outcome is
#'
#' \deqn{y_i^{\text{obs}} = u_i + v_i}
#' \deqn{u_i \sim \text{Binomial}(y_i^{\text{true}}, \pi_{g[i]})}
#' \deqn{v_i \sim \text{Poisson}(\kappa_{h[i]} \gamma_i w_i)}
#' \deqn{\pi_g \sim \text{Beta}(m_g^{(\pi)} / d_g^{(\pi)}, (1-m_g^{(\pi)}) / d_g^{(\pi)})}
#' \deqn{\kappa_h \sim \text{Gamma}(1/d_h^{(\kappa)}, 1/(d_h^{(\kappa)} m_h^{(\kappa)}))}
#'
#' where
#' - \eqn{y_i^{\text{obs}}} is the observed outcome for cell \eqn{i};
#' - \eqn{y_i^{\text{true}}} is the true outcome for cell \eqn{i};
#' - \eqn{\gamma_i} is the rate for cell \eqn{i};
#' - \eqn{w_i} is exposure for cell \eqn{i};
#' - \eqn{\pi_{g[i]}} is the probability that a member of the
#'     target population in cell \eqn{i} is correctly enumerated in that cell;
#' - \eqn{\kappa_{h[i]}} is the overcoverage rate for cell \eqn{i};
#' - \eqn{m_g^{(\pi)}} is the expected value for \eqn{\pi_g}
#'   (specified via `prob`);
#' - \eqn{d_g^{(\pi)}} is disperson for \eqn{\pi_g} (specified via `prob`);
#' - \eqn{m_h^{(\kappa)}} is the expected value for \eqn{\kappa_h}
#'   (specified via `rate`); and
#' - \eqn{d_h^{(\kappa)}} is disperson for \eqn{\kappa_h} (specified via `rate`).
#'
#' @param mod An object of class `"bage_mod_pois"`,
#' created with [mod_pois()].
#' @param prob The prior for the probability
#' that a person or event in the target
#' population will correctly enumerated.
#' A data frame with a variable
#' called `"mean"`, a variable called `"disp"`,
#' and, optionally, one or more 'by' variables.
#' @param rate The prior for the overcoverage rate.
#' A data frame with a variable
#' called `"mean"`, a variable called `"disp"`,
#' and, optionally, one or more 'by' variables.
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [augment()] Original data plus estimated values,
#'   including estimates of true value for
#'   the outcome variable
#' - [components()] Estimated values for
#'   model parameters, including inclusion
#'   probabilities and overcount rates
#' - [set_datamod_undercount()] An undercount-only
#'   data model
#' - [set_datamod_overcount()] An overcount-only
#'   data model
#' - [datamods] All data models implemented in `bage`
#' - [confidential] Confidentialization
#'   procedures modeled in `bage`
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify 'prob' and 'rate'
#' prob <- data.frame(sex = c("Female", "Male"),
#'                    mean = c(0.95, 0.97),
#'                    disp = c(0.05, 0.05))
#' rate <- data.frame(mean = 0.03, disp = 0.15)
#'
#' ## specify model
#' mod <- mod_pois(divorces ~ age * sex + time,
#'                 data = nzl_divorces,
#'                 exposure = population) |>
#'   set_datamod_miscount(prob = prob, rate = rate)
#' mod
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#' mod
#'
#' ## original data, plus imputed values for outcome
#' mod |>
#'   augment()
#'
#' ## parameter estimates
#' library(dplyr)
#' mod |>
#'   components() |>
#'   filter(term == "datamod")
#'
#' ## the data have in fact been confidentialized,
#' ## so we account for that, in addition
#' ## to accounting for undercoverage and
#' ## overcoverage
#' mod <- mod |>
#'  set_confidential_rr3() |>
#'  fit()
#' mod
#' @export
set_datamod_miscount <- function(mod, prob, rate) {
  ## preliminaries
  measure_vars_prob <- c("mean", "disp")
  measure_vars_rate <- c("mean", "disp")
  check_bage_mod(x = mod, nm_x = "mod")
  nm_distn <- nm_distn(mod)
  if (nm_distn != "pois") {
    model_descr <- model_descr(mod)
    cli::cli_abort(c(paste("A miscount data model can only be used",
                           "with a Poisson model."),
                     i = "{.arg mod} is a {model_descr} model."))
  }
  data <- mod$data
  ## process 'prob'
  check_datamod_val(x = prob,
                    nm_x = "prob",
                    measure_vars = measure_vars_prob)
  nms_by_prob <- setdiff(names(prob), measure_vars_prob)
  by_val_prob <- prob[nms_by_prob]
  check_datamod_by_val(by_val = by_val_prob,
                       data = data,
                       nm_val = "prob",
                       nm_data = "data")
  check_positive(x = prob$mean,
                 nm_x = "mean",
                 nm_df = "prob")
  check_positive(x = prob$disp,
                 nm_x = "disp",
                 nm_df = "prob")
  check_lt_one(x = prob$mean, nm_x = "mean")
  prob_mean <- make_datamod_measure(data = data,
                                    by_val = by_val_prob,
                                    measure = prob$mean)
  prob_disp <- make_datamod_measure(data = data,
                                    by_val = by_val_prob,
                                    measure = prob$disp)
  prob_levels <- make_datamod_levels(data = data,
                                     by_val = by_val_prob,
                                     nm_component = "prob")
  prob_matrix_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val_prob)
  ## process 'rate'
  check_datamod_val(x = rate,
                    nm_x = "rate",
                    measure_vars = measure_vars_rate)
  nms_by_rate <- setdiff(names(rate), measure_vars_rate)
  by_val_rate <- rate[nms_by_rate]
  check_datamod_by_val(by_val = by_val_rate,
                       data = data,
                       nm_val = "rate",
                       nm_data = "data")
  check_positive(x = rate$mean,
                 nm_x = "mean",
                 nm_df = "rate")
  check_positive(x = rate$disp,
                 nm_x = "disp",
                 nm_df = "rate")
  rate_mean <- make_datamod_measure(data = data,
                                    by_val = by_val_rate,
                                    measure = rate$mean)
  rate_disp <- make_datamod_measure(data = data,
                                    by_val = by_val_rate,
                                    measure = rate$disp)
  rate_levels <- make_datamod_levels(data = data,
                                     by_val = by_val_rate,
                                     nm_component = "rate")
  rate_matrix_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val_rate)
  ## construct 'nms_by'
  nms_by <- union(nms_by_prob, nms_by_rate)
  ## construct datamod and add to 'mod'
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       prob_arg = prob,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       rate_arg = rate,
                                       nms_by = nms_by)
  if (has_datamod(mod)) {
    datamod_old <- mod$datamod
    alert_replacing_existing_datamod(datamod_new = datamod,
                                     datamod_old = datamod_old)
  }
  mod$datamod <- datamod
  mod <- unfit(mod)
  mod
}


## set_datamod_noise' ---------------------------------------------------------

#' Specify Noise Data Model
#'
#' @description 
#' Specify a data model in which
#'
#' `observed outcome = true outcome + error`,
#'
#' where the error has a symmetric distribution
#' with mean 0.
#'
#' If the true outcome has a normal
#' distribution, then the error has a
#' normal distribution. If the
#' true outcome has a Poisson distribution,
#' then the error has a symmetric Skellam
#' distribution.
#'
#' @details
#'
#' The model assumes that the outcome variable
#' is unbiased. If there is in fact evidence
#' of biases, then this evidence should be
#' used to create a de-biased version of the
#' outcome variable in `data`, and this de-biased
#' version should be used by [mod_norm()] or
#' [mod_pois()].
#'
#' If `set_datamod_noise()` is used with a Poisson
#' model, then the dispersion term for
#' the Poisson rates must be set to zero.
#' This can be done using [set_disp()],
#' though `set_datamod_noise()` will also
#' do so.
#'
#' @section The Skellam distribution:
#'
#' The Skellam distribution is restricted to integers,
#' but can take positive and negative values.
#'
#' If
#'
#' \deqn{X_1 \sim \text{Poisson}(\mu_1)}
#' \deqn{X_2 \sim \text{Poisson}(\mu_2)}
#'
#' then
#'
#' \deqn{Y = X_1 - X_2}
#'
#' has a \eqn{\text{Skellam}(\mu_1, \mu_2)} distribution.
#' If \eqn{\mu_1 = \mu_2}, then the distribution
#' is symmetric.
#'
#' @section The `sd` argument:
#'
#' `sd` can be a single number, in which
#' case the same standard deviation
#' is used for all cells.
#' `sd` can also be a data frame with a
#' with a variable called `"sd"` and
#' one or more columns with 'by' variables. 
#' For instance, a  `sd` of
#' ```
#' data.frame(sex = c("Female", "Male"),
#'            sd = c(330, 240))
#'```
#' implies that measurement errors
#' have standard deviation 330 for females
#' and 240 for males.
#' 
#' @section Mathematical details:
#'
#' The model for the observed outcome is
#'
#' \deqn{y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i}
#'
#' with
#'
#' \deqn{\epsilon_i \sim \text{N}(0, s_{g[i]}^2)}
#'
#' if \eqn{y_i^{\text{true}}} has a normal distribution, and
#'
#' \deqn{\epsilon_i \sim \text{Skellam}(0.5 s_{g[i]}^2, 0.5 s_{g[i]}^2)}
#'
#' if \eqn{y_i^{\text{true}}} has a Poisson distribution, where
#' 
#' - \eqn{y_i^{\text{obs}}} is the observed outcome for cell \eqn{i};
#' - \eqn{y_i^{\text{true}}} is the true outcome for cell \eqn{i};
#' - \eqn{\epsilon_i} is the measurement error for cell \eqn{i}; and
#' - \eqn{s_{g\lbrack i\rbrack }} is the standard deviation of
#'   the measurement error for cell \eqn{i}.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_norm()] or [mod_pois()].
#' @param sd Standard deviation of measurement errors.
#' A single number, or a data frame
#' with 'by' variables.
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [mod_norm()] Specify a normal model
#' - [mod_pois()] Specify a Poisson model
#' - [augment()] Original data plus estimated values,
#'   including estimates of true value for outcome
#' - [datamods] Data models implemented in `bage`
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## Normal model ------------------------------
#' 
#' ## prepare outcome variable
#' library(dplyr, warn.conflicts = FALSE)
#' spend <- nld_expenditure |>
#'   mutate(log_spend = log(value + 1))
#'
#' ## specify model
#' mod <- mod_norm(log_spend ~ age * diag + year,
#'                 data = spend,
#'                 weights = 1) |>
#'   set_datamod_noise(sd = 0.1)
#' 
#' ## fit model
#' mod <- mod |>
#'   fit()
#' mod
#' 
#' ## create new aggregated diagnositic
#' ## group variable
#' library(dplyr, warn.conflicts = FALSE)
#' spend <- spend |>
#'   mutate(diag_ag = case_when(
#'     diag == "Neoplasms" ~ diag,
#'     diag == "Not allocated" ~ diag,
#'     TRUE ~ "Other"
#'   ))
#'
#' ## assume size of measurement errors
#' ## varies across these aggregated groups
#' sd_diag <- data.frame(diag_ag = c("Neoplasms",
#'                                   "Not allocated",
#'                                   "Other"),
#'                       sd = c(0.05, 0.2, 0.1))
#'
#' ## fit model that uses diagnostic-specific
#' ## standard deviations
#' mod <- mod_norm(log_spend ~ age * diag + year,
#'                 data = spend,
#'                 weights = 1) |>
#'   set_datamod_noise(sd = sd_diag)
#'
#'
#' ## Poisson model -----------------------------
#'
#' mod <- mod_pois(deaths ~ month,
#'                 data = usa_deaths,
#'                 exposure = 1) |>
#'   set_datamod_noise(sd = 200)
#' @export
set_datamod_noise <- function(mod, sd) {
  ## preliminaries
  measure_vars_sd <- "sd"
  check_bage_mod(x = mod, nm_x = "mod")
  model_descr <- model_descr(mod)
  nm_distn <- nm_distn(mod)
  if (!(nm_distn %in% c("pois", "norm"))) {
    cli::cli_abort(c("{.arg mod} is a {model_descr} model.",
                     i = paste("A noise data model can only be used",
                               "with Poisson or normal models.")))
  }
  data <- mod$data
  ## model-specific processing
  if (nm_distn == "pois") {
    if (has_disp(mod)) {
      cli::cli_alert(paste("Setting dispersion to zero. (Required when using",
                           "noise data model with Poisson rates model.)"))
      mod <- set_disp(mod, mean = 0)
    }
    outcome_sd <- NULL
  }
  else if (nm_distn == "norm")
    outcome_sd <- mod$outcome_sd
  else
    cli::cli_abort("Internal error: {.val {nm_distn}} is not a valid valud for {.arg distn}.") # nocov
  ## process 'sd'
  if (is.numeric(sd)) {
    check_number(x = sd, nm_x = "sd")
    if (sd <= 0)
      cli::cli_abort("{.arg sd} less than or equal to 0.")
    sd <- data.frame(sd = sd)
  }
  check_datamod_val(x = sd,
                    nm_x = "sd",
                    measure_vars = measure_vars_sd)
  nms_by_sd <- setdiff(names(sd), measure_vars_sd)
  by_val_sd <- sd[nms_by_sd]
  check_datamod_by_val(by_val = by_val_sd,
                       data = data,
                       nm_val = "sd",
                       nm_data = "data")
  check_positive(x = sd$sd,
                 nm_x = "sd",
                 nm_df = "sd")
  sd_sd <- sd$sd
  sd_sd <- make_datamod_measure(data = data,
                                by_val = by_val_sd,
                                measure = sd_sd)
  sd_levels <- make_datamod_levels(data = data,
                                   by_val = by_val_sd,
                                   nm_component = "sd")
  sd_matrix_outcome <- make_matrix_val_outcome(data = data,
                                               by_val = by_val_sd)
  ## construct datamod and add to 'mod'
  datamod <- new_bage_datamod_noise(sd_sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    sd_arg = sd,
                                    nms_by = nms_by_sd,
                                    outcome_sd = outcome_sd)
  if (has_datamod(mod)) {
    datamod_old <- mod$datamod
    alert_replacing_existing_datamod(datamod_new = datamod,
                                     datamod_old = datamod_old)
  }
  mod$datamod <- datamod
  mod <- unfit(mod)
  mod
}


## 'set_datamod_overcount' ----------------------------------------------------

#' Specify Overcount Data Model
#'
#' Specify a data model for the outcome
#' in a Poisson model, where the outcome
#' is subject to overcount
#'
#' The overcount data model assumes that
#' reported values for the outcome overstate
#' the actual values. The reported values
#' might be affected by double-counting,
#' for instance, or might include some
#' people or events that are not in the target
#' population.
#' 
#' @section The `rate` argument:
#'
#' The `rate` argument specifies a prior
#' distribution for the overcoverage
#' rate. `rate` is a
#' data frame with a variable called `"mean"`,
#' a variable called `"disp"`, and, optionally,
#' one or more 'by' variables.
#' For instance, a  `rate` of
#' ```
#' data.frame(sex = c("Female", "Male"),
#'            mean = c(0.05, 0.03),
#'            disp = c(0.1, 0.15))
#'```
#' implies that the reported value
#' for the outcome is expected to
#' overstate the true value by about 5%
#' for females, and about 3% for females,
#' with greater unceratinty for males than females.
#'
#' @section Mathematical details:
#'
#' The model for the observed outcome is
#'
#' \deqn{y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i}
#' \deqn{\epsilon_i \sim \text{Poisson}(\kappa_{g[i]} \gamma_i w_i)}
#' \deqn{\kappa_g \sim \text{Gamma}(1/d_g, 1/(d_g m_g))}
#'
#' where
#' - \eqn{y_i^{\text{obs}}} is the observed outcome for cell \eqn{i};
#' - \eqn{y_i^{\text{true}}} is the true outcome for cell \eqn{i};
#' - \eqn{\epsilon_i} overcount in cell \eqn{i};
#' - \eqn{\gamma_i} is the rate for cell \eqn{i};
#' - \eqn{w_i} is exposure for cell \eqn{i};
#' - \eqn{\kappa_{g[i]}} is the overcoverage rate for cell \eqn{i};
#' - \eqn{m_g} is the expected value for \eqn{\kappa_g}
#'   (specified via `rate`); and
#' - \eqn{d_g} is disperson for \eqn{\kappa_g} (specified via `rate`).
#'
#' @inheritParams set_datamod_miscount
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [augment()] Original data plus estimated values,
#'   including estimates of true value for
#'   the outcome variable
#' - [components()] Estimated values for
#'   model parameters, including inclusion
#'   probabilities and overcount rates
#' - [set_datamod_undercount()] An undercount-only
#'   data model
#' - [set_datamod_miscount()] An undercount-and-overcount
#'   data model
#' - [datamods] All data models implemented in `bage`
#' - [confidential] Confidentialization
#'   procedures modeled in `bage`
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify 'rate'
#' rate <- data.frame(sex = c("Female", "Male"),
#'                    mean = c(0.1, 0.13),
#'                    disp = c(0.2, 0.2))
#'
#' ## specify model
#' mod <- mod_pois(divorces ~ age * sex + time,
#'                 data = nzl_divorces,
#'                 exposure = population) |>
#'   set_datamod_overcount(rate)
#' mod
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#' mod
#'
#' ## original data, plus imputed values for outcome
#' mod |>
#'   augment()
#'
#' ## parameter estimates
#' library(dplyr)
#' mod |>
#'   components() |>
#'   filter(term == "datamod")
#'
#' ## the data have in fact been confidentialized,
#' ## so we account for that, in addition
#' ## to accounting for overcoverage
#' mod <- mod |>
#'  set_confidential_rr3() |>
#'  fit()
#' mod
#' @export
set_datamod_overcount <- function(mod, rate) {
  ##  preliminaries
  measure_vars_rate <- c("mean", "disp")
  check_bage_mod(x = mod, nm_x = "mod")
  nm_distn <- nm_distn(mod)
  if (nm_distn != "pois") {
    model_descr <- model_descr(mod)
    cli::cli_abort(c(paste("An overcount data model can only be used",
                           "with a Poisson model."),
                     i = "{.arg mod} is a {model_descr} model."))
  }
  data <- mod$data
  ## process 'rate'
  check_datamod_val(x = rate,
                    nm_x = "rate",
                    measure_vars = measure_vars_rate)
  nms_by_rate <- setdiff(names(rate), measure_vars_rate)
  by_val_rate <- rate[nms_by_rate]
  check_datamod_by_val(by_val = by_val_rate,
                       data = data,
                       nm_val = "rate",
                       nm_data = "data")
  check_positive(x = rate$mean,
                 nm_x = "mean",
                 nm_df = "rate")
  check_positive(x = rate$disp,
                 nm_x = "disp",
                 nm_df = "rate")
  rate_mean <- make_datamod_measure(data = data,
                                    by_val = by_val_rate,
                                    measure = rate$mean)
  rate_disp <- make_datamod_measure(data = data,
                                    by_val = by_val_rate,
                                    measure = rate$disp)
  rate_levels <- make_datamod_levels(data = data,
                                     by_val = by_val_rate,
                                     nm_component = "rate")
  rate_matrix_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val_rate)
  ## construct 'nms_by'
  nms_by <- nms_by_rate
  ## construct datamod and add to object
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        rate_arg = rate,
                                        nms_by = nms_by)
  if (has_datamod(mod)) {
    datamod_old <- mod$datamod
    alert_replacing_existing_datamod(datamod_new = datamod,
                                     datamod_old = datamod_old)
  }
  mod$datamod <- datamod
  mod <- unfit(mod)
  mod
}


## 'set_datamod_undercount' ---------------------------------------------------

#' Specify Undercount Data Model
#'
#' Specify a data model for the outcome
#' in a Poisson or binomial model,
#' where the outcome
#' is subject to undercount.
#'
#' The undercount data model assumes that
#' reported values for the outcome variable
#' understate the true values, because
#' the reported values miss some people
#' or events in the target population.
#' In other words, the probability that
#' any given unit in the target population
#' will be included in the reported outcome
#' is less than 1.
#'
#' @section The `prob` argument:
#'
#' The `prob` argument specifies a prior
#' distribution for the probability
#' that a person or event in the target
#' population is included in the
#' reported outcome. `prob` is a
#' data frame with a variable called `"mean"`,
#' a variable called `"disp"`, and, optionally,
#' one or more 'by' variables.
#' For instance, a  `prob` of
#' ```
#' data.frame(sex = c("Female", "Male"),
#'            mean = c(0.95, 0.92),
#'            disp = c(0.02, 0.015))
#'```
#' implies that the expected value for
#' the inclusion probability is 0.95 for females
#' and 0.92 for males, with slightly more
#' uncertainty for females than for males.
#'
#' @section Mathematical details:
#'
#' The model for the observed outcome is
#'
#' \deqn{y_i^{\text{obs}} \sim \text{Binomial}(y_i^{\text{true}}, \pi_{g[i]})}
#' \deqn{\pi_g \sim \text{Beta}(m_g^{(\pi)} / d_g^{(\pi)}, (1-m_g^{(\pi)}) / d_g^{(\pi)})}
#'
#' where
#' - \eqn{y_i^{\text{obs}}} is the observed outcome for cell \eqn{i};
#' - \eqn{y_i^{\text{true}}} is the true outcome for cell \eqn{i};
#' - \eqn{\pi_{g[i]}} is the probability that a member of the
#'     target population in cell \eqn{i} is correctly enumerated in that cell;
#' - \eqn{m_g} is the expected value for \eqn{\pi_g}
#'   (specified via `prob`); and
#' - \eqn{d_g} is disperson for \eqn{\pi_g} (specified via `prob`).
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()] or [mod_binom()].
#' @param prob The prior for the probability
#' that a person or event in the target
#' population will correctly enumerated.
#' A data frame with a variable
#' called `"mean"`, a variable called `"disp"`,
#' and, optionally, one or more 'by' variables.
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [augment()] Original data plus estimated values,
#'   including estimates of true value for
#'   the outcome variable
#' - [components()] Estimated values for
#'   model parameters, including inclusion
#'   probabilities and overcount rates
#' - [set_datamod_overcount()] An overcount-only
#'   data model
#' - [set_datamod_miscount()] An undercount-and-overcount
#'   data model
#' - [datamods] All data models implemented in `bage`
#' - [confidential] Confidentialization
#'   procedures modeled in `bage`
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' ## specify 'prob'
#' prob <- data.frame(sex = c("Female", "Male"),
#'                    mean = c(0.95, 0.97),
#'                    disp = c(0.05, 0.05))
#'
#' ## specify model
#' mod <- mod_pois(divorces ~ age * sex + time,
#'                 data = nzl_divorces,
#'                 exposure = population) |>
#'   set_datamod_undercount(prob)
#' mod
#'
#' ## fit model
#' mod <- mod |>
#'   fit()
#' mod
#'
#' ## original data, plus imputed values for outcome
#' mod |>
#'   augment()
#'
#' ## parameter estimates
#' library(dplyr)
#' mod |>
#'   components() |>
#'   filter(term == "datamod")
#'
#' ## the data have in fact been confidentialized,
#' ## so we account for that, in addition
#' ## to accounting for undercoverage
#' mod <- mod |>
#'  set_confidential_rr3() |>
#'  fit()
#' mod
#' @export
set_datamod_undercount <- function(mod, prob) {
  ##  preliminaries
  measure_vars_prob <- c("mean", "disp")
  check_bage_mod(x = mod, nm_x = "mod")
  nm_distn <- nm_distn(mod)
  if (!(nm_distn %in% c("pois", "binom"))) {
    model_descr <- model_descr(mod)
    cli::cli_abort(c(paste("An undercount data model can only be used",
                           "with a Poisson or binomial model."),
                     i = "{.arg mod} is a {model_descr} model."))
  }
  data <- mod$data
  ## process 'prob'
  check_datamod_val(x = prob,
                    nm_x = "prob",
                    measure_vars = measure_vars_prob)
  nms_by_prob <- setdiff(names(prob), measure_vars_prob)
  by_val_prob <- prob[nms_by_prob]
  check_datamod_by_val(by_val = by_val_prob,
                       data = data,
                       nm_val = "prob",
                       nm_data = "data")
  check_positive(x = prob$mean,
                 nm_x = "mean",
                 nm_df = "prob")
  check_lt_one(x = prob$mean,
               nm_x = "mean",
               nm_df = "prob")
  check_positive(x = prob$disp,
                 nm_x = "disp",
                 nm_df = "prob")
  prob_mean <- make_datamod_measure(data = data,
                                    by_val = by_val_prob,
                                    measure = prob$mean)
  prob_disp <- make_datamod_measure(data = data,
                                    by_val = by_val_prob,
                                    measure = prob$disp)
  prob_levels <- make_datamod_levels(data = data,
                                     by_val = by_val_prob,
                                     nm_component = "prob")
  prob_matrix_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val_prob)
  ## construct 'nms_by'
  nms_by <- nms_by_prob
  ## construct datamod and add to object
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         prob_arg = prob,
                                         nms_by = nms_by)
  if (has_datamod(mod)) {
    datamod_old <- mod$datamod
    alert_replacing_existing_datamod(datamod_new = datamod,
                                     datamod_old = datamod_old)
  }
  mod$datamod <- datamod
  mod <- unfit(mod)
  mod
}


## 'set_datamod_outcome_rr3' --------------------------------------------------

#' Specify RR3 Data Model
#'
#' #' `r lifecycle::badge('deprecated')
#'
#' This function has been deprecated, and will
#' be removed from future versions of `bage`.
#' Please used function [set_confidential_rr3()]
#' instead.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns A revised version of `mod`.
#'
#' @examples
#' ## 'injuries' variable in 'nzl_injuries' dataset
#' ## has been randomly rounded to base 3
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   set_confidential_rr3() |> ## rather than set_datamod_outcome_rr3
#'   fit()
#' @export
set_datamod_outcome_rr3 <- function(mod) {
  lifecycle::deprecate_warn(when = "0.9.4",
                            what = "set_datamod_outcome_rr3()",
                            with = "set_confidential_rr3()")
  set_confidential_rr3(mod)
}


## 'set_disp' -----------------------------------------------------------------

## HAS_TESTS
#' Specify Prior for Dispersion or Standard Deviation
#'
#' Specify the mean of prior for the dispersion
#' parameter (in Poisson and binomial models) or the
#' standard deviation parameter (in normal models.)
#'
#' The dispersion or mean parameter has an exponential
#' distribution with mean \eqn{\mu},
#'
#' \deqn{p(\xi) = \frac{1}{\mu}\exp\left(\frac{-\xi}{\mu}\right).}
#'
#' By default \eqn{\mu} equals 1.
#'
#' In Poisson and binomial models,
#' `mean` can be set to `0`, implying
#' that the dispersion term is also `0`.
#' In normal models, `mean` must be non-negative.
#'
#' If `set_disp()` is applied to
#' a fitted model, `set_disp()` [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @inheritParams set_confidential_rr3
#' @param mean Mean value for the exponential prior.
#' In Poisson and binomial models, can be set to 0.
#' Default is `1`.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#' model for rates, probabilities, or means
#' - [set_prior()] Specify prior for a term
#' - [set_n_draw()] Specify the number of draws
#' - [is_fitted()] Test whether a model is fitted
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_disp(mean = 0.1)
#' mod |> set_disp(mean = 0)
#' @export
set_disp <- function(mod, mean = 1) {
    check_bage_mod(x = mod, nm_x = "mod")
    nm_distn <- nm_distn(mod)
    zero_ok <- nm_distn %in% c("pois", "binom")
    check_scale(mean, nm_x = "mean", zero_ok = zero_ok)
    mean_disp <- as.double(mean)
    mod$mean_disp <- mean_disp
    mod <- unfit(mod)
    mod
}


## 'set_n_draw' ---------------------------------------------------------------

## HAS_TESTS
#' Specify Number of Draws from Prior or Posterior Distribution
#'
#' Specify the number of draws from the posterior
#' distribution to be used in model output.
#' A newly-created `bage_mod` object has an
#' `n_draw` value of 1000. Higher values
#' may be appropriate for characterizing
#' the tails of distributions, or for
#' publication-quality graphics and summaries.
#'
#' If the new value for `n_draw` is greater than
#' the old value, and the model has already been fitted,
#' then the model is [unfitted][unfit()], and
#' function [fit()] may need to be called again.
#'
#' @inheritParams set_confidential_rr3
#' @param n_draw Number of draws.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [bage::n_draw.bage_mod()] query the value of `n_draw`
#' - [bage::augment()], [bage::components()] functions for
#'   drawing from prior or posterior distribution - the output
#'   of which is affected by the value of `n_draw`
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#'   model
#' - [set_prior()] Specify prior for a term
#' - [set_disp()] Specify prior for dispersion
#' - [fit()] Fit a model
#' - [unfit()] Reset a model
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' mod # value for 'n_draw' displayed in object
#' n_draw(mod) # or use 'n_draw()' to query
#'
#' mod <- mod |>
#'   set_n_draw(n_draw = 5000)
#' mod
#' @export
set_n_draw <- function(mod, n_draw = 1000L) {
  check_bage_mod(x = mod, nm_x = "mod")
  poputils::check_n(n = n_draw,
                    nm_n = "n_draw",
                    min = 0L,
                    max = NULL,
                    divisible_by = NULL)
  n_draw <- as.integer(n_draw)
  n_draw_old <- mod$n_draw
  mod$n_draw <- n_draw
  if (is_fitted(mod)) {
    if (n_draw > n_draw_old) {
      cli::cli_alert(paste("New value for {.arg n_draw} ({.val {n_draw}}) greater than",
                           "old value ({.val {n_draw_old}}), so unfitting model."))
      mod <- unfit(mod)
    }
    if (n_draw < n_draw_old) {
      s <- seq_len(n_draw)
      mod$draws_effectfree <- mod$draws_effectfree[, s, drop = FALSE]
      mod$draws_hyper <- mod$draws_hyper[, s, drop = FALSE]
      mod$draws_hyperrandfree <- mod$draws_hyperrandfree[, s, drop = FALSE]
      if (has_covariates(mod))
        mod$draws_coef_covariates <- mod$draws_coef_covariates[, s, drop = FALSE]        
      if (has_disp(mod))
        mod$draws_disp <- mod$draws_disp[s]
      if (has_datamod_param(mod))
        mod$draws_datamod_param <- mod$draws_datamod_param[, s, drop = FALSE]
    }
  }
  mod
}


## 'set_prior' ----------------------------------------------------------------

## HAS_TESTS
#' Specify Prior for Model Term
#'
#' Specify a prior distribution for an intercept,
#' a main effect, or an interaction.
#'
#' If `set_prior()` is applied to
#' a fitted model, `set_prior()` [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @param mod A `bage_mod` object, created with
#' [mod_pois()], [mod_binom()], or [mod_norm()].
#' @param formula A formula giving the term
#' and a function for creating a prior.
#'
#' @returns A modified `bage_mod` object.
#'
#' @seealso
#' - [priors] Current choices for prior distributions
#' - [is_fitted()] Test whether a model is fitted
#' - [set_disp()] Specify prior for dispersion
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_prior(age ~ RW2())
#' @export
set_prior <- function(mod, formula) {
  nm_response <- deparse1(formula[[2L]])
  check_bage_mod(x = mod, nm_x = "mod")
  check_format_prior_formula(formula)
  nms_terms <- names(mod$priors)
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  nm_response_split <- strsplit(nm_response, split = ":")[[1L]]
  nms_terms_split <- lapply(nms_terms, strsplit, split = ":")
  nms_terms_split <- lapply(nms_terms_split, `[[`, 1L)
  is_matched <- FALSE
  for (i in seq_along(nms_terms_split)) {
    is_matched <- setequal(nm_response_split, nms_terms_split[[i]])
    if (is_matched)
      break
  }
  if (!is_matched)
    cli::cli_abort(c("Problem with prior formula {.code {deparse1(formula)}}.",
                     i = "The response must be a term from the model formula {.code {deparse1(mod$formula)}}.",
                     i = "The model formula contains terms {.val {nms_terms}}."))
  prior <- tryCatch(eval(formula[[3L]]),
                    error = function(e) e)
  if (inherits(prior, "error"))
    cli::cli_abort(c("Problem with prior formula {.code {deparse1(formula)}}.",
                     i = prior$message))
  dimnames_term <- dimnames_terms[[i]]
  is_prior_ok_for_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender)
  mod$priors[[i]] <- prior
  mod <- unfit(mod)
  mod
}


## 'set_seeds' --------------------------------------------------------------

## HAS_TESTS
#' Reset Random Seeds in Model Object
#'
#' Reset random seeds stored in a model object.
#' When `new_seeds` is `NULL` (the default),
#' the new seeds are generated randomly; otherwise
#' they are taken from `new_seeds`.
#'
#' When an object of class `"bage_mod"` is first created,
#' values are generated four four random seeds:
#'
#' - `seed_components`
#' - `seed_augment`
#' - `seed_forecast_components`
#' - `seed_forecast_augment`
#'
#' When [fit()], [components()], [augment()],
#' and [forecast()] are called on the model object,
#' the seeds are used internally to ensure that
#' he same inputs generate the same outputs, even
#' when the outputs involve random draws.
#'
#' End users are unlikely to call `set_seeds()` in
#' a data analysis, though it may occasionally by useful
#' when building a simulation from scratch.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param new_seeds `NULL` (the default) or a list
#' of integers with names `"seed_components"`
#' `"seed_augment"`, `"seed_forecast_components"`,
#' and `"seed_forecast_augment"`.
#'
#' @returns A revised version of `mod`.
#'
#' @seealso
#' - [report_sim()] Do a simulation study. (`report_sim()`
#'   calls `set_seeds()` internally.)
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a model
#' - [fit()] Fit a model
#' - [unfit()] Reset model, deleting estimates
#'
#' @examples
#' ## fit model
#' mod <- mod_pois(injuries ~ age,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   fit()
#'
#' ## call 'components()'
#' components(mod)
#'
#' ## call 'components()' again - same results
#' components(mod)
#'
#' ## reset seeds
#' mod <- set_seeds(mod)
#'
#' ## calling 'set_seeds' unfits the model
#' is_fitted(mod)
#'
#' ## so we fit it again
#' mod <- fit(mod)
#'
#' ## when we call components, we get
#' ## different results from earlier
#' components(mod)
#' @export
set_seeds <- function(mod, new_seeds = NULL) {
  check_bage_mod(x = mod, nm_x = "mod")
  if (is.null(new_seeds)) {
    mod$seed_components <- make_seed()
    mod$seed_augment <- make_seed()
    mod$seed_forecast_components <- make_seed()
    mod$seed_forecast_augment <- make_seed()
  }
  else {
    check_new_seeds(new_seeds)
    mod$seed_components <- new_seeds$seed_components
    mod$seed_augment <- new_seeds$seed_augment
    mod$seed_forecast_components <- new_seeds$seed_forecast_components
    mod$seed_forecast_augment <- new_seeds$seed_forecast_augment
  }
  mod <- unfit(mod)
  mod
}


## 'set_var_age' --------------------------------------------------------------

#' Specify Age Variable
#'
#' Specify which variable (if any) represents age.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the age variable
#' from variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#'
#' `~ age + region + age:region`
#'
#' contains variables `age` and `region`,
#' and terms `age`, `region`, and `age:region`.
#'
#' By default, **bage** gives a term involving age a
#' ([RW()]) prior. Changing the age variable
#' via `set_var_age()` can change priors:
#' see below for an example.
#'
#' If `set_var_age()` is applied to
#' a fitted model, `set_var_age()` [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @inheritParams set_confidential_rr3
#' @param name The name of the age variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_sexgender()] Set sex or gender variable
#' - [set_var_time()] Set time variable
#' - [is_fitted()] Test whether a model is fitted
#' - internally, **bage** uses [poputils::find_var_age()]
#'   to locate age variables
#'
#' @examples
#' ## rename 'age' variable to something unusual
#' injuries2 <- nzl_injuries
#' injuries2$age_last_birthday <- injuries2$age
#'
#' ## mod_pois does not recognize age variable
#' mod <- mod_pois(injuries ~ age_last_birthday * ethnicity + year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the age variable explicitly
#' ## (which, as a side effect, changes the prior on
#' ## the age main effect)
#' mod |>
#'   set_var_age(name = "age_last_birthday")
#' @export
set_var_age <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "age")
}


## 'set_var_sexgender' --------------------------------------------------------

## HAS_TESTS
#' Specify Sex or Gender Variable
#'
#' Specify which variable (if any) represents sex or gender.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the sex/gender variable
#' from variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#'
#' `~ gender + region + gender:region`
#'
#' contains variables `gender` and `region`,
#' and terms `gender`, `region`, and `gender:region`.
#'
#' If `set_var_sexgender()` is applied to
#' a fitted model, `set_var_sexgender()` [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @inheritParams set_confidential_rr3
#' @param name The name of the sex or gender variable.
#'
#' @returns A `"bage_mod"` object
#'
#' @seealso
#' - [set_var_age()] Set age variable
#' - [set_var_time()] Set time variable
#' - [is_fitted()] Test whether model is fitted
#' - internally, **bage** uses [poputils::find_var_sexgender()]
#'   to locate sex or gender variables
#' - internally, **bage** uses [poputils::find_label_female()]
#'   to locate female categories within a sex or gender variable
#' - internally, **bage** uses [poputils::find_label_male()]
#'   to locate male categories within a sex or gender variable
#'
#' @examples
#' ## rename 'sex' variable to something unexpected
#' injuries2 <- nzl_injuries
#' injuries2$biological_sex <- injuries2$sex
#'
#' ## mod_pois does not recognize sex variable
#' mod <- mod_pois(injuries ~ age * biological_sex + year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the sex variable explicitly
#' mod |>
#'   set_var_sexgender(name = "biological_sex")
#' @export
set_var_sexgender <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "sexgender")
}


## 'set_var_time' --------------------------------------------------------------

## HAS_TESTS
#' Specify Time Variable
#'
#' Specify which variable (if any) represents time.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the time variable
#' from variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#'
#' `~ time + region + time:region`
#'
#' contains variables `time` and `region`,
#' and terms `time`, `region`, and `time:region`.
#'
#' By default, **bage** gives a term involving time a
#' ([RW()]) prior. Changing the time variable
#' via `set_var_time()` can change priors:
#' see below for an example.
#'
#' If `set_var_time()` is applied to
#' a fitted model, `set_var_time()` [unfits][unfit()]
#' the model, deleting existing estimates.
#'
#' @inheritParams set_confidential_rr3
#' @param name The name of the time variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_age()] Set age variable
#' - [set_var_sexgender()] Sex sex or gender variable
#' - [is_fitted()] Test if model has been fitted
#' - internally, **bage** uses [poputils::find_var_time()]
#'   to locate time variables
#'
#' @examples
#' ## rename time variable to something unusual
#' injuries2 <- nzl_injuries
#' injuries2$calendar_year <- injuries2$year
#'
#' ## mod_pois does not recognize time variable
#' mod <- mod_pois(injuries ~ age * ethnicity + calendar_year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the time variable explicitly
#' ## (which, as a side effect, changes the prior on
#' ## the time main effect)
#' mod |>
#'   set_var_time(name = "calendar_year")
#' @export
set_var_time <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "time")
}


## HAS_TESTS
#' Unfit a Model
#'
#' Reset a model, deleting all estimates.
#'
#' @param mod A fitted object of class `"bage_mod"`,
#' object, created through a call to [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns An unfitted version of `mod`.
#'
#' @seealso
#' - [fit()] Fit a model
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a model
#' - [set_seeds()] Reset random seeds
#' - Functions such as [set_prior()], [set_disp()] and
#'   [set_var_age()] unfit models as side effects.
#'
#' @examples
#' ## create a model, which starts out unfitted
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' is_fitted(mod)
#'
#' ## calling 'fit' produces a fitted version
#' mod <- fit(mod)
#' is_fitted(mod)
#'
#' ## calling 'unfit' resets the model
#' mod <- unfit(mod)
#' is_fitted(mod)
#' @export
unfit <- function(mod) {
  check_bage_mod(x = mod, nm_x = "mod")
  mod["draws_effectfree"] <- list(NULL)
  mod["draws_hyper"] <- list(NULL)
  mod["draws_hyperrandfree"] <- list(NULL)
  mod["draws_coef_covariates"] <- list(NULL)
  mod["draws_disp"] <- list(NULL)
  mod["draws_datamod_param"] <- list(NULL)
  mod["point_effectfree"] <- list(NULL)
  mod["point_hyper"] <- list(NULL)
  mod["point_hyperrandfree"] <- list(NULL)
  mod["point_coef_covariates"] <- list(NULL)
  mod["point_disp"] <- list(NULL)
  mod["point_datamod_param"] <- list(NULL)
  mod["computations"] <- list(NULL)
  mod["oldpar"] <- list(NULL)
  mod["vars_inner"] <- list(NULL)
  mod
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Specify var_age, var_sexgender, or var_time
#'
#' Can include resetting priors.
#' Called by user-visible functions
#' 'set_var_age', 'set_var_sexgender',
#' and 'set_var_time'.
#'
#' @param mod A `bage_mod` object.
#' @param name The name of the variable.
#' @param var "age", "sexgender", or "time"
#'
#' @returns A `bage_mod` object
#'
#' @noRd
set_var_inner <- function(mod, name, var) {
  choices <- c("age", "sexgender", "time")
  check_bage_mod(x = mod, nm_x = "mod")
  var <- match.arg(var, choices = choices)
  vars_oth <- setdiff(choices, var)
  attr_name <- paste0("var_", var)
  attr_names_oth <- paste0("var_", vars_oth)
  ## extract values
  formula <- mod$formula
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  name_old <- mod[[attr_name]]
  names_oth <- lapply(attr_names_oth, function(nm) mod[[nm]])
  has_name_old <- !is.null(name_old)
  names_priors <- names(priors)
  lengths_effects <- make_lengths_effect(dimnames_terms)
  ## check 'name'
  check_string(x = name, nm_x = "name")
  check_formula_has_variable(name = name, formula = formula)
  for (i_oth in seq_along(names_oth)) {
    nm_oth <- names_oth[[i_oth]]
    if (!is.null(nm_oth)) {
      if (identical(name, nm_oth)) {
        cli::cli_abort(c("Variables for {var} and {vars_oth[[i_oth]]} have the same name.",
                         i = "{.var {attr_name}} is {.val {name}}.",
                         i = "{.var {attr_names_oth[[i_oth]]}} is {.val {name}}."))
      }
    }
  }
  ## modify var
  mod[[attr_name]] <- name
  ## reset priors
  var_age <- mod[["var_age"]]
  var_time <- mod[["var_time"]]
  length_effect <- lengths_effects[[name]]
  priors[[name]] <- default_prior(nm_term = name,
                                  var_age = var_age,
                                  var_time = var_time,
                                  length_effect = length_effect)
  if (has_name_old) {
    length_effect_old <- lengths_effects[[name_old]]
    priors[[name_old]] <- default_prior(nm_term = name_old,
                                        var_age = var_age,
                                        var_time = var_time,
                                        length_effect = length_effect_old)
  }
  ## modify priors
  mod$priors <- priors
  ## unfit
  mod <- unfit(mod)
  ## return
  mod
}



