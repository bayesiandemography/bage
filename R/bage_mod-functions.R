
## User-visible functions that look like methods, but technically are not


## 'set_datamod_outcome_rr3' ----------------------------------------------------------

#' Specify RR3 Data Model
#'
#' Specify a data model where the outcome variable
#' has been randomly rounded to base 3.
#'
#' `set_datamod_outcome_rr3()` can only be used with
#' Poisson and binomial models (created with
#' [mod_pois()] and [mod_binom()].)
#'
#' Random rounding to base 3 (RR3) is a confidentialization
#' technique that is sometimes applied by statistical
#' agencies. RR3 is applied to integer data. The
#' procedure for rounding value \eqn{n} is as follows:
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
#' If `set_datamod_outcome_rr3()` is applied to
#' a fitted model, it 'unfits'
#' the model, deleting existing estimates.
#'
#' @param mod An object of class `"bage_mod"`,
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns A modified version of `mod`.
#'
#' @seealso
#' - [datamods] Overview of data models implemented in **bage**
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#'   model for rates, probabilities, or means
#'
#' @examples
#' ## 'injuries' variable in 'nzl_injuries' dataset
#' ## has been randomly rounded to base 3
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn) |>
#'   set_datamod_outcome_rr3() |>
#'   fit()
#' @export
set_datamod_outcome_rr3 <- function(mod) {
  ## check is valid distribution
  valid_distn <- c("binom", "pois")
  nm_distn <- nm_distn(mod)
  if (!(nm_distn %in% valid_distn))
    cli::cli_abort(c("Outcome has {.val {nm_distn}} distribution.",
                     i = "RR3 data model can only be used with {.val {valid_distn}} distributions."))
  ## check that values for outcome all divisible by 3
  outcome <- mod$outcome
  is_base3 <- is.na(outcome) | ((outcome %% 3L) == 0L)
  n_not_base3 <- sum(!is_base3)
  if (n_not_base3 > 0L)
    cli::cli_abort("Outcome variable has {cli::qty(n_not_base3)} value{?s} not divisible by 3.")
  ## return
  mod$datamod_outcome <- new_bage_datamod_outcome_rr3()
  mod <- unfit(mod)
  mod
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
#' In Poisson and binomial models,
#' `mean` can be set to `0`, implying
#' that the dispersion term is also `0`.
#' In normal models, `mean` must be non-negative.
#'
#' If `set_disp()` is applied to
#' a fitted model, it 'unfits'
#' the model, deleting existing estimates.
#'
#' @inheritParams set_datamod_outcome_rr3
#' @param mean Mean value for the exponential prior.
#' In Poisson and binomial models, can be set to 0.
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
set_disp <- function(mod, mean) {
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
#' @inheritParams set_datamod_outcome_rr3
#' @param n_draw Number of draws.
#'
#' @returns A `bage_mod` object
#' 
#' @seealso
#' - [bage::augment()], [bage::components()] functions for
#'   drawing from prior or posterior distribution - the output
#'   of which is affected by the value of `n_draw`.
#' - [mod_pois()], [mod_binom()], [mod_norm()] Specify a
#' model
#' - [set_prior()] Specify prior for a term
#' - [set_disp()] Specify prior for dispersion
#' - [fit()] Fit a model
#' - [unfit()] Reset a model
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = nzl_injuries,
#'                 exposure = popn)
#' mod
#'
#' mod |>
#'   set_n_draw(n_draw = 5000)
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
      if (has_disp(mod))
        mod$draws_disp <- mod$draws_disp[s]
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
#' a fitted model, it 'unfits'
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
#' a fitted model, it 'unfits'
#' the model, deleting existing estimates.
#' 
#' @inheritParams set_datamod_outcome_rr3
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
#' a fitted model, it 'unfits'
#' the model, deleting existing estimates.
#' 
#' @inheritParams set_datamod_outcome_rr3
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
#' a fitted model, it 'unfits'
#' the model, deleting existing estimates.
#' 
#' @inheritParams set_datamod_outcome_rr3
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
  mod["draws_disp"] <- list(NULL)
  mod["point_effectfree"] <- list(NULL)
  mod["point_hyper"] <- list(NULL)
  mod["point_hyperrandfree"] <- list(NULL)
  mod["point_disp"] <- list(NULL)
  mod["computations"] <- list(NULL)
  mod["oldpar"] <- list(NULL)
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

