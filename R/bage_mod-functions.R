
## User-visible functions that look like methods, but technically are not

## 'set_disp' -----------------------------------------------------------------

## HAS_TESTS
#' Set Mean in Prior for Dispersion or Standard Deviation
#'
#' Specify the mean of prior for the dispersion
#' parameter (in Poisson and binomial models) or the
#' standard deviation parameter (in normal models.)
#'
#' The dispersion or mean parameter has an exponential
#' distribution with mean \eqn{\mu}.
#'
#' \deqn{p(\xi) = \frac{1}{\mu}\exp\left(\frac{-\xi}{\mu}\right)}
#'
#' In Poisson and binomial models,
#' `mean` can be set to `0`, implying
#' that the dispersion term is also `0`.
#' In normal models, `mean` must be non-negative.
#'
#' If the `mod` argument to `set_disp` is
#' a fitted model, then `set_disp` 'unfits'
#' the model, deleting existing estimates.
#' 
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param mean Mean value for the expoential prior.
#' In Poisson and binomial models, can be set to 0.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [is_fitted()]
#' 
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_disp(mean = 0.1)
#' mod |> set_disp(mean = 0)
#' @export
set_disp <- function(mod, mean) {
    check_bage_mod(x = mod, nm_x = "mod")
    nm_distn <- nm_distn(mod)
    zero_ok <- nm_distn %in% c("pois", "binom")
    check_scale(mean, x_arg = "mean", zero_ok = zero_ok)
    mean_disp <- as.double(mean)
    mod$mean_disp <- mean_disp
    mod <- unfit(mod)
    mod
}


## 'set_n_draw' ---------------------------------------------------------------

## HAS_TESTS
#' Set the number of draws
#'
#' Specify the number of draws from the posterior
#' distribution to be used in model output.
#' A newly-created `bage_mod` object has an
#' `n_draw` value of 1000. Higher values
#' may be appropriate for characterising
#' the tails of distributions, or for
#' publication-quality graphics and summaries.
#'
#' The value of `n_draw` does not affect
#' model fitting: it only affects posterior
#' summaries.
#'
#' @inheritParams set_disp
#' @param n_draw Number of draws.
#'
#' @returns A `bage_mod` object
#' 
#' @seealso Functions whose output is affected
#' by the number of draws include [bage::augment()].
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#'
#' mod |>
#'   set_n_draw(n_draw = 5000)
#' @export
set_n_draw <- function(mod, n_draw = 1000L) {
  check_bage_mod(x = mod, nm_x = "mod")
  check_n(n = n_draw,
          nm_n = "n_draw",
          min = 0L,
          max = NULL,
          null_ok = FALSE)
  n_draw <- as.integer(n_draw)
  n_draw_old <- mod$n_draw
  mod$n_draw <- n_draw
  if (is_fitted(mod)) {
    if (n_draw > n_draw_old)
      mod <- make_stored_draws(mod)
    if (n_draw < n_draw_old) {
      s <- seq_len(n_draw)
      mod$draws_linpred <- mod$draws_linpred[, s, drop = FALSE] 
      mod$draws_hyper <- mod$draws_hyper[, s, drop = FALSE]
      if (has_disp(mod))
        mod$draws_disp <- mod$draws_disp[s]
    }
  }
  mod
}


## 'set_prior' ----------------------------------------------------------------

## HAS_TESTS
#' Specify a Prior
#'
#' Specify a prior distribution for an intercept,
#' a main effect, or an interaction.
#'
#' If the `mod` argument to `set_prior` is
#' a fitted model, then `set_disp` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' @param mod A `bage_mod` object, created with
#' [mod_pois()], [mod_binom()], or [mod_norm()].
#' @param formula A formula giving the term
#' and a function for creating a prior.
#'
#' @returns A modified `bage_mod` object.
#'
#' @seealso
#' - [priors] Current choices for prior distributions.
#' - [is_fitted()]
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_prior(age ~ RW2())
#' @export
set_prior <- function(mod, formula) {
  nm_response <- deparse1(formula[[2L]])
  check_bage_mod(x = mod, nm_x = "mod")
  check_format_prior_formula(formula)
  nms_terms <- names(mod$priors)
  matrices_effect_outcome <- mod$matrices_effect_outcome
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  matrices_along_by <- mod$matrices_along_by
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
  if (uses_along(prior)) {
    matrix_along_by <- choose_matrix_along_by(prior = prior,
                                              matrices = matrices_along_by[[i]],
                                              var_time = var_time,
                                              var_age = var_age)
  }
  else {
    length_effect <- ncol(matrices_effect_outcome[[i]])
    matrix_along_by <- matrix(seq.int(from = 0L, to = length_effect - 1L),
                              ncol = 1L)
  }
  agesex <- make_agesex_inner(nm = nms_terms[[i]],
                              var_age = var_age,
                              var_sexgender = var_sexgender)
  is_prior_ok_for_term(prior = prior,
                       nm = nm_response,
                       matrix_along_by = matrix_along_by,
                       var_time = var_time,
                       var_age = var_age,
                       agesex = agesex)
  mod$priors[[i]] <- prior
  mod <- unfit(mod)
  mod
}


## 'set_var_age' --------------------------------------------------------------

#' Set the age variable
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
#' If the `mod` argument to `set_var_age` is
#' a fitted model, then `set_var_age` 'unfits'
#' the model by deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param name The name of the age variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_sexgender()] sets the sex or gender variable
#' - [set_var_time()] sets the time variable
#' - internally, **bage** uses [poputils::find_var_age()]
#'   to locate age variables
#' - [is_fitted()]
#' 
#' @examples
#' ## rename 'age' variable to something unusual
#' injuries2 <- injuries
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
#' Set a sex or gender variable
#'
#' Specify which variable (if any) represents sex or gender.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to find a sex or gender variable
#' using variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#' 
#' `~ gender + region + gender:region`
#'
#' contains variables `gender` and `region`,
#' and terms `gender`, `region`, and `gender:region`.
#'
#' If the `mod` argument to `set_var_sexgender` is
#' a fitted model, then `set_var_sexgender` 'unfits'
#' the model deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param name The name of the sex or gender variable.
#'
#' @returns A `"bage_mod"` object
#'
#' @seealso
#' - [set_var_age()] sets the age variable
#' - [set_var_time()] sets the time variable
#' - internally, **bage** uses [poputils::find_var_sexgender()]
#'   to locate sex or gender variables
#' - internally, **bage** uses [poputils::find_label_female()]
#'   to locate female categories within a sex or gender variable
#' - internally, **bage** uses [poputils::find_label_male()]
#'   to locate male categories within a sex or gender variable
#' - [is_fitted()]
#' 
#' @examples
#' ## rename 'sex' variable to something unexpected
#' injuries2 <- injuries
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
#' Set the time variable
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
#' If the `mod` argument to `set_var_time` is
#' a fitted model, then `set_var_time` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param name The name of the time variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_age()] sets the age variable
#' - [set_var_sexgender()] sets the sex or gender
#' - internally, **bage** uses [poputils::find_var_time()]
#'   to locate time variables
#' - [is_fitted()]
#'
#' @examples
#' ## rename time variable to something unusual
#' injuries2 <- injuries
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



## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Set var_age, var_sexgender, or var_time
#'
#' Can include resetting priors.
#' Called by user-visible functions
#' 'set_var_age', 'sex_var_sexgender',
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
  name_old <- mod[[attr_name]]
  names_oth <- lapply(attr_names_oth, function(nm) mod[[nm]])
  has_name_old <- !is.null(name_old)
  names_priors <- names(priors)
  matrices_effect_outcome <- mod$matrices_effect_outcome
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
  length_effect <- ncol(matrices_effect_outcome[[name]])
  priors[[name]] <- default_prior(nm_term = name,
                                  var_age = var_age,
                                  var_time = var_time,
                                  length_effect = length_effect)
  if (has_name_old) {
    length_effect_old <- ncol(matrices_effect_outcome[[name_old]])
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


## HAS_TESTS
#' Reset a model
#'
#' @param mod A `bage_mod` object.
#'
#' @returns A `bage_mod` object
#'
#' @noRd
unfit <- function(mod) {
    mod["est"] <- list(NULL)
    mod["is_fixed"] <- list(NULL)
    mod["R_prec"] <- list(NULL)
    mod["scaled_eigen"] <- list(NULL)
    mod["draws_linpred"] <- list(NULL)
    mod["draws_hyper"] <- list(NULL)
    mod["draws_disp"] <- list(NULL)
    mod
}
