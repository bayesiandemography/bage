
## User-visible functions that look like methods, but technically are not

## 'set_disp' -----------------------------------------------------------------

## NO_TESTS
#' Set scale parameter for dispersion
#'
#' Specify the scale parameter `s` in the prior
#' for dispersion.
#'
#' In Poisson and binomial models,
#' `s` can be set to `0`, implying
#' that the dispersion term is also `0`.
#' In normal models, `s` must be non-negative.
#'
#' If the `mod` argument to `set_disp` is
#' a fitted model, then `set_disp` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param s Scale term. In Poisson and
#' binomial models, `s` must be non-negative.
#' In normal models, `s` must be positive.
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
#' mod |> set_disp(s = 0.1)
#' mod |> set_disp(s = 0)
#' @export
set_disp <- function(mod, s) {
    checkmate::assert_class(mod, "bage_mod")
    nm_distn <- nm_distn(mod)
    zero_ok <- nm_distn %in% c("pois", "binom")
    check_scale(s, x_arg = "s", zero_ok = zero_ok)
    scale_disp <- as.double(s)
    mod$scale_disp <- scale_disp
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
    checkmate::assert_class(mod, "bage_mod")
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 0L,
                                    coerce = TRUE)
    mod$n_draw <- n_draw
    mod
}


## 'set_prior' ----------------------------------------------------------------

## HAS_TESTS
#' Change the prior for a model term
#'
#' Specify a non-default prior distribution
#' for the intercept, a main effect,
#' or an interaction.
#'
#' If the `mod` argument to `set_prior` is
#' a fitted model, then `set_disp` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' `formula` gives the name of a main
#' effect or interaction, and a function
#' specifying a prior, eg
#' `age ~ RW2()`.
#'
#' @param mod A `bage_mod` object, created with
#' [mod_pois()], [mod_binom()], or [mod_norm()].
#' @param formula A formula giving the term
#' and a function for creating a prior.
#'
#' @returns A modified `bage_mod` object.
#'
#' @seealso
#' - [N()]
#' - [NFixed()]
#' - [RW()]
#' - [RW2()]
#' - [AR1()]
#' - [Known()]
#' - [SVD()]
#' - [Spline()]
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
    checkmate::assert_class(mod, "bage_mod")
    check_format_prior_formula(formula)
    nms_terms <- names(mod$priors)
    matrices_par_outcome <- mod$matrices_par_outcome
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
    length_par <- ncol(matrices_par_outcome[[i]])
    agesex <- make_agesex_inner(nm = nms_terms[[i]],
                                var_age = var_age,
                                var_sexgender = var_sexgender)
    is_prior_ok_for_term(prior = prior,
                         nm = nm_response,
                         length_par = length_par,
                         agesex = agesex)
    mod$priors[[i]] <- prior
    mod <- unfit(mod)
    mod
}


## Seasonal effect ------------------------------------------------------------

## NO_TESTS
#' Add a seasonal effect
#'
#' Add a seasonal effect to a model.
#'
#' TODO - description
#'
#' If the `mod` argument to `set_season` is
#' a fitted model, then `set_season` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param n Number of seasons.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#'
#' @returns A modified `bage_mod` object.
#'
#' @seealso
#' - [is_fitted()]
#' 
#' @examples
#' mod <- mod_pois(deaths ~ month,
#'                 data = us_acc_deaths,
#'                 exposure = 1)
#' mod
#' mod |> set_season(n = 12)
#' @export
set_season <- function(mod, n, s = 1) {
    checkmate::assert_class(mod, "bage_mod")
    check_n(n, min = 2L, max = NULL, null_ok = FALSE)
    n <- as.integer(n)
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    var_time <- mod$var_time
    priors <- mod$priors
    if (is.null(var_time))
        cli::cli_abort(c("Can't specify seasonal effect when time variable not identified.",
                         i = "Please use {.fun set_var_time} to identify time variable."))
    nms_terms <- names(priors)
    if (!(var_time %in% nms_terms))
        cli::cli_abort(c(paste("Can't use seasonal effect when model does not",
                               "contain time main effect."),
                         i = "Model has terms {.val {nms_terms}}."))
    n_time <- n_time(mod)
    if (n > (n_time %/% 2L))
        cli::cli_abort(c(paste("Estimation period not long enough to use seasonal effect",
                               "with {n} seasons."),
                         i = "Must have at least two time points for each season.",
                         i = "Data used for estimation has {n_time} time points."))
    mod$n_season <- n
    mod$scale_season <- s
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
#' By default, an age main effect has a random walk
#' ([RW()]) prior. Changing the age variable
#' via `set_var_age()` can change priors:
#' see below for an example.
#'
#' If the `mod` argument to `set_var_age` is
#' a fitted model, then `set_var_age` 'unfits'
#' the model, by deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param name The name of the age variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_sexgender()] sets the sex or gender variable
#' - [set_var_time()] sets the time variable
#' - internally, `bage` uses [poputils::find_var_age()]
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
#' the model, by deleting existing estimates.
#' 
#' @inheritParams set_disp
#' @param name The name of the sex or gender variable.
#'
#' @returns A `"bage_mod"` object
#'
#' @seealso
#' - [set_var_age()] sets the age variable
#' - [set_var_time()] sets the time variable
#' - internally, `bage` uses [poputils::find_var_sexgender()]
#'   to locate sex or gender variables
#' - internally, `bage` uses [poputils::find_label_female()]
#'   to locate female categories within a sex or gender variable
#' - internally, `bage` uses [poputils::find_label_male()]
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
#' By default, an time main effect has a random walk
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
#' - internally, `bage` uses [poputils::find_var_time()]
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


## generate.bage_mod <- function(x, known = "(Intercept)", ...) {
##     nm_distn <- x$nm_distn
##     is_fitted <- !is.null(x$est)
##     if (!is_fitted)
##         x <- fit(x)
##     nms_priors <- names(x$priors)
##     check_known(known = known,
##                 nms_priors = nms_priors,
##                 nm_distn = nm_distn)
##     if (known == ".")
##         generate_all(x)
##     else if (known %in% c("rate", "prob", "mean"))
##         generate_outcome(x)
##     else
##         generate_partial(mod, known = known)
## }

## fit.bage_mod_sim <- function(x) {
##     NULL
## }


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
    checkmate::assert_class(mod, "bage_mod")
    checkmate::assert_choice(var, choices = choices)
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
    matrices_par_outcome <- mod$matrices_par_outcome
    ## check 'name'
    checkmate::assert_string(name, min.chars = 1L)
    check_formula_has_variable(name = name, formula = formula)
    for (i_oth in seq_along(names_oth)) {
        nm_oth <- names_oth[[i_oth]]
        if (!is.null(nm_oth)) {
            if (identical(name, nm_oth))
                stop(gettextf("%s variable and %s variable have same name [\"%s\"]",
                              var,
                              vars_oth[[i_oth]],
                              name),
                     call. = FALSE)
        }
    }
    ## modify var
    mod[[attr_name]] <- name
    ## reset priors
    var_age <- mod[["var_age"]]
    var_time <- mod[["var_time"]]
    length_par <- ncol(matrices_par_outcome[[name]])
    priors[[name]] <- default_prior(nm_term = name,
                                    var_age = var_age,
                                    var_time = var_time,
                                    length_par = length_par)
    if (has_name_old) {
        length_par_old <- ncol(matrices_par_outcome[[name_old]])
        priors[[name_old]] <- default_prior(nm_term = name_old,
                                            var_age = var_age, 
                                            var_time = var_time,
                                            length_par = length_par)
    }
    ## modify priors
    mod$priors <- priors
    ## unfit
    mod <- unfit(mod)
    ## return
    mod
}


## NO_TESTS
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
    mod["prec"] <- list(NULL)
    mod["R_prec"] <- list(NULL)
    mod
}
