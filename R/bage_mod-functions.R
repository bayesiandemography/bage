
## User-visible functions that look like methods, but technically are not

## 'set_age_var' --------------------------------------------------------------

#' Set the age variable
#'
#' Specify which variable (if any) represents age.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the age variable,
#' based on variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#' 
#' `~ age + region + age:region`
#'
#' has variables `age` and `region`,
#' and terms `age`, `region`, and `age:region`.
#'
#' By default, an age main effect has a random walk
#' ([RW()]) prior. Calling `set_age_prior()` can
#' affect priors: see below for an example.
#' 
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param name The name of the age variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso [set_time_var()]
#' 
#' @examples
#' ## rename 'age' variable to something unusual
#' injuries2 <- injuries
#' injuries2$age_last_birthday <- age
#'
#' ## mod_pois does not recognize age variable
#' mod <- mod_pois(injuries ~ age_last_birthday * ethnicity + year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the age variable explicitly
#' ## (which changes the prior on
#' ## the age main effect)
#' mod |>
#'   set_age(name = "age_last_birthday")
#' @export
set_age_var <- function(mod, name) {
    checkmate::assert_class(mod, "bage_mod")
    ## extract values
    formula <- mod$formula
    priors <- mod$priors
    age_var_old <- mod$age_var
    time_var <- mod$time_var
    has_age_var_old <- !is.null(age_var_old)
    has_time_var <- !is.null(time_var)
    names_priors <- names(priors)
    ## check 'name'
    checkmate::assert_string(name, min.chars = 1L)
    check_formula_has_variable(name = name, formula = formula)
    if (has_time_var) {
        if (identical(name, time_var))
            stop(gettextf("age variable and time variable have same name [\"%s\"]",
                          name),
                 call. = FALSE)
    }
    ## reset priors if appropriate
    has_age_main_effect <- is_main_effect(name = name,
                                          formula = formula)
    if (has_age_main_effect)
        priors[[name]] <- RW()
    if (has_age_var_old) {
        had_age_main_effect <- is_main_effect(name = age_var_old,
                                              formula = formula)
        if (had_age_main_effect)
            priors[[age_var_old]] <- N()
    }
    ## modify model object and return
    mod$priors <- priors
    mod$age_var <- name
    mod
}    


## 'set_n_draw' ---------------------------------------------------------------

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
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
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
#' for a main effect or interaction.
#'
#' If the `mod` argument to `set_prior` is
#' a fitted model, then `set_prior` 'unfits'
#' `mod`, ie `set_prior` deletes existing
#' estimates and returns `mod` to an
#' unfitted state.
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
#' @returns A `bage_mod` object.
#'
#' @seealso [N()], [RW()], [RW2()], [AR1()], [Known()]
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_prior(age ~ RW2())
#' @export
set_prior <- function(mod, formula) {
    checkmate::assert_class(mod, "bage_mod")
    check_format_prior_formula(formula)
    nm_response <- deparse1(formula[[2L]])
    nms_terms <- names(mod$priors)
    i <- match(nm_response, nms_terms, nomatch = 0L)
    if (i == 0L)
        stop(gettextf(paste("response in prior formula '%s' not a",
                            "valid term from model formula '%s' :",
                            "valid terms are %s"),
                      deparse1(formula),
                      deparse1(mod$formula),
                      paste(sprintf("'%s'", nms_terms), collapse = ", ")),
             call. = FALSE)
    prior <- tryCatch(eval(formula[[3L]]),
                      error = function(e) e)
    if (inherits(prior, "error"))
        stop(gettextf("prior formula '%s' invalid : %s",
                      deparse1(formula),
                      prior$message),
             call. = FALSE)
    mod$priors[[i]] <- prior
    mod["est"] <- list(NULL)
    mod["prec"] <- list(NULL)
    mod
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
