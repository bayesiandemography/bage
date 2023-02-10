
## User-visible functions that look a bit like methods, but technically are not

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
