

## TODO - set_errors()
## TODO - set_season()
## TODO - set_data_source()


#' Set the number of draws
#'
#' Specify the number of draws from the posterior
#' distribution to be used in model output.
#' A newly-created `bage_mod` object has an
#' `n_draw` value of 1000. Higher values
#' may be appropriate for characterising
#' the tails of distributions, or for
#' publication-quality graphics or summaries.
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




set_prior <- function(mod, formula) {
    check_valid_prior_formula(formula)
    nm_response <- as.character(formula[[2L]])
    prior <- tryCatch(eval(formula[[3L]]),
                      error = function(e) e)
    if (inherits(prior, "error"))
        stop(gettextf("prior '%s' invalid : %s",
                      nm_response,
                      prior$message),
             call. = FALSE)
    nms_priors <- names(mod$priors)
    i <- match(nm_response, nms_priors, nomatch = 0L)
    if (i == 0L)
        stop(gettextf("'%s' is not a valid for formula '%s' : valid terms are %s",
                      nm_response,
                      mod$formula,
                      paste(nms_priors, collapse = ", ")),
             call. = FALSE)
    mod$priors[[i]] <- prior
    mod
}

