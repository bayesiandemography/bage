
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





## #' Simulate from a model
## #'
## #' - fit model to x_gen
## #' - fix any 'known' quantities to their posterior means
## #' - for i_sim in 1:n_sim
## #'     - draw remaining parameters, plus outcome, in x_gen
## #'     - supply 'outcome' to x_est
## #'     - fit x_est; store posterior means and prec
## #'
## #' note - any NAs in outcome of x_gen are carried through to x_est
## #'    (though underlying true value is generated and stored - to allow
## #'     for study of performance of imputation)
## #'
## #' 
## #' 
## #'
## #' @param x_est A `bage_mod` object.
## #' @param x_gen `NULL` or a `bage_mod` object.
## #' @param n_sim Number of draws from simulated posterior.
## #' Defaults to 1.
## #'
## #' @returns A `bage_mod_sim` object
## #'
## #' @export    
## sim <- function(x_est, x_gen = NULL, n_sim = 1, known = "(Intercept)") {
##     ## TODO add some checks
##     n_sim <- checkmate::assert_int(lower = 1L, coerce = TRUE)
##     if (is.null(x_gen))
##         x_gen <- x_est
##     vals <- generate_vals(x = x_gen,
##                           n = n_sim,
##                           known = known)
##     hyper <- vals$hyper
##     par <- vals$par
##     outcome <- vals$outcome
##     est <- vector(mode = "list", length = n_sim)
##     prec <- vector(mode = "list", length = n_sim)
##     for (i_sim in seq_len(n_sim)) {
##         x_est$outcome <- outcome[[i_sim]]
##         x_est <- fit(x_est)
##         est[[i_sim]] <- x_est$est
##         prec[[i_sim]] <- x_est$prec
##     }
##     ans <- x_gen
##     ans$hyper <- hyper
##     ans$par <- par
##     ans$outcome <- outcome
##     ans$est <- est
##     ans$prec <- prec
##     class_old <- class(x_gen)
##     class_new <- c(paste0(class_old[[1L]], "_sim"), class_old)
##     class(ans) <- class_new
##     ans
## }
    

## generate_vals <- function(x, n_draw, known) {
##     priors <- x$priors
##     par <- x$par
##     draw_outcome <- get_draw_outcome(x)
##     vals_hyper <- lapply(priors, draw_vals_hyper, n_draw = n_draw)
##     length_par <- length(unlist(par))
##     vals_par <- .mapply(draw_vals_par,
##                         dots = list(prior = priors,
##                                     vals_hyper = vals_hyper),
##                         MoreArgs = list(length_par = length_par,
##                                         n_draw = n_draw))
##     vals_par_m <- do.call(rbind, vals_par)
##     linear_pred <- matrix_par %*% vals_par_m
##     mean <- inv_transform(linear_pred)
##     outcome <- draw_outcome(mean = mean,
##                             offset = offset)
##     list(hyper = hyper,
##          par = par,
##          outcome = outcome)
## }
