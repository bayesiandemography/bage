
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
