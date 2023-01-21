
#' Simulate from a model
#'
#' @param x_est A `bage_mod` object.
#' @param x_gen `NULL` or a `bage_mod` object.
#' @param n_sim Number of draws from simulated posterior.
#' Defaults to 1.
#'
#' @returns A `bage_mod_sim` object
#'
#' @export    
sim <- function(x_est, x_gen = NULL, n_sim = 1, known = "(Intercept)") {
    ## TODO add some checks
    n_sim <- checkmate::assert_int(lower = 1L, coerce = TRUE)
    if (is.null(x_gen))
        x_gen <- x_est
    vals <- generate_vals(x = x_gen,
                          n = n_sim,
                          known = known)
    hyper <- vals$hyper
    par <- vals$par
    outcome <- vals$outcome
    est <- vector(mode = "list", length = n_sim)
    prec <- vector(mode = "list", length = n_sim)
    for (i_sim in seq_len(n_sim)) {
        x_est$outcome <- outcome[[i_sim]]
        x_est <- fit(x_est)
        est[[i_sim]] <- x_est$est
        prec[[i_sim]] <- x_est$prec
    }
    ans <- x_gen
    ans$hyper <- hyper
    ans$par <- par
    ans$outcome <- outcome
    ans$est <- est
    ans$prec <- prec
    class_old <- class(x_gen)
    class_new <- c(paste0(class_old[[1L]], "_sim"), class_old)
    class(ans) <- class_new
    ans
}
    
        

        

generate_vals <- function(x, n, known) {
    priors <- x$priors
    offset <- x$offset
    matrix_par <- make_combined_matrix_par(x)
    inv_transform <- get_inv_transform(x)
    align_to_data <- get_align_to_data(x)
    draw_outcome <- get_draw_outcome(x)
    hyper_par <- lapply(priors, draw_hyper_par)
    hyper <- hyper_par[[1L]]
    par <- hyper_par[[2L]]
    linear_pred <- m %*% par
    mean <- inv_transform(linear_pred)
    outcome <- draw_outcome(mean = mean, offset = offset)
    list(hyper = hyper,
         par = par,
         outcome = outcome)
}
    
    
    
    
        
            
