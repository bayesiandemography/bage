norm <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_norm(scale = scale)
}

new_bage_prior_norm <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L)
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}
    
    
    

## RW <- function(scale = 1) {
##     checkmate::assert_number(scale)
##     if (scale <= 0)
##         stop(gettextf("value for '%s' [%s] is non-positive",
##                       "scale", scale),
##              call. = FALSE)
##     new_bage_prior_rw(scale)
## }


## new_bage_prior_rw <- function(scale) {
##     ans <- list(scale = scale)
##     class(ans) <- c("bage_prior_rw", "bage_prior")
##     ans
## }
