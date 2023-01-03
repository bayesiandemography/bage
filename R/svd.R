
## HAS_TESTS
#' Prepare a matrix of rates for 
#' a singular value decomposition
#'
#' Prepare a matrix of rates, \code{x},
#' for a singular value decomposition,
#' typically via function \code{\link{scaled_svd}}.
#' The preparation consists of
#' valdity checking, replacing any
#' zeros with small modelled values,
#' and then taking logs.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version
#' of \code{x}.
#'
#' @seealso \code{\link{scaled_svd}},
#' \code{\link{prepare_svd_probs}}
#' @export
prepare_svd_rates <- function(x) {
    checkmate::assert_matrix(x,
                             min.rows = 1L,
                             min.cols = 1L)
    checkmate::assert_numeric(x,
                              lower = 0,
                              finite = TRUE,
                              any.missing = FALSE)
    x <- replace_zeros(x)
    log(x)
}


## HAS_TESTS
#' Prepare a matrix of probabilities for 
#' a singular value decomposition
#'
#' Prepare a matrix of probabilities or
#' proportions, \code{x},
#' for a singular value decomposition,
#' typically via function \code{\link{scaled_svd}}.
#' The preparation consists of
#' valdity checking, replacing any
#' zeros and ones with modelled values,
#' and then transforming to a logit scale.
#'
#' @param x A matrix of probabilities
#' or proportions.
#'
#' @return A modified version
#' of \code{x}.
#'
#' @seealso \code{\link{scaled_svd}},
#' \code{\link{prepare_svd_probs}}
#' @export
prepare_svd_probs <- function(x) {
    checkmate::assert_matrix(x,
                             min.rows = 1L,
                             min.cols = 1L)
    checkmate::assert_numeric(x,
                              lower = 0,
                              upper = 1,
                              any.missing = FALSE)
    x <- replace_zeros_ones(x)
    log(x / (1 - x))
}




scaled_svd <- function(x, n_component) {
    checkmate::assert_matrix(x,
                             min.rows = 1L,
                             min.cols = 1L)
    checkmate::assert_numeric(x,
                              finite = TRUE,
                              any.missing = FALSE)
    checkmate::assert_count(n_component,
                            positive = TRUE)
    svd <- svd(x = x,
               nu = n_component,
               nv = n_component)
    U <- svd$u
    s <- seq_len(n_component)
    D <- diag(svd$d[s])
    V <- svd$v
    mean_V <- colMeans(V)
    mean <- as.numeric(U %*% D %*% mean_V)
    sd_V <- apply(V, MARGIN = 2L, FUN = stats::sd)
    sd_V <- diag(sd_V)
    components <- U %*% D %*% sd_V
    list(mean = mean,
         components = components)
}
    

## HAS_TESTS
#' Replace zeros in a matrix 'x' of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace zeros in \code{x}, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no infinite values.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros <- function(x) {
    is_zero <- x == 0
    if (any(is_zero)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
    }
    x
}

## HAS_TESTS
#' Replace zeros and ones in a matrix 'x' of
#' estimated probabilities
#'
#' Based on a simple main effects model,
#' replace zeros and ones in \code{x},
#' a matrix of estimated probabilities.
#' The replacement values should typically be
#' near 0 or 1.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no values abouve one.
#'
#' @param x A matrix of probabilities.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros_ones <- function(x) {
    is_zero <- x == 0
    is_one <- x == 1
    if (any(is_zero) || any(is_one)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
        x[is_one] <- 0.5 + 0.5 * predicted[is_one]
    }
    x
}

    
    
