
## HAS_TESTS
#' Replace zeros in a matrix of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace zeros in `x`, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that `x` is a valid numeric
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

    
## HAS_TESTS    
#' Given a matrix of rates, calculate
#' transform using SVD
#'
#' Assume 'x' is has been checked and is valid
#'
#' @param x Matrix of rates,
#' probabilities, or means
#' @param n Number of components
#'
#' @returns A named list with two
#' elements: matrix called 'transform',
#' and a vector called 'translate'.
#'
#' @noRd
scaled_svd <- function(x, n) {
    svd <- svd(x = x,
               nu = n,
               nv = n)
    U <- svd$u
    D <- diag(svd$d[seq_len(n)])
    V <- svd$v
    m <- colMeans(V)
    S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
    transform <- U %*% D %*% S
    translate <- as.numeric(U %*% D %*% m)
    dn <- dimnames(x)
    if (!is.null(dn[[1L]])) {
        dimnames(transform) <- c(dn[1L], list(component = seq_len(n)))
        names(translate) <- dn[[1L]]
    }
    list(transform = transform,
         translate = translate)
}



#' Construct parsimonious representations of rates based on SVD
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values.
#'
#' `svd_transform() typically proceeds as follows:
#' - transform values in matrix `x` (eg take logs)
#' - carry out a SVD on the transformed version of `x`
#' - centre and scale the results from the SVD to produce
#' matrix `transform` and vector `translate`.
#'
#' If 
#' - \eqn{X} is a the matrix of transformed values
#' of `x`
#' - \eqn{A} is the matrix called `transform`,
#' - \eqn{b} is the vector called `translate`, and
#' - \eqn{\beta} is a vector of standard normal variates,
#'
#' and
#'
#' \deqn{v = A \beta + b}
#'
#' then \eqn{v} should look like a randomly-selected
#' column from \eqn{X}.
#'
#' Matrix `x` typically has age along the rows,
#' and some combination of classification variables,
#' such as country and time, along the columns.
#' One exception is when the SVD is to capture
#' the relationship between female and male rates,
#' in which case rows are formed by interacting
#' sex and age. See below for an example.
#'
#' The number of components used by the SVD
#' (and hence the number of columns of the
#' `tranform` matrix) is governed by
#' argument `n`. The default is 5.
#'
#' When `scale` is `"log"` or `"logit"`,
#' `svd_transform()` converts any `0`s in
#' `x` to values just above `0` before
#' applying the log or logit function.
#' When `scale` is `"logit"`,
#' `svd_transform() also converts any
#' `1`s to values just below `1`.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param n Number of components of the SVD to use.
#' Defaults to 5.
#' @param scale `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#'
#' @returns A named list with two elements:
#' - `transform`, a numeric matrix
#' - `translate`, a numeric vector
#'
#' @seealso
#' - [to_matrix()] can create matrices to
#' supply to `svd_transform`
#' - <name of vignette> has mathematical details
#' - example of use of `svd_transform`:
#' href{https://github.com/bayesiandemography/svd_mortality}{svd_mortality}
#' 
#' @examples
#' x <- matrix(rgamma(n = 50), nrow = 5, ncol = 10)
#' x
#' svd_transform(x, n = 3)
#' svd_transform(x, n = 3, scale = "none") 
#' @export
svd_transform <- function(x,
                          n = 5,
                          scale = c("log", "logit", "none")) {
    ## check 'n'
    n <- checkmate::assert_count(n,
                                 positive = TRUE,
                                 coerce = TRUE)
    ## check 'x'
    checkmate::assert_matrix(x,
                             mode = "numeric",
                             any.missing = FALSE,
                             min.rows = 1L,
                             min.cols = 1L)
    if (ncol(x) < n)
        stop(gettextf("number of columns in '%s' [%d] less than '%s' [%d]",
                      "x",
                      ncol(x),
                      "n",
                      n),
             call. = FALSE)
    ## check 'scale'
    scale <- match.arg(scale)
    if (scale %in% c("log", "logit")) {
        if (any(x < 0))
            stop(gettextf("'%s' is \"%s\" but '%s' has negative values",
                          "scale",
                          scale,
                          "x"),
                 call. = FALSE)
    }
    if (scale  == "logit") {
        if (any(x > 1))
            stop(gettextf("'%s' is \"%s\" but '%s' has values greater than 1",
                          "scale",
                          scale,
                          "x"),
                 call. = FALSE)
    }
    ## transform to log or logit scale if necessary
    if (scale == "log") {
        x <- replace_zeros(x)
        x <- log(x)
    }
    if (scale == "logit") {
        x <- replace_zeros_ones(x)
        x <- log(1 / (1 - x))
    }
    ## apply svd
    svd <- svd(x = x,
               nu = n,
               nv = n)
    U <- svd$u
    D <- diag(svd$d[seq_len(n)])
    V <- svd$v
    ## standardise
    m <- colMeans(V)
    S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
    transform <- U %*% D %*% S
    translate <- as.numeric(U %*% D %*% m)
    ## add names
    dn <- dimnames(x)
    if (!is.null(dn[[1L]])) {
        dimnames(transform) <- c(dn[1L], list(component = seq_len(n)))
        names(translate) <- dn[[1L]]
    }
    ## return
    list(transform = transform,
         translate = translate)
}
