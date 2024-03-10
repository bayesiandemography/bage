
## HAS_TESTS
#' Create components needed by a scaled_svd object
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values.
#'
#' `scaled_svd_comp() typically proceeds as follows:
#' - transform values in matrix `x` (eg take logs)
#' - center each column of `x`
#' - carry out a SVD on the transformed version of `x`
#' - centre and scale the results from the SVD to produce
#' matrix `matrix` and vector `offset`.
#'
#' If 
#' - \eqn{X} is a the matrix of transformed values
#' of `x`
#' - \eqn{A} is the matrix called `matrix`,
#' - \eqn{b} is the vector called `offset`, and
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
#' One exception is when the SVD is used to capture
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
#' `scaled_svd_comp()` converts any `0`s in
#' `x` to values just above `0` before
#' applying the log or logit function.
#' When `scale` is `"logit"`,
#' `scaled_svd_comp() also converts any
#' `1`s to values just below `1`.
#'
#' Centering is appropriate when modelling
#' the shape of, for instance, age profiles,
#' rather than the absolute level of the profiles.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param n Number of components of the SVD to use.
#' Defaults to 10.
#' @param transform `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param center Whether to center the transformed
#' version of `x` before carrying out the SVD.
#'
#' @returns A named list with two elements:
#' - `matrix`, a numeric matrix
#' - `offset`, a numeric vector
#'
#' @seealso
#' - [poputils::to_matrix()] can create matrices to
#' supply to `scaled_svd_comp`
#' - <name of vignette> has mathematical details
#' - example of use of `scaled_svd_comp`:
#' href{https://github.com/bayesiandemography/svd_mortality}{svd_mortality}
#' 
#' @examples
#' x <- matrix(rgamma(n = 50, shape = 1), nrow = 5, ncol = 10)
#' x
#' scaled_svd_comp(x, n = 3)
#' scaled_svd_comp(x, n = 3, transform = "none")
#' @export
scaled_svd_comp <- function(x,
                            n = 10,
                            transform = c("log", "logit", "none"),
                            center = TRUE) {
  ## check 'n'
  check_n(n = n,
          nm_n = "n",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  n <- as.integer(n)
  ## check 'x'
  check_is_matrix(x, nm_x = "x")
  check_numeric(x, nm_x = "x")
  if (ncol(x) < n)
    stop(gettextf("number of columns in '%s' [%d] less than '%s' [%d]",
                  "x",
                  ncol(x),
                  "n",
                  n),
         call. = FALSE)
  ## check 'transform'
  transform <- match.arg(transform)
  if (transform %in% c("log", "logit")) {
    if (any(x < 0))
      stop(gettextf("'%s' is \"%s\" but '%s' has negative values",
                    "transform",
                    transform,
                    "x"),
           call. = FALSE)
  }
  if (transform  == "logit") {
    if (any(x > 1))
      stop(gettextf("'%s' is \"%s\" but '%s' has values greater than 1",
                    "transform",
                    transform,
                    "x"),
           call. = FALSE)
  }
  ## check 'center'
  check_flag(center)
  ## transform to log or logit scale if necessary
  if (transform == "log") {
    x <- replace_zeros(x)
    x <- log(x)
  }
  if (transform == "logit") {
    x <- replace_zeros_ones(x)
    x <- log(1 / (1 - x))
  }
  ## center, if required
  if (center) {
    col_means <- colMeans(x)
    col_means <- rep(col_means, each = nrow(x))
    x <- x - col_means
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
  matrix <- U %*% D %*% S
  offset <- as.numeric(U %*% D %*% m)
  ## add names
  dn <- dimnames(x)
  if (!is.null(dn[[1L]])) {
    dimnames(matrix) <- c(dn[1L], list(component = seq_len(n)))
    names(offset) <- dn[[1L]]
  }
  ## convert matrix to sparse matrix
  matrix <- Matrix::sparseMatrix(i = row(matrix),
                                 j = col(matrix),
                                 x = as.double(matrix),
                                 dimnames = dimnames(matrix))
  ## return
  list(matrix = matrix,
       offset = offset)
}


## Helper functions -----------------------------------------------------------

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

