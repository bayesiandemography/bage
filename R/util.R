
## HAS_TESTS
#' Insert a Into a Data Frame
#'
#' Insert a variable into a dataframe, immediately
#' after another variable.
#'
#' Currently assumes that not inserting
#' at start of 'df'.
#'
#' @param df A data frame (including a tibble)
#' @param nm_after Name of the variable that the
#' 'x' should come after
#' @param x New variable
#' @param nm_x Name of new variable
#'
#' @returns A modified version of 'df'
#'
#' @noRd
insert_after <- function(df, nm_after, x, nm_x) {
  nms_df <- names(df)
  n_df <- length(nms_df)
  i_after <- match(nm_after, names(df))
  if (i_after < n_df) {
    s_before <- seq_len(i_after)
    s_after <- seq.int(from = i_after + 1L, to = n_df)
    ans <- vctrs::vec_cbind(df[s_before],
                            x,
                            df[s_after],
                            .name_repair = "universal_quiet")
  }
  else {
    ans <- vctrs::vec_cbind(df,
                            x,
                            .name_repair = "universal_quiet")
  }
  names(ans)[[i_after + 1L]] <- nm_x
  ans
}


## HAS_TESTS
#' Check Whether Currently in Test or Snapshot
#'
#' Based on testthat::is_testing() and testthat::is_snapshot()
#' 
#' @returns TRUE or FALSE
#'
#' @noRd
is_not_testing_or_snapshot <- function() {
  is_testing <- identical(Sys.getenv("TESTTHAT"), "true")
  is_snapshot <- identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")
  !is_testing && !is_snapshot
}


## HAS_TESTS
#' Test Whether Two Objects Have the Same Class
#'
#' @param x,y Objects
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_class <- function(x, y)
  identical(class(x)[[1L]], class(y)[[1L]])


## HAS_TESTS
#' Use a precision matrix to construct scaled eigen vectors
#'
#' The scaled eigen vectors can be used to draw from a
#' multivariate normal distribution with the given
#' pecision matrix.
#'
#' @param prec A symmetric positive definite matrix.
#'
#' @returns A matrix with the same dimensions as 'prec'
#'
#' @noRd
make_scaled_eigen <- function(prec) {
    tolerance <- 1e-6
    eigen <- eigen(prec, symmetric = TRUE)
    vals <- eigen$values
    vecs <- eigen$vectors
    min_valid_val <- -tolerance * abs(vals[[1L]]) ## based on MASS::mvrnorm
    if (any(vals < min_valid_val))
        cli::cli_abort("Estimated precision matrix not positive definite.")  ## nocov
    vals <- pmax(vals, abs(min_valid_val))
    sqrt_inv_vals <- sqrt(1 / vals)
    vecs %*% diag(sqrt_inv_vals)
}


#' Paste Two Vectors Separated by a Dot
#'
#' @param x A vector
#' @param y A vector
#'
#' @returns A character vector
#'
#' @noRd
paste_dot <- function(x, y) paste(x, y, sep = ".")


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from a Cholesky decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param R_prec Cholesky decomposition of precision matrix
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_chol <- function(n, mean, R_prec) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + backsolve(R_prec, Z)
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from an eigen decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param scaled_eigen Matrix of scaled eigenvalues
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_eigen <- function(n, mean, scaled_eigen) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + scaled_eigen %*% Z
}


## HAS_TESTS
#' Version of 'rpois_rvec' With Upper Limit on Lambda
#'
#' Coercion from integer to real within 'rpois' can create
#' numerical problems and valgrind errors, so switch
#' to just setting random variate to lambda, above a
#' given threshold. Warn the user that this is happening.
#'
#' @param n Number of variates (where each variate contains 'n_draw' values)
#' @param lambda Expected values. An rvec.
#'
#' @returns An rvec
#'
#' @noRd
rpois_rvec_guarded <- function(n, lambda) {
  threshold <- 1e8
  lambda <- as.matrix(lambda)
  is_gt <- !is.na(lambda) & (lambda > threshold)
  n_gt <- sum(is_gt)
  if (n_gt > 0L) {
    pc <- 100 * mean(is_gt)
    pc <- signif(pc, digits = 2)
    cli::cli_warn(c("Large values for {.arg lambda} used to generate Poisson variates.",
                    i = "{.val {pc}} percent of values for {.arg lambda} are above {.val {threshold}}.",
                    i = "Using deterministic approximation to generate variates for these values."))
  }
  ans <- lambda
  is_lt <- !is_gt
  ans[is_lt] <- stats::rpois(n = sum(is_lt), lambda = lambda[is_lt])
  ans <- rvec::rvec_dbl(ans)
  ans
}


## HAS_TESTS
#' Convert Rvec Columns to Numeric Columns by Taking Means
#'
#' @param data A data frame
#'
#' @return A data frame
#'
#' @noRd
rvec_to_mean <- function(data) {
  is_rvec <- vapply(data, rvec::is_rvec, TRUE)
  data[is_rvec] <- lapply(data[is_rvec], rvec::draws_mean)
  data
}
