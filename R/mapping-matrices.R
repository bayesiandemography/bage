
## HAS_TESTS
#' Generate Labels for the SVD Dimension of an SVD Prior
#'
#' @param prior Object of class 'bag_prior'
#' @param along Name of 'along' dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_sexgender Name of time dimension, or NULL
#'
#' @returns A character vector
#'
#' @noRd
get_labels_svd <- function(prior, dimnames_term, var_sexgender) {
  n_comp <- prior$specific$n_comp
  indep <- prior$specific$indep
  nm_split <- dimnames_to_nm_split(dimnames_term)
  has_sexgender <- !is.null(var_sexgender)
  if (has_sexgender) {
    term_has_sexgender <- var_sexgender %in% nm_split
    include_sexgender <- indep && term_has_sexgender
  }
  else
    include_sexgender <- FALSE
  ans <- paste0("comp", seq_len(n_comp))
  if (include_sexgender) {
    levels_sex <- dimnames_term[[var_sexgender]]
    ans <- paste(rep(levels_sex, each = n_comp),
                 ans,
                 sep = ".")
  }
  ans
}


## HAS_TESTS
#' Find the Index of the Along Dimension,
#' Throwing an Error If Cannot be Found
#'
#' If 'along' is non-NULL, look for a dimension
#' with that name. Otherwise, use the dimension
#' identified by 'var_time', or by 'var_age'.
#'
#' @param along Name of the 'along' dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of the time dimension, or NULL
#' @param var_age Name of the age dimension, or NULL
#'
#' @returns An integer
#'
#' @noRd
make_i_along <- function(along, dimnames_term, var_time, var_age) {
  nms <- names(dimnames_term)
  nm_term <- paste(nms, collapse = ":")
  if (is.null(along)) {
    has_time <- !is.null(var_time)
    if (has_time) {
      i_along <- match(var_time, nms, nomatch = 0L)
      if (i_along > 0L)
        return(i_along)
    }
    has_age <- !is.null(var_age)
    if (has_age) {
      i_along <- match(var_age, nms, nomatch = 0L)
      if (i_along > 0L)
        return(i_along)
    }
    msg <- "Prior for term {.var {nm_term}} does not have a value for {.arg along}."
    if (!has_time)
      msg <- c(msg,
               i = "No time variable identified. Use function {.fun set_var_time} to identify?")
    if (!has_age)
      msg <- c(msg,
               i = "No age variable identified. Use function {.fun set_var_age} to identify?")
    msg <- c(msg,
             i = "Choices for {.arg along}: {.val {nms}}.")
    cli::cli_abort(msg)
  }
  else {
    if (length(along) != 1L)
      cli::cli_abort("Internal error: {.arg along} does not have length 1.")
    i_along <- match(along, nms, nomatch = 0L)
    if (i_along > 0L)
      return(i_along)
    cli::cli_abort(c("Prior for {.var {nm_term}} has invalid value for {.arg along}.",
                     i = "Value supplied: {.val {along}}.",
                     i = "Valid choices: {.val {nms}}."))
  }
}


## HAS_TESTS
#' Find the Index of the Time Dimension, Throwing an Error if
#' No Time Dimension is Present
#'
#' @param var_time Name of time dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#'
#' @returns An integer
#'
#' @noRd
make_i_time <- function(var_time, dimnames_term) {
  nms <- names(dimnames_term)
  nm_term <- paste(nms, collapse = ":")
  if (is.null(var_time))
    cli::cli_abort("Internal error: {.arg var_time} is NULL.")
  ans <- match(var_time, nms, nomatch = 0L)
  if (ans == 0L)
    cli::cli_abort("Internal error: Term {.var {nm_term}} does not include time variable.")
  ans
}


## HAS_TESTS
#' Make Matrix Giving Mapping between Position Along Age
#' or Age-Sex Dimension and Position in Matrix
#'
#' Make matrix giving mapping between (i) row and column
#' within a matrix formed by bringing age, or age and sex, dimensions
#' to the front, and (ii) the (zero-base) offset within
#' within the original effect term.
#'
#' Note that matrix (i) is what we get when we multiply
#' the effectfree matrix by the SVD matrix where columns
#' are SVD components and rows are age or age-sex.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_agesex <- function(dimnames_term, var_age, var_sexgender) {
  nms <- names(dimnames_term)
  nm_term <- paste(nms, collapse = ":")
  has_age <- !is.null(var_age)
  has_var_sexgender <- !is.null(var_sexgender)
  if (!has_age)
    cli::cli_abort("Internal error: {.arg var_age} not specified.")
  i_along <- match(var_age, nms, nomatch = 0L)
  if (i_along == 0L)
    cli::cli_abort("Internal error: Term {.var {nm_term}} does not have an age dimension.")
  if (has_var_sexgender) {
    i_sex <- match(var_sexgender, nms, nomatch = 0L)
    if (i_sex > 0L)
      i_along <- c(i_along, i_sex)
  }
  i_along <- sort(i_along)
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}

## HAS_TESTS
#' Make 'matrix_along_by' for all Parameters in One Term
#'
#' @param along Name of 'along' dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_effect <- function(along, dimnames_term, var_time, var_age) {
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}
  

## HAS_TESTS
#' Make 'matrix_along_by' for Free Parameters for Term
#' with SVD Prior
#'
#' The svd dimension representing age or age-sex is
#' assumed to come first. The term may or may not
#' include a time dimension If it does, then
#' that is the 'along' dimension; if it does not, then the
#' svd dimension is treated as the along dimension
#' (for the purposes of standardization, not for
#' calculating the log-posterior.)
#'
#' @param prior Object of class 'bage_mod'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param var_sexgender Name of sex/gender dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_effectfree_svd <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  labels_svd <- get_labels_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender)
  non_agesex <- setdiff(names(dimnames_term), c(var_age, var_sexgender))
  dimnames_nonagesex <- dimnames_term[non_agesex]
  nms_nonagesex <- names(dimnames_nonagesex)
  dimnames_new <- c(list(.svd = labels_svd), dimnames_nonagesex)
  i_along <- 1L
  has_var_time <- !is.null(var_time)
  if (has_var_time) {
    i_time <- match(var_time, nms_nonagesex, nomatch = 0L)
    i_along <- i_along + i_time
  }
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_new)
}

## HAS_TESTS
#' Basic Function for Making 'matrix_along_by'
#'
#' Unlike 'along', 'i_along' can have length > 1.
#' 
#' @param i_along Index for the 'along' dimension
#' @param dimnames_terms Named list of character vectors
#' giving categories within each dimension of term
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_inner <- function(i_along, dimnames_term) {
  dim <- lengths(dimnames_term)
  nms <- names(dimnames_term)
  n_dim <- length(dim)
  s_dim <- seq_len(n_dim)
  length_effect <- prod(dim)
  i <- seq.int(from = 0L, length.out = length_effect)
  a <- array(i, dim = dim)
  perm <- c(i_along, s_dim[-i_along])
  ans <- aperm(a, perm = perm)
  ans <- matrix(ans, nrow = prod(dim[i_along]))
  rownames <- expand.grid(dimnames_term[i_along])
  rownames <- Reduce(paste_dot, rownames)
  rownames(ans) <- rownames
  names(dimnames(ans))[[1L]] <- paste(nms[i_along], collapse = ":")
  if (n_dim > 1L) {
    colnames <- expand.grid(dimnames_term[-i_along])
    colnames <- Reduce(paste_dot, colnames)
    colnames(ans) <- colnames
    names(dimnames(ans))[[2L]] <- paste(nms[-i_along], collapse = ":")
  }
  else {
    colnames(ans) <- NULL
    names(dimnames(ans))[[2L]] <- ""
  }
  ans
}

## HAS_TESTS
#' Transform SVD Values Back to Original Scale
#'
#' @param svd Draws for SVD Coefficients. A matrix
#' or an rvec.
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array representation of term
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A matrix or an rvec
#'
#' @noRd
svd_to_effect <- function(svd,
                          prior,
                          dimnames_term,
                          var_age,
                          var_sexgender) {
  is_svd_rvec <- rvec::is_rvec(svd)
  if (is_svd_rvec)
    svd <- as.matrix(svd)
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  levels_age <- dimnames_term[[var_age]]
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  has_sexgender <- !is.null(var_sexgender)
  if (has_sexgender) {
    levels_sexgender <- dimnames_term[[var_sexgender]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else {
    levels_sexgender <- NULL
    joint <- NULL
  }
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  levels_effect <- dimnames_to_levels(dimnames_term)
  n_by <- ncol(matrix_agesex) ## special meaning of 'by': excludes age and sex
  m <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  b <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  I <- Matrix::.sparseDiagonal(n_by)
  ones <- Matrix::sparseMatrix(i = seq_len(n_by),
                               j = rep.int(1L, times = n_by),
                               x = rep.int(1L, times = n_by))
  agesex_to_standard <- make_index_matrix(matrix_agesex)
  m_all_by <- Matrix::kronecker(I, m)
  b_all_by <- Matrix::kronecker(ones, b)
  b_all_by <- Matrix::drop(b_all_by)
  ans <- m_all_by %*% svd + b_all_by
  ans <- agesex_to_standard %*% ans
  ans <- Matrix::as.matrix(ans)
  if (is_svd_rvec)
    ans <- rvec::rvec_dbl(ans)
  else {
    levels_effect <- dimnames_to_levels(dimnames_term)
    dimnames(ans) <- list(levels_effect, NULL)
  }
  ans
}
