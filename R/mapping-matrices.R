
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
#' Convert 'matrix_agesex' or 'matrix_along_by'
#' to Sparse Index Matrix
#'
#' @param m Matrix produced by
#' 'make_matrix_agesex' or 'make_matrix_along_by'
#'
#' @returns A sparse matrix consisting of
#' 1s and 0s
#'
#' @noRd
make_index_matrix <- function(m) {
  n <- length(m)
  i <- as.integer(m) + 1L
  j <- seq_len(n)
  x <- rep.int(1L, times = n)
  ans <- Matrix::sparseMatrix(i = i,
                              j = j,
                              x = x)
  rn_old <- rownames(m)
  cn_old <- colnames(m)
  if (is.null(cn_old))
    cn_new <- rep(rn_old, times = ncol(m))
  else
    cn_new <- paste(rn_old, rep(cn_old, each = length(rn_old)), sep = ".")
  rn_new <- cn_new[match(seq_len(n), m + 1L)]
  dimnames(ans) <- list(rn_new, cn_new)
  ans
}


## HAS_TESTS
#' Make a Matrix that Adds a 'Column' to a
#' Vector that Can Be Represented as a Matrix
#'
#' The rowsums of the new matrix are all 0.
#' The vector in nr*nc-space is projected
#' into nr*(nr+1)-space.
#'
#' Note that this matrix does not deal
#' with along-by dimensions. See
#' 'make_matrix_append' for that.
#'
#' @param nr,nc Dimensions of matrix before
#' expansion
#'
#' @returns A sparse matrix with dimensions
#' (nr * (nc + 1)) x (nr * nc)
#'
#' @noRd
make_matrix_add_columns <- function(nr, nc) {
  m_transpose_pre <- make_matrix_transpose(nr = nr, nc = nc)
  m_add_element_one <- make_matrix_add_element(nc)
  I <- diag(nr)
  m_add_element <- kronecker(I, m_add_element_one)
  m_transpose_post <- make_matrix_transpose(nr = nc + 1L, nc = nr)
  ans <- m_transpose_post %*% m_add_element %*% m_transpose_pre
  ans <- Matrix::Matrix(ans)
  ans
}


#' @noRd
make_matrix_add_columns <- function(along, dimnames_term, var_time, var_age) {
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  is_last_element_by <- dimnames_term
  for (i_dim in seq_along(dimnames_term)) {
    is_last_element[[i_dim]][] <- FALSE
    if (i_dim != i_along)
      is_last_element[[i_dim]][[length(is_last_element[[i_dim]]]]] <- TRUE
  }
  length_dim <- length(dimnames_term[[i_dim]])
      is_last_element_by <- 
      
  is_last_element <- 
  
  m_transpose_pre <- make_matrix_transpose(nr = nr, nc = nc)
  m_add_element_one <- make_matrix_add_element(nc)
  I <- diag(nr)
  m_add_element <- kronecker(I, m_add_element_one)
  m_transpose_post <- make_matrix_transpose(nr = nc + 1L, nc = nr)
  ans <- m_transpose_post %*% m_add_element %*% m_transpose_pre
  ans <- Matrix::Matrix(ans)
  ans
}



## HAS_TESTS
#' Make a Matrix that Adds an Element to a Vector
#'
#' The tcrossprod of the resulting matrix have elements
#' n / (n+1) on the diagnoal, and -1/(n+1) on the off-diagonal.
#'
#' @param n Number of elements of the vector *before*
#' adding an extra one
#'
#' @returns A (n+1) x n matrix
#'
#' @noRd
make_matrix_add_element <- function(n) {
  m <- matrix(1, nrow = n + 1L)
  qr <- qr(m)
  ans <- qr.Q(qr, complete = TRUE)
  ans <- ans[, -1L, drop = FALSE]
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
#' Create along-by Matrix for a Single Dimension
#' of a Main Effect or Interaction
#'
#' Create a matrix where entry (i,j) is
#' the position in an array (using
#' zero-based indexing) of 'along'
#' value i and 'by' value j.
#'
#' @param i_along Index of the along dimension(s)
#' @param dim Dimensions of the array
#'
#' @returns A matrix of integers.
#'
#' @noRd
make_matrix_along_by <- function(i_along, dim, dimnames) {
  n_dim <- length(dim)
  i <- seq.int(from = 0L, length.out = prod(dim))
  a <- array(i, dim = dim)
  s <- seq_along(dim)
  perm <- c(i_along, s[-i_along])
  ans <- aperm(a, perm = perm)
  ans <- matrix(ans, nrow = prod(dim[i_along]))
  rownames <- expand.grid(dimnames[i_along])
  rownames <- Reduce(paste_dot, rownames)
  rownames(ans) <- rownames
  names(dimnames(ans))[[1L]] <- paste(names(dimnames)[i_along], collapse = ".")
  if (length(dim) > 1L) {
    colnames <- expand.grid(dimnames[-i_along])
    colnames <- Reduce(paste_dot, colnames)
    colnames(ans) <- colnames
    names(dimnames(ans))[[2L]] <- paste(names(dimnames)[-i_along], collapse = ".")
  }
  ans
}


## HAS_TESTS
#' Make 'matrix_along_by' for Free Parameters for Term
#' that Drops First Element of Along
#'
#' @param along Name of "along" dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_drop_first_along <- function(along,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age) {
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  dimnames_term[[i_along]] <- dimnames_term[[i_along]][-1L]
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
#' Make Mapping Matrix for Spline Components
#'
#' Spline components consist of
#' free parameters, plus a zero at the start
#' of each subseries
#'
#' @param prior Object of class 'bage_prior_spline'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param drop_first_along Whether to drop first value
#' of 'along' (as required to create effectfree)
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_spline <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        drop_first_along) {
  along <- prior$specific$along
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  s <- seq_len(n_comp)
  if (drop_first_along)
    s <- s[-1L]
  labels_along <- paste0("comp", s)
  dimnames_term[[i_along]] <- labels_along
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
#' @param drop_first_along Whether to omit the first
#' element of the 'along' dimension.
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_svd <- function(prior,
                                     dimnames_term,
                                     var_time,
                                     var_age,
                                     var_sexgender,
                                     drop_first_along) {
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
    if (drop_first_along)
      dimnames_new[[i_along]] <- dimnames_new[[i_along]][-1L]
  }
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_new)
}


## HAS_TESTS
#' Make Matrix that Rearranges Vector so Along Dimension First
#'
#' @param matrix_along_by Mapping matrix for vector
#'
#' @return A sparse matrix
#'
#' @noRd
make_matrix_along_first <- function(matrix_along_by) {
  n <- length(matrix_along_by)
  i <- seq_len(n)
  j <- as.integer(matrix_along_by) + 1L
  x <- rep.int(1L, times = n)
  Matrix::sparseMatrix(i = i,
                       j = j,
                       x = x)
}  


## HAS_TESTS
#' Make a Matrix that Appends a Zero at the Start
#' of Each 'along' Series
#'
#' @param along Name of "along" dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_append_zero <- function(matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  m_append_zero_one <- rbind(0, Matrix::.sparseDiagonal(n_along))
  I <- Matrix::.sparseDiagonal(n_by)
  ans <- Matrix::kronecker(I, m_append_zero_one)
  is_along_first <- all(matrix_along_by[, 1L] == seq.int(from = 0L, length.out = n_along))
  if (!is_along_first) {
    m_standard_to_alongfirst <- make_matrix_along_first(matrix_along_by)
    matrix_along_by_new <- rbind(matrix_along_by,
                                 matrix_along_by[n_along, ] + n_by)
    m_alongfirst_to_standard <- make_index_matrix(matrix_along_by_new)
    ans <- m_alongfirst_to_standard %*% ans %*% m_standard_to_alongfirst
  }
  ans
}



## HAS_TESTS
#' Make a Matrix that Appends an Extra 'along'
#' Series and/or Appends a Zero at the
#' Start of Each 'along' Series
#'
#' @param matrix_along_by Mapping matrix for the original matrix
#' @param append_column Whether to append an extra
#' 'along' series
#' @param append_zero Whether to append zeros
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_append <- function(matrix_along_by, append_column, append_zero) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  is_along_first <- all(matrix_along_by[, 1L] == seq.int(from = 0L, length.out = n_along))
  ans <- Matrix::.sparseDiagonal(n_along * n_by)
  ## assume that 'along' is first dimension
  if (append_column) {
    m <- make_matrix_add_column(nr = n_along, nc = n_by)
    ans <- m %*% ans
    n_by <- n_by + 1L
    if (!is_along_first) {
      s <- seq.int(from = 0L, length.out = n_along * n_by)
      matrix_along_by_new <- matrix(s, nrow = n_along, byrow = TRUE)
    }
  }
  if (append_zero) {
    m_append_zero_one <- rbind(0, Matrix::.sparseDiagonal(n_along))
    I <- Matrix::.sparseDiagonal(n_by)
    m <- Matrix::kronecker(I, m_append_zero_one)
    ans <- m %*% ans
    n_along <- n_along + 1L
    if (!is_along_first) {
      s <- seq.int(from = 0L, length.out = n_along * n_by)
      matrix_along_by_new <- matrix(s, nrow = n_along, byrow = TRUE)
    }
  }
  if (!is_along_first) {
    m_to_alongfirst <- make_matrix_along_first(matrix_along_by)
    m_from_alongfirst <- make_index_matrix(matrix_along_by_new)
    ans <- m_from_alongfirst %*% ans %*% m_to_alongfirst
  }
  ans
}


## HAS_TESTS
#' Make Matrix to Expand Age or Age-Sex Dimension Modelled with SVD,
#' and Return to Original Order of Dimensions
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_expand_svd <- function(prior,
                                   dimnames_term,
                                   var_time,
                                   var_age,
                                   var_sexgender) {
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  nm <- dimnames_to_nm(dimnames_term)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  has_age <- !is.null(var_age)
  has_sexgender <- !is.null(var_sexgender)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  n_by <- ncol(matrix_agesex) ## special meaning of 'by': excludes age and sex
  levels_age <- if (is.null(var_age)) NULL else dimnames_term[[var_age]]
  levels_sexgender <- if (is.null(var_sexgender)) NULL else dimnames_term[[var_sexgender]]
  levels_effect <- dimnames_to_levels(dimnames_term)
  if (has_sexgender) {
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else
    joint <- NULL
  m <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  I <- Matrix::.sparseDiagonal(n_by)
  m_all_by <- Matrix::kronecker(I, m)
  agesex_to_standard <- make_index_matrix(matrix_agesex)
  agesex_to_standard %*% m_all_by
}


## HAS_TESTS
#' Make Matrix from Spline (With or Without First Along) to Effect
#'
#' @param prior Object of class 'bage_prior_spline'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param drop_first_along Whether first value for each 'along' has
#' been dropped from spline values
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_spline_effect <- function(prior,
                                      dimnames_term,
                                      var_time,
                                      var_age,
                                      drop_first_along) {
  along <- prior$specific$along
  m_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                   dimnames_term = dimnames_term,
                                                   var_time = var_time,
                                                   var_age = var_age)
  n_along <- nrow(m_along_by_effect)
  n_by <- ncol(m_along_by_effect)
  n_comp <- get_n_comp_spline(prior = prior, n_along = n_along)
  X <- make_spline_matrix(n_comp = n_comp, n_along = n_along)
  if (drop_first_along)
    X <- X[, -1L]
  I <- Matrix::.sparseDiagonal(n_by)
  ans <- Matrix::kronecker(I, X)
  is_along_first <- all(m_along_by_effect[, 1L] == seq.int(from = 0L, length.out = n_along))
  if (!is_along_first) {
    m_along_by_spline <- make_matrix_along_by_spline(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age,
                                                     drop_first_along = drop_first_along)
    m_standard_to_alongfirst <- make_matrix_along_first(m_along_by_spline)
    m_alongfirst_to_standard <- make_index_matrix(m_along_by_effect)
    ans <- m_alongfirst_to_standard %*% ans %*% m_standard_to_alongfirst
  }
  ans
}


## HAS_TESTS
#' Make Matrix from Dynamic SVD (With or Without First Along) to Effect
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param drop_first_along Whether first value for each 'along' (time) has
#' been dropped from svd values
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_svddynamic_effect <- function(prior,
                                          dimnames_term,
                                          var_time,
                                          var_age,
                                          var_sexgender,
                                          drop_first_along) {
  ## expand svd dimension
  dimnames_agesex <- dimnames_term
  if (drop_first_along)
    dimnames_agesex[[var_time]] <- dimnames_agesex[[var_time]][-1L]
  ans <- make_matrix_expand_svd(prior = prior,
                                dimnames_term = dimnames_agesex,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender)
  if (drop_first_along) {
    ## add extra time period
    along <- prior$specific$along
    i_along <- make_i_along(along = along,
                            dimnames_term = dimnames_agesex,
                            var_time = var_time,
                            var_age = var_age)
    dim_agesex <- lengths(dimnames_agesex)
    m_along_by <- make_matrix_along_by(i_along = i_along,
                                       dim = dim_agesex,
                                       dimnames = dimnames_agesex)
    m_append_zero <- make_matrix_append(m_along_by,
                                        append_column = FALSE,
                                        append_zero = TRUE)
    ans <- m_append_zero %*% ans
  }
  dimnames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make a Matrix that Transposes a Vector that Can Be
#' Represented as a Matrix
#'
#' @param nr,nc Dimensions of matrix before tranposing
#'
#' @returns An (nr*nc) x (nr*nc) matrix
#'
#' @noRd
make_matrix_transpose <- function(nr, nc) {
  n <- nr * nc
  ans <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    j <- ((i - 1L) %/% nc) + ((i - 1L) %% nc) * nr + 1L
    ans[i, j] <- 1
  }
  ans
}


## HAS_TESTS
#' Make Offset used in converting effectfree to effect for SVD Priors
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A vector.
#'
#' @noRd
make_offset_effectfree_effect_svd <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender) {
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  nm <- dimnames_to_nm(dimnames_term)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  has_age <- !is.null(var_age)
  has_sexgender <- !is.null(var_sexgender)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  n_by <- ncol(matrix_agesex)  ## special meaning of 'n_by': excludes age and sex
  levels_age <- if (has_age) dimnames_term[[var_age]] else NULL
  levels_sexgender <- if (has_sexgender) dimnames_term[[var_sexgender]] else NULL
  levels_effect <- dimnames_to_levels(dimnames_term)
  if (has_sexgender) {
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else
    joint <- NULL
  b <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  ones <- Matrix::sparseMatrix(i = seq_len(n_by),
                               j = rep.int(1L, times = n_by),
                               x = rep.int(1L, times = n_by))
  b_all_by <- Matrix::kronecker(ones, b)
  agesex_to_standard <- make_index_matrix(matrix_agesex)
  ans <- agesex_to_standard %*% b_all_by
  ans <- Matrix::drop(ans)
  names(ans) <- levels_effect
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
