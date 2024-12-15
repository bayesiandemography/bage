
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
make_dim_svd <- function(prior,
                         dimnames_term,
                         var_age,
                         var_sexgender) {
  labels_svd <- get_labels_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  i_age <- match(var_age, nm_split)
  if (is.null(var_sexgender))
      i_sexgender <- 0L
  else
    i_sexgender <- match(var_sexgender, nm_split, nomatch = 0L)
  i_agesex <- c(i_age, i_sexgender)
  dim <- lengths(dimnames_term)
  dim_noagesex <- dim[-i_agesex]
  c(length(labels_svd), dim_noagesex)
}


## HAS_TESTS
#' Calculate 'i_along' for a Prior that Specifically Requires Age or Time
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param vartime "age" or "time"
#'
#' @returns An integer
#'
#' @noRd
make_i_along_agetime <- function(prior, dimnames_term, var_age, var_time, agetime) {
  nm_split <- dimnames_to_nm_split(dimnames_term)
  nm_term <- dimnames_to_nm(dimnames_term)
  str_nm_prior <- str_nm_prior(prior)
  nm_var <- paste0("var_", agetime)
  nm_fun <- paste0("set_var_", agetime)
  AgeTime <- agetime
  substr(AgeTime, 1L, 1L) <- toupper(substr(AgeTime, 1L, 1L))
  var <- get(nm_var)
  has_var <- !is.null(var)
  if (!has_var)
    cli::cli_abort(c(paste("Using {.var {str_nm_prior}} prior when {agetime}",
                           "variable not identified."),
                     i = "Use function {.fun {nm_fun}} to identify {agetime} variable?"))
  i_along <- match(var, nm_split, nomatch = 0L)
  if (identical(i_along, 0L))
    cli::cli_abort(c(paste("Using {.var {str_nm_prior}} prior with a",
                           "term that does not involve {agetime}."),
                     i = "Term: {.var {nm_term}}.",
                     i = "{AgeTime} variable: {.var {var}}."))
  i_along
}



## HAS_TESTS
#' Find the Index of the Along Dimension,
#' Throwing an Error If Cannot be Found
#'
#' Helper function for 'make_i_along'
#'
#' @param Name of 'along' dimension, or NULL
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of the time dimension, or NULL
#' @param var_age Name of the age dimension, or NULL
#'
#' @returns An integer
#'
#' @noRd
make_i_along_inner <- function(along, dimnames_term, var_time, var_age) {
  nm_split <- dimnames_to_nm_split(dimnames_term)
  nm_term <- dimnames_to_nm(dimnames_term)
  if (is.null(along)) {
    has_time <- !is.null(var_time)
    if (has_time) {
      i_along <- match(var_time, nm_split, nomatch = 0L)
      if (i_along > 0L)
        return(i_along)
    }
    has_age <- !is.null(var_age)
    if (has_age) {
      i_along <- match(var_age, nm_split, nomatch = 0L)
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
             i = "Choices for {.arg along}: {.val {nm_split}}.")
    cli::cli_abort(msg)
  }
  else {
    if (length(along) != 1L)
      cli::cli_abort("Internal error: {.arg along} does not have length 1.")
    i_along <- match(along, nm_split, nomatch = 0L)
    if (i_along > 0L)
      return(i_along)
    cli::cli_abort(c("Prior for {.var {nm_term}} has invalid value for {.arg along}.",
                     i = "Value supplied: {.val {along}}.",
                     i = "Valid choices: {.val {nm_split}}."))
  }
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
#' Make 'matrix_along_by' for all Parameters in One Term
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_effect <- function(prior, dimnames_term, var_time, var_age) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  ans <- make_matrix_along_by_inner(i_along = i_along,
                                    dim = dim)
  rownames <- list(dimnames_term[[i_along]])
  names(rownames) <- names(dimnames_term)[[i_along]]
  if (ncol(ans) > 1L) {
    colnames <- list(dimnames_to_levels(dimnames_term[-i_along]))
    names(colnames) <- dimnames_to_nm(dimnames_term[-i_along])
  }
  else
    colnames <- list(NULL)
  dimnames(ans) <- c(rownames, colnames)
  ans
}


## HAS_TESTS
#' Helper function for 'make_matrix_along_by_effectfree'
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sexgender variable, or NULL
#' @param dim Dimension of the term, once arguments
#' such as 'con' and 'append_zero' have been applied.
#'
#' @returns A matrix.
#'
#' @noRd
make_matrix_along_by_effectfree_inner <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  append_zero) {
  along <- prior$specific$along
  con <- prior$specific$con
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  if (append_zero)
    dim[[i_along]] <- dim[[i_along]] - 1L
  if (con == "by")
    dim[-i_along] <- dim[-i_along] - 1L
  make_matrix_along_by_effectfree_innermost(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            var_sexgender = var_sexgender,
                                            dim = dim)
}


## HAS_TESTS
#' Basic Function for Making 'matrix_along_by'
#'
#' Unlike 'along', 'i_along' can have length > 1.
#' 
#' @param i_along Index for the 'along' dimension(s)
#' @param dim Dimension of the array
#'
#' @returns A dense matrix
#'
#' @noRd
make_matrix_along_by_inner <- function(i_along, dim) {
  s <- seq_along(dim)
  n <- prod(dim)
  i <- seq.int(from = 0L, length.out = n)
  n_along <- prod(dim[i_along])
  a <- array(i, dim = dim)
  perm <- c(i_along, s[-i_along])
  ans <- aperm(a, perm = perm)
  ans <- matrix(ans, nrow = n_along)
  ans
}


## HAS_TESTS
#' Make 'matrix_along_by' for Free Parameters for Term
#' with Dynamic SVD Prior
#'
#' Helper function for 'make_matrix_along_by_effectfree_innermost'
#' 
#' @param prior Object of class 'bage_mod'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param var_sexgender Name of sex/gender dimension, or NULL
#' @param dim Dimension of term, once arguments
#' such as 'con' and 'append_zero' have been applied.
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_svddynamic <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            dim) {
  labels_svd <- get_labels_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  i_age <- match(var_age, nm_split)
  if (is.null(var_sexgender))
    i_sexgender <- 0L
  else
    i_sexgender <- match(var_sexgender, nm_split, nomatch = 0L)
  i_agesex <- c(i_age, i_sexgender)
  dim_noagesex <- dim[-i_agesex]
  dim <- c(length(labels_svd), dim_noagesex)
  i_along <- 1L
  has_var_time <- !is.null(var_time)
  if (has_var_time) {
    offset_time <- match(var_time, nm_split[-i_agesex])
    i_along <- i_along + offset_time
  }
  make_matrix_along_by_inner(i_along = i_along,
                             dim = dim)
}


## HAS_TESTS
#' Make a Matrix that Appends a Zero at the Start
#' of Each 'along' Series, Assuming 'along' is First Dimension
#'
#' @param dim_after Dimensions of array representing term,
#' after zeros have been added
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_append_zero <- function(dim_after) {
  n_along_after <- dim_after[[1L]]
  n_along_before <- n_along_after - 1L
  n_by <- prod(dim_after[-1L])
  m <- rbind(0, Matrix::.sparseDiagonal(n_along_before))
  I <- Matrix::.sparseDiagonal(n_by)
  Matrix::kronecker(I, m)
}


## HAS_TESTS
#' Make Matrix That Centers An Array Within Each Element of Along Dimension
#'
#' @param i_along Index for along dimension
#' @param dim Dimensions of array
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_con_by <- function(i_along, dim) {
  s <- seq_along(dim)
  perm <- c(i_along, s[-i_along])
  dim_after <- dim[perm]
  m_perm <- make_matrix_perm_along_to_front(i_along = i_along,
                                            dim_after = dim_after)
  dim_by <- dim[-i_along]
  Z <- make_matrix_unconstr_constr(dim_by)
  H <- tcrossprod(Z)
  n_along <- dim[[i_along]]
  I <- Matrix::.sparseDiagonal(n_along)
  ans <- Matrix::kronecker(H, I)
  ans <- Matrix::crossprod(m_perm, ans %*% m_perm)
  ans
}


## HAS_TESTS
#' Make a Matrix of Summation Constraints for an Array
#'
#' Contains redundant constraints
#'
#' @param dim The dimension of the array
#'
#' @returns A dense matrix of 0s and 1s.
#'
#' @noRd
make_matrix_constraints <- function(dim) {
  diags <- lapply(dim, diag)
  ones <- lapply(dim, function(d) matrix(1, nrow = 1L, ncol = d))
  s <- seq_along(dim)
  ans <- lapply(s, function(i) replace(diags, i, ones[i]))
  ans <- lapply(ans, rev)
  ans <- lapply(ans, function(x) Reduce(kronecker, x))
  ans <- do.call(rbind, ans)
  ans
}


## HAS_TESTS
#' Make Matrix for Post-Processing SVD Draws When
#' 'append_zero' is TRUE
#'
#' Note that the age/sexgender dimension is always
#' first in the inputs and outputs. We need to
#' temporariliy shift the time dimension
#' to first place to add the rows of zeros.
#'
#' @param prior Object of class 'bage_mod'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param var_sexgender Name of sex/gender dimension, or NULL
#' @param dim Dimension of term, once arguments
#' such as 'con' and 'append_zero' have been applied.
#' 
#' @returns A sparse diagonal matrix
#'
#' @noRd
make_matrix_draws_svd_appendzero <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender) {
  dim_svd <- make_dim_svd(prior = prior,
                          dimnames_term = dimnames_term,
                          var_age = var_age,
                          var_sexgender)
  i_time <- match(var_time, names(dim_svd))
  dim_nozero <- dim_svd
  dim_nozero[[i_time]] <- dim_nozero[[i_time]] - 1L
  s <- seq_along(dim_svd)
  perm <- c(i_time, s[-i_time])
  dim_nozero_timefirst <- dim_nozero[perm]
  m_to <- make_matrix_perm_along_to_front(i_along = i_time,
                                          dim_after = dim_nozero_timefirst)
  dim_haszero_timefirst <- dim_svd[perm]
  m_append <- make_matrix_append_zero(dim_haszero_timefirst)
  m_from <- make_matrix_perm_along_from_front(i_along = i_time,
                                              dim_after = dim_svd)
  m_from %*% m_append %*% m_to
}


## HAS_TESTS
#' Make Matrix for Post-Processing SVD Draws When
#' 'append_zero' is FALSE
#'
#' @param prior Object of class 'bage_mod'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param var_sexgender Name of sex/gender dimension, or NULL
#' @param dim Dimension of term, once arguments
#' such as 'con' and 'append_zero' have been applied.
#' 
#' @returns A sparse diagonal matrix
#'
#' @noRd
make_matrix_draws_svd_nozero <- function(prior,
                                         dimnames_term,
                                         var_time,
                                         var_age,
                                         var_sexgender) {
  dim_svd <- make_dim_svd(prior = prior,
                          dimnames_term = dimnames_term,
                          var_age = var_age,
                          var_sexgender)
  n <- prod(dim_svd)
  Matrix::.sparseDiagonal(n)
}


#' Make Matrix to Transform between 'effectfree' and 'effect
#'
#' Workhorse for 'make_matrix_effectfree_effect'
#'
#' @param along Name of 'along' variable, or NULL
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param append_zero Whether to append a zero to the start
#' of each 'along' series.
#' @param con Type of constraints
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_effectfree_effect_inner <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                append_zero,
                                                con) {
  if (uses_along(prior))
    con <- prior$specific$con
  else
    con <- "none"
  dim <- lengths(dimnames_term)
  s <- seq_along(dim)
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  ## we don't know the dimensions of 'effectfree',
  ## so we work backwards from 'effect', at each step,
  ## calculating the dimensions before the transformation
  ans <- vector(mode = "list", length = 5L)
  ## transform from along-first to original order
  ans[[1L]] <- make_matrix_perm_along_from_front(i_along = i_along, dim_after = dim)
  s_along_first <- c(i_along, s[-i_along])
  dim <- dim[s_along_first]
  ## append zeros
  if (append_zero) {
    ans[[2L]] <- make_matrix_append_zero(dim)
    dim[[1L]] <- dim[[1L]] - 1L
  }
  ## transform from unconstrained space to constrained space
  if (con == "by") {
    ans[[3L]] <- make_matrix_unconstr_constr_along(dim)
    dim[-1L] <- dim[-1L] - 1L
  }
  ## transform to along-first from original order
  ans[[4L]] <- make_matrix_perm_along_to_front(i_along = i_along, dim_after = dim)
  dim <- dim[match(s, s_along_first)]
  ## transform from subspace to original space
  matrix_sub_orig <- make_matrix_sub_orig(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender,
                                          dim_after = dim)
  ans[[5L]] <- matrix_sub_orig
  ## combine
  ans <- Filter(Negate(is.null), ans)
  ans <- Reduce('%*%', ans)
  ans
}


## HAS_TESTS
#' Make a Matrix that Permutes the Dimensions of the Array Representation
#' of a Term so that the 'age' and Possibly 'sex'/'gender' Dimensions Move
#' from First to Standard Positions
#'
#' Note that in the pre-transform matrix, age comes before
#' sex if i_age < i_sexgender, otherwise sex/gender comes before age.
#'
#' @param i_age Original index of 'age' dimension
#' @param i_sexgender Original index of 'sex' dimension, or 0L
#' @param dim Dimension of array after tranformation
#' (ie with 'age' and 'sex'/'gender' in their original positions)
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_perm_agesex_from_front <- function(i_age, i_sexgender, dim_after) {
  has_sex <- i_sexgender > 0L
  if (has_sex) {
    i_agesex <- c(i_age, i_sexgender)
    i_agesex <- sort(i_agesex)
  }
  else
    i_agesex <- i_age
  n <- prod(dim_after)
  a_standard <- array(seq_len(n), dim = dim_after)
  s <- seq_along(dim_after)
  perm <- c(i_agesex, s[-i_agesex])
  a_front <- aperm(a_standard, perm = perm)
  j <- match(a_standard, a_front)
  Matrix::sparseMatrix(i = seq_len(n),
                       j = j,
                       x = rep(1, times = n))
}
  

## HAS_TESTS
#' Make a Matrix that Permutes the Dimensions of the Array Representation
#' of a Term so that the 'along' Dimension Moves from First to its
#' Standard Position
#'
#' @param i_along Original index of 'along' dimension
#' @param dim_after Dimension of array after tranformation
#' (ie with 'along' dimension in original position)
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_perm_along_from_front <- function(i_along, dim_after) {
  n <- prod(dim_after)
  if (i_along == 1L) {
    Matrix::.sparseDiagonal(n)
  }
  else {
    s <- seq_along(dim_after)
    perm_before <- c(i_along, s[-i_along])
    dim_before <- dim_after[perm_before]
    perm_after <- match(s, perm_before)
    a0 <- array(seq_len(n), dim = dim_before)
    a1 <- aperm(a0, perm = perm_after)
    Matrix::sparseMatrix(i = seq_len(n),
                         j = a1,
                         x = rep(1, times = n))
  }
}


## HAS_TESTS
#' Make a Matrix that Permutes the Dimensions of the Array Representation
#' of a Term so that the 'along' Dimension Comes First
#'
#' @param i_along Original index of 'along' dimension
#' @param dim_after Dimension of array after transformation
#' (ie with 'along' dimension at front)
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_perm_along_to_front <- function(i_along, dim_after) {
  n <- prod(dim_after)
  if (i_along == 1L) {
    Matrix::.sparseDiagonal(n)
  }
  else {
    s <- seq_along(dim_after)
    perm <- c(i_along, s[-i_along])
    dim_before <- dim_after[match(s, perm)]
    a0 <- array(seq_len(n), dim = dim_before)
    a1 <- aperm(a0, perm = perm)
    Matrix::sparseMatrix(i = seq_len(n),
                         j = a1,
                         x = rep(1, times = n))
  }
}


## HAS_TESTS
#' Make a matrix of B-spline basis functions
#'
#' Based on Eilers and Marx (1996). Flexible Smoothing
#' with B-splines and Penalties.
#' Statistical Science, 11(2), 89-121.
#'
#' @param n_along Number of elements of dimension being modelled
#' @param n_comp Number of columns in spline matrix
#'
#' @returns Matrix with 'n_along' rows and 'n_comp' columns
#'
#' @noRd
make_matrix_spline <- function(n_along, n_comp) {
  n_interval <- n_comp - 3L
  interval_length <- (n_along - 1L) / n_interval
  start <- 1 - 3 * interval_length
  end <- n_along + 3 * interval_length
  x <- seq(from = start, to = end, by = 0.001)
  base <- splines::bs(x = x, df = n_comp + 5L)
  i_keep <- findInterval(seq_len(n_along), x)
  j_keep <- seq.int(from = 3L, length.out = n_comp)
  ans <- base[i_keep, j_keep]
  colmeans <- colMeans(ans)
  ans <- ans - rep(colmeans, each = nrow(ans))
  Matrix::sparseMatrix(i = row(ans),
                       j = col(ans),
                       x = as.double(ans))
}


## HAS_TESTS
#' Make Matrix from SVD Subspace to Original Space
#'
#' Workhorse for SVD methods for 'make_matrix_sub_orig'
#'
#' When 'con' is "by", project into a
#' lower-dimensional subspace.
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param dim_after Dimension after applying the
#' transform (which is not necessarily the same as the dimension
#' implied by 'dimnames_term')
#' @param con Type of constraints
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_sub_orig_svd <- function(prior,
                                     dimnames_term,
                                     var_age,
                                     var_sexgender,
                                     dim_after,
                                     con) {
  nm_split <- dimnames_to_nm_split(dimnames_term)
  i_age <- match(var_age, nm_split)
  if (is.null(var_sexgender))
    i_sexgender <- 0L
  else
    i_sexgender <- match(var_sexgender, nm_split, nomatch = 0L)
  n_by <- prod(dim_after[-c(i_age, i_sexgender)])
  ans <- get_matrix_or_offset_svd_prior(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        get_matrix = TRUE)
  if (con == "by") {
    ## make matrix that projects unconstrained age-sex into lower dimension
    dim_agesex_constr <- dim_after[sort(c(i_age, i_sexgender))]
    dim_agesex_unconstr <- dim_agesex_constr + 1L
    m_constr <- make_matrix_unconstr_constr(dim_agesex_unconstr)
    ans <- crossprod(m_constr, ans)
  }
  I <- Matrix::.sparseDiagonal(n_by)
  ans <- Matrix::kronecker(I, ans)
  m_agesex <- make_matrix_perm_agesex_from_front(i_age = i_age,
                                                 i_sexgender = i_sexgender,
                                                 dim_after = dim_after)
  ans <- m_agesex %*% ans
  ans
}


## HAS_TESTS
#' Make Matrix Mapping Unconstrained Parameter Vector
#' to Constrained Parameter Vector, Assuming 'along' is First Dimension
#'
#' Constraints apply within each category of the 'along' variable.
#'
#' @param dim_after Dimension of for array representation of term
#' after transformation (ie with unconstrained parameters)
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_unconstr_constr <- function(dim_after) {
  m_constr <- make_matrix_constraints(dim_after)
  qr <- qr(t(m_constr))
  Q <- qr.Q(qr, complete = TRUE)
  n_constr <- qr$rank
  Q[, -seq_len(n_constr), drop = FALSE]
}


## HAS_TESTS
#' Make Matrix Mapping Unconstrained Parameter Vector
#' to Constrained Parameter Vector, Assuming 'along' is First Dimension
#'
#' Constraints apply within each category of the 'along' variable.
#'
#' @param dim_after Dimension of for array representation of term
#' after transformation (ie with unconstrained parameters)
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_unconstr_constr_along <- function(dim_after) {
  n_dim <- length(dim_after)
  n_along <- dim_after[[1L]]
  dim_by <- dim_after[-1L]
  m_constr <- make_matrix_unconstr_constr(dim_by)
  I <- Matrix::.sparseDiagonal(n_along)
  Matrix::kronecker(m_constr, I)
}


## HAS_TESTS
#' Make Offset used in Converting 'effectfree' to 'effect' for SVD Priors
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param con Type of constraints
#'
#' @returns A vector of doubles
#'
#' @noRd
make_offset_effectfree_effect_svd <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender) {
  if (uses_along(prior)) {
    along <- prior$specific$along
    con <- prior$specific$con
  }
  else {
    along <- NULL
    con <- "none"
  }
  dim <- lengths(dimnames_term)
  s <- seq_along(dim)
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  ## we don't know the dimensions of 'effectfree',
  ## so we work backwards from 'effect', at each step,
  ## calculating the dimensions before the transformation
  ans <- vector(mode = "list", length = 4L)
  ## transform from along-first to original order
  ans[[1L]] <- make_matrix_perm_along_from_front(i_along = i_along,
                                                 dim_after = dim)
  s_along_first <- c(i_along, s[-i_along])
  dim <- dim[s_along_first]
  ## transform from unconstrained space to constrained space
  if (con == "by") {
    ans[[2L]] <- make_matrix_unconstr_constr_along(dim)
    dim[-1L] <- dim[-1L] - 1L
  }
  ## transform to along-first from original order
  ans[[3L]] <- make_matrix_perm_along_to_front(i_along = i_along,
                                               dim_after = dim)
  dim <- dim[match(s, s_along_first)]
  ## transform from subspace to original space
  offset <- make_offset_sub_orig_svd(prior = prior,
                                     dimnames_term = dimnames_term,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender,
                                     dim_after = dim,
                                     con = con)
  ans[[4L]] <- offset
  ## combine
  ans <- Filter(Negate(is.null), ans)
  ans <- Reduce('%*%', ans)
  ans <- as.double(ans)
  ans
}


## HAS_TESTS
#' Make Offset for Transformation from SVD Subspace to Original Space
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param dim_after Dimension after applying the
#' transform (which is not necessarily the same as the dimension
#' implied by 'dimnames_term')
#' @param con Type of constraints
#'
#' @returns A sparse matrix
#'
#' @noRd
make_offset_sub_orig_svd <- function(prior,
                                     dimnames_term,
                                     var_age,
                                     var_sexgender,
                                     dim_after,
                                     con) {
  nm_split <- dimnames_to_nm_split(dimnames_term)
  i_age <- match(var_age, nm_split)
  if (is.null(var_sexgender))
    i_sexgender <- 0L
  else
    i_sexgender <- match(var_sexgender, nm_split, nomatch = 0L)
  n_by <- prod(dim_after[-c(i_age, i_sexgender)])
  ans <- get_matrix_or_offset_svd_prior(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        get_matrix = FALSE)
  if (con == "by") {
    ## make matrix that projects unconstrained age-sex into lower dimension
    dim_agesex_constr <- dim_after[sort(c(i_age, i_sexgender))]
    dim_agesex_unconstr <- dim_agesex_constr + 1L
    m_constr <- make_matrix_unconstr_constr(dim_agesex_unconstr)
    ans <- crossprod(m_constr, ans)
  }
  ones <- Matrix::sparseMatrix(i = seq_len(n_by),
                               j = rep.int(1L, times = n_by),
                               x = rep.int(1L, times = n_by))
  ans <- Matrix::kronecker(ones, ans)
  m_agesex <- make_matrix_perm_agesex_from_front(i_age = i_age,
                                                 i_sexgender = i_sexgender,
                                                 dim_after = dim_after)
  ans <- m_agesex %*% ans
  ans <- as.double(ans)
  ans
}

  
