
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
  joint <- prior$specific$joint
  is_indep <- !is.null(joint) && !joint
  ans <- paste0("comp", seq_len(n_comp))
  if (is_indep) {
    levels_sex <- dimnames_term[[var_sexgender]]
    ans <- paste(rep(levels_sex, each = n_comp),
                 ans,
                 sep = ".")
  }
  ans
}


## HAS_TESTS
#' Convert Mapping Matrix Such As 'matrix_agesex'
#' or 'matrix_along_by' to Index Matrix
#'
#' @param m Mapping matrix
#'
#' @returns A sparse matrix consisting of
#' 1s and 0s
#'
#' @noRd
invert_mapping_matrix <- function(m) {
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
    cli::cli_abort("Internal error: {.arg {var_age}} not specified.")
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



#' Make a 'matrix_along_by' Matrix for Free Parameters for One Term
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sexgender variable, or NULL
#'
#' @returns A matrix.
#'
#' @noRd
make_matrix_along_by_effectfree <- function(prior,
                                      dimnames_term,
                                      var_time,
                                      var_age,
                                      var_sexgender) {
  UseMethod("make_matrix_along_by_effectfree")
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  along <- prior$specific$along
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}


## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_spline <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  along <- prior$specific$along
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  labels_along <- paste0("comp", seq_len(n_comp))
  dimnames_term[[i_along]] <- labels_along
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}


#' @export
make_matrix_along_by_effectfree.bage_prior_svd <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_ar <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw2 <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
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
  dimnames_term <- dimnames_term[non_agesex]
  dimnames_term <- c(list(.svd = labels_svd), dimnames_term)
  has_time <- !is.null(var_time)
  if (has_time)
    i_along <- match(var_time, names(dimnames_term)[-1L]) + 1L ## can't match first dim
  else
    i_along <- 1L
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}
  
## HAS_TESTS
#' Make 'matrix_along_by' for all Forecasted Parameters in One Term
#'
#' Assumes that the term contains a time variable.
#' 
#' @param along Name of 'along' dimension, or NULL
#' @param labels_forecast Labels for future periods
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_effect_forecast <- function(along,
                                                 labels_forecast,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age) {
  i_time <- make_i_time(var_time = var_time,
                        dimnames_term = dimnames_term)
  dimnames_term[[i_time]] <- labels_forecast
  make_matrix_along_by_effect(along = along,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age)
}


#' Make 'matrix_along_by' for all Forecasted Free Parameters in One Term
#'
#' Assumes that the term contains a time variable.
#' 
#' @param prior Object of class 'bage_prior'
#' @param labels_forecast Labels for future periods
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time dimension, or NULL
#' @param var_age Name of age dimension, or NULL
#' @param var_sexgender Name of sex/gender dimension, or NULL
#'
#' @returns A matrix
#'
#' @noRd
make_matrix_along_by_effectfree_forecast <- function(prior,
                                                     labels_forecast,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender) {
  i_time <- make_i_time(var_time = var_time,
                        dimnames_term = dimnames_term)
  dimnames_term[[i_time]] <- labels_forecast
  make_matrix_along_by_effectfree(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender)
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

  
