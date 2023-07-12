
#' Derive default prior from name of term
#'
#' @param nm_term Name of model term
#' @param var_age Name of age variable, or NULL
#' @param var_time Name of time variable, or NULL
#'
#' @returns A list of objects of class "bage_prior"
#'
#' @noRd
default_prior <- function(nm_term, var_age, var_time) {
    scale_intercept <- 10
    if (nm_term == "(Intercept)")
        NFixed(sd = scale_intercept)
    else if (nm_term %in% c(var_age, var_time))
        RW()
    else
        N()
}


## HAS_TESTS
#' Infer the name of the age variable
#'
#' Given a formula, guess which term, if
#' any, is the age variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_age <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_age(nms)
}


## HAS_TESTS
#' Infer the name of the sex or gender variable
#'
#' Given a formula, guess which term, if
#' any, is the age variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_sexgender <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_sexgender(nms)
}


## HAS_TESTS
#' Infer the name of the time variable
#'
#' Given a formula, guess which term, if
#' any, is the time variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_time <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_time(nms)
}


## HAS_TESTS
#' Identify age main effects and age-sex/gender interactions
#'
#' Classify terms as "age", "age:sex", "sex:age", and "other".
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named character vector.
#'
#' @noRd
make_agesex <- function(mod) {
    priors <- mod$priors
    var_age <- mod$var_age
    var_sexgender <- mod$var_sexgender
    nms <- names(priors)
    ans <- vapply(nms,
                  FUN = make_agesex_inner,
                  FUN.VALUE = "",
                  var_age = var_age,
                  var_sexgender = var_sexgender)
    names(ans) <- nms
    ans
}


#' Classify a term, based on the name
#'
#' Decide whether an intercept, main effect,
#' or interaction is
#' - an age main effect
#' - an age-sex/gender interaction
#' - a sex/gender-age interaction
#' - something else
#' based on the name of the term,
#' plus 'var_age' and 'var_sexgender'
#'
#' @param nm Name of the term. A string.
#' @param var_age Name of the age variable. A string.
#' @param var_sexgender Name of the sex/gender
#' variable. A string.
#'
#' @returns A string. One of "age", "age:sex", "sex:age", "other".
#'
#' @noRd
make_agesex_inner <- function(nm, var_age, var_sexgender) {
    nm_split <- strsplit(nm, split = ":")[[1L]]
    n <- length(nm_split)
    if (n == 1L) {
        if (identical(nm_split, var_age))
            "age"
        else
            "other"
    }
    else if (n == 2L) {
        if (identical(nm_split, c(var_age, var_sexgender)))
            "age:sex"
        else if (identical(nm_split, c(var_sexgender, var_age)))
            "sex:age"
        else
            "other"
    }
    else
        "other"
}


## HAS_TESTS
#' Make 'const'
#'
#' Make vector to hold constants for priors.
#'
#' We generate 'const' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_const <- function(mod) {
    priors <- mod$priors
    ans <- lapply(priors, function(x) x$const)
    ans <- unlist(ans, use.names = FALSE)
    ans
}


## HAS_TESTS
#' Make 'hyper'
#'
#' Make vector to hold hyper-parameters
#' for priors.
#'
#' We generate 'hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyper <- function(mod) {
    priors <- mod$priors
    ans <- rep(0, times = length(priors))
    lengths <- vapply(priors, function(x) x$n_hyper, 0L)
    ans <- rep(ans, times = lengths)
    ans
}


## HAS_TESTS
#' Make 'i_prior'
#'
#' Make 'i_prior' a vector of integers used to
#' choose appropriate 'logprior' function
#' in the TMB template
#'
#' We generate 'i_prior' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns An named integer vector.
#'
#' @noRd
make_i_prior <- function(mod) {
    priors <- mod$priors
    vapply(priors, function(x) x$i_prior, 0L)
}


## HAS_TESTS
#' Make vector of indicators showing whether
#' cell contributes to likelihood
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of 1Ls and 0Ls.
#'
#' @noRd
make_is_in_lik <- function(mod) {
    outcome <- mod$outcome
    offset <- mod$offset
    ans <- (!is.na(outcome)
        & !is.na(offset)
        & (offset > 0))
    as.integer(ans)
}


## HAS_TESTS
#' Lengths of vectors of parameters
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A named integer vector.
#'
#' @noRd
make_lengths_par <- function(mod) {
    priors <- mod$priors
    matrices_par_outcome <- mod$matrices_par_outcome
    ans <- vapply(matrices_par_outcome, ncol, 1L)
    names(ans) <- names(priors)
    ans
}


## HAS_TESTS
#' Lengths of vectors of free parameters
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A named integer vector.
#'
#' @noRd
make_lengths_parfree <- function(mod) {
    priors <- mod$priors
    matrices <- make_matrices_parfree_par(mod)
    ans <- vapply(matrices, ncol, 1L)
    names(ans) <- names(priors)
    ans
}


## HAS_TESTS
#' Make mapping used by MakeADFun
#'
#' Make 'map' argument to be passed to
#' MakeADFun. Normally just NULL,
#' but consists of a single factor
#' if one or more terms is being treated
#' as known.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns NULL or a list with
#' a single element called 'parfree'.
#'
#' @noRd
make_map <- function(mod) {
    priors <- mod$priors
    lengths_parfree <- make_lengths_parfree(mod)
    map_parfree <- lapply(lengths_parfree,
                          function(n) rep(0, times = n))
    for (i_term in seq_along(priors)) {
        prior <- priors[[i_term]]
        is_known <- is_known(prior)
        if (is_known)
            map_parfree[[i_term]][] <- NA
    }
    map_parfree <- unlist(map_parfree, use.names = FALSE)
    n <- length(map_parfree)
    is_na <- is.na(map_parfree)
    n_na <- sum(is_na)
    if (n_na == 0L)
        ans <- NULL
    else {
        map_parfree[!is_na] <- seq_len(n - n_na)
        map_parfree <- factor(map_parfree)
        ans <- list(parfree = map_parfree)
    }
    ans
}   


## HAS_TESTS
#' Make list of matrices mapping terms to full array
#'
#' Make list of matrices mapping main effects or
#' interactions to array holding full rates (or,
#' equivalently, the outcome variable, or the
#' linear predictor.)
#' 
#' @param formula Formula specifying model
#' @param outcome Array holding values for response
#' variable
#'
#' @returns A named list
#'
#' @noRd
make_matrices_par_outcome_array <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    dim_outcome <- dim(outcome)
    ans <- apply(factors,
                 MARGIN = 2L,
                 FUN = make_matrix_par_outcome_array,
                 dim = dim_outcome,
                 simplify = FALSE)
    names(ans) <- colnames(factors)
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        n_outcome <- length(outcome)
        i <- seq_len(n_outcome)
        j <- rep.int(1L, times = n_outcome)
        x <- rep.int(1L, times = n_outcome)
        m <- Matrix::sparseMatrix(i = i, 
                                  j = j,
                                  x = x)
        ans <- c(list("(Intercept)" = m), ans)
    }
    ans
}


## HAS_TESTS
#' Make list of matrices mapping terms to outcome vector
#'
#' Make list of matrices mapping main effects or
#' interactions to vector holding outcome
#' (or, equivalently, the linear predictor.)
#' 
#' @param formula Formula specifying model
#' @param data Data frame
#'
#' @returns A named list
#'
#' @noRd
make_matrices_par_outcome_vec <- function(formula, data) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE]
    factors <- factors > 0L
    nms_vars <- rownames(factors)
    nms_terms <- colnames(factors)
    ans <- vector(mode = "list", length = length(nms_terms))
    for (i_term in seq_along(nms_terms)) {
        nms_vars_term <- nms_vars[factors[, i_term]]
        data_term <- data[nms_vars_term]
        data_term[] <- lapply(data_term, factor)
        contrasts_term <- lapply(data_term, stats::contrasts, contrast = FALSE)
        nm_term <- nms_terms[[i_term]]
        formula_term <- paste0("~", nm_term, "-1")
        formula_term <- stats::as.formula(formula_term)
        m_term <- Matrix::sparse.model.matrix(formula_term,
                                              data = data_term,
                                              contrasts.arg = contrasts_term,
                                              row.names = FALSE)
        attr(m_term, "Dimnames")[2L] <- list(NULL)
        ans[[i_term]] <- m_term
    }
    names(ans) <- nms_terms
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        n_data <- nrow(data)
        i <- seq_len(n_data)
        j <- rep.int(1L, times = n_data)
        x <- rep.int(1L, times = n_data)
        m <- Matrix::sparseMatrix(i = i,
                                  j = j,
                                  x = x)
        ans <- c(list("(Intercept)" = m), ans)
    }
    ans        
}


## NO_TESTS
#' Make list of matrices mapping parfree to par
#'
#' Make list of matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of matrices
#'
#' @noRd
make_matrices_parfree_par <- function(mod) {
    priors <- mod$priors
    levels_par <- make_levels_par(mod)
    terms_par <- make_terms_par(mod)
    agesex <- make_agesex(mod)
    levels_par <- split(levels_par, terms_par)
    ans <- .mapply(make_matrix_parfree_par,
                   dots = list(prior = priors,
                               levels_par = levels_par,
                               agesex = agesex),
                   MoreArgs = list())
    names(ans) <- names(priors)
    ans    
}


## HAS_TESTS
#' Make matrix mapping term to full array
#'
#' Make sparse matrix mapping the elements of a
#' main effect or interaction to an array holding
#' the full rates (or equivalently, the outcome
#' variable, or the linear predictor.)
#'
#' @param dim `dim` attribute of array holding rates.
#' @param is_in_term Whether the term includes
#' the dimension.
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_par_outcome_array <- function(dim, is_in_term) {
    make_submatrix <- function(d, is_in) {
        i <- seq_len(d)
        j <- if (is_in) i else rep.int(1L, times = d)
        Matrix::sparseMatrix(i = i, j = j)
    }
    submatrices <- .mapply(make_submatrix,
                           dots = list(d = as.list(dim),
                                       is_in = as.list(is_in_term)),
                           MoreArgs = list())
    submatrices <- rev(submatrices)
    ans <- Reduce(Matrix::kronecker, submatrices)
    ans <- methods::as(ans, "dMatrix")
    ans
}


## HAS_TESTS
#' Make array holding offset variable
#' cross-classified by predictors
#'
#' Unlike in `xtabs()`, NAs are not converted
#' to 0s.
#' 
#' @param formula Formula specifying model
#' @param vname_offset Name of the offset variable.
#' @param data A data frame.
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_offset_array <- function(formula, vname_offset, data) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0(vname_offset,
                            "~",
                            paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- stats::as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data)
    dim_ans <- dim(ans)
    dn_ans <- dimnames(ans)
    ans <- as.double(ans)
    formula_na <- paste0("is.na(",
                         vname_offset,
                         ")~",
                         paste(nms_vars[-1L], collapse = "+"))
    formula_na <- stats::as.formula(formula_na)
    is_na <- stats::xtabs(formula_na, data = data) > 0L
    ans[is_na] <- NA_real_
    ans <- array(ans, dim = dim_ans, dimnames = dn_ans)
    ans
}


## HAS_TESTS
#' Make offset consisting of 1s and 0s,
#' the same size as outcome
#'
#' The return value contains 0s for
#' combinations of classifying
#' variables not found in the data.
#'
#' @param formula Formula specifying model
#' @param data A data frame
#'
#' @returns An array of 1.0s and 0.0s
#' (with named dimnames)
#'
#' @noRd
make_offset_ones_array <- function(formula, data) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0("~", paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- stats::as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data)
    ans <- array(as.double(ans),
                 dim = dim(ans),
                 dimnames = dimnames(ans))
    ans <- 1.0 * (ans > 0)
    ans
}


## HAS_TESTS
#' Make offset consisting entirely of 1s,
#' the same length as outcome
#'
#' @param data Data frame
#'
#' @returns Vector of doubles.
#'
#' @noRd
make_offset_ones_vec <- function(data) {
    rep(1.0, times = nrow(data))
}


## HAS_TESTS
#' Make vector holding offset variable
#'
#' The offset is standardised to have mean 1.
#' 
#' @param vname_offset Name of the offset variable.
#' @param data A data frame
#'
#' @returns An vector of doubles.
#'
#' @noRd
make_offset_vec <- function(vname_offset, data) {
    nms_data <- names(data)
    ans <- data[[match(vname_offset, nms_data)]]
    n_obs <- sum(!is.na(ans))
    if (n_obs >= 1L)
        ans <- ans / mean(ans, na.rm = TRUE)
    ans <- as.double(ans)
    ans
}


## NO_TESTS
#' Make list of offsets using in converting
#' parfree to par
#'
#' Make list vectors used in converting free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of vectors
#'
#' @noRd
make_offsets_parfree_par <- function(mod) {
    priors <- mod$priors
    lengths_par <- make_lengths_par(mod)
    ans <- .mapply(make_offset_parfree_par,
                   dots = list(prior = priors,
                               length_par = lengths_par),
                   MoreArgs = list())
    ans <- unlist(ans)
    ans    
}


## HAS_TESTS
#' Make array holding outcome variable
#' cross-classified by predictors
#'
#' Unlike in `xtabs()`, NAs are not converted
#' to 0s.
#'
#' @param formula Formula specifying model
#' @param data A data frame
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_outcome_array <- function(formula, data) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0(nms_vars[[1L]],
                            "~",
                            paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- stats::as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data, addNA = TRUE)
    dim_ans <- dim(ans)
    dn_ans <- dimnames(ans)
    ans <- as.double(ans)
    formula_na <- paste0("is.na(",
                         nms_vars[[1L]],
                         ")~",
                         paste(nms_vars[-1L], collapse = "+"))
    formula_na <- stats::as.formula(formula_na)
    is_na <- stats::xtabs(formula_na, data = data) > 0L
    ans[is_na] <- NA_real_
    ans <- array(ans, dim = dim_ans, dimnames = dn_ans)
    ans
}


## HAS_TESTS
#' Make vector holding outcome variable
#'
#' Extracts the outcome variable from 'data'.
#'
#' @param formula Formula specifying model
#' @param data A data frame
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_outcome_vec <- function(formula, data) {
    nm_response <- deparse1(formula[[2L]])
    nms_data <- names(data)
    ans <- data[[match(nm_response, nms_data)]]
    ans <- as.double(ans)
    ans
}


## HAS_TESTS
#' Make vector containing parameters for
#' intercept, main effects, and interactions
#'
#' Return value is 0 where a parameter is being estimated,
#' and potentially non-zero where a parameter is
#' being treated as known.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_parfree <- function(mod) {
    priors <- mod$priors
    lengths_parfree <- make_lengths_parfree(mod)
    ans <- lapply(lengths_parfree, function(n) rep(0, times = n))
    for (i_term in seq_along(priors)) {
        prior <- priors[[i_term]]
        is_known <- is_known(prior)
        if (is_known) {
            values <- values_known(prior)
            ans[[i_term]] <- values
        }
    }
    ans <- unlist(ans)
    names(ans) <- rep(names(priors), times = lengths_parfree)
    ans
}


## HAS_TESTS
#' Make default priors
#'
#' Make named list holding default priors.
#' Default prior is N(0, 1) for
#' all terms except
#' - intercept, where it is N(0, 10^2),
#' - age variable, where it is RW()
#' - time variable, where it is RW()
#'
#' @param formula Formula specifying model
#' @param var_age Name of age variable, or NULL
#' @param var_time Name of time variable, or NULL
#'
#' @returns Named list of objects with class
#' 'bage_prior'.
#'
#' @noRd
make_priors <- function(formula, var_age, var_time) {
    nms_terms <- attr(stats::terms(formula), "term.labels")
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept)
        nms_terms <- c("(Intercept)", nms_terms)
    ans <- lapply(X = nms_terms,
                  FUN = default_prior,
                  var_age = var_age,
                  var_time = var_time)
    names(ans) <- nms_terms
    ans
}        


## HAS_TESTS
#' Make factor identifying components of 'const'
#'
#' Make factor the same length as 'const',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with constants.
#'
#' We generate 'terms_const' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'const'.
#'
#' @noRd
make_terms_const <- function(mod) {
    priors <- mod$priors
    nms_terms <- names(priors)
    lengths <- vapply(priors, function(x) length(x$const), 0L)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'hyper'
#'
#' Make factor the same length as 'hyper',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with constants.
#'
#' We generate 'terms_hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'hyper'.
#'
#' @noRd
make_terms_hyper <- function(mod) {
    priors <- mod$priors
    nms_terms <- names(priors)
    lengths <- vapply(priors, function(x) x$n_hyper, 0L)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of
#' combined parameter vector
#'
#' Make factor the same length as
#' a combined parameter vector
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor.
#'
#' @noRd
make_terms_par <- function(mod) {
    priors <- mod$priors
    matrices <- mod$matrices_par_outcome
    lengths <- vapply(matrices, ncol, 1L)
    nms <- names(priors)
    ans <- rep(nms, times = lengths)
    ans <- factor(ans, levels = nms)
    ans
}


## NO_TESTS
#' Make factor identifying components of 'parfree'
#'
#' Make factor the same length as 'parfree',
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'parfree'.
#'
#' @noRd
make_terms_parfree <- function(mod) {
    priors <- mod$priors
    matrices <- make_matrices_parfree_par(mod)
    lengths <- vapply(matrices, ncol, 1L)
    nms <- names(priors)
    ans <- rep(nms, times = lengths)
    ans <- factor(ans, levels = nms)
    ans
}


## NO_TESTS
#' Make integer vector of flags for whether
#' each prior uses a matrix mapping parfree to par
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_matrix_parfree_par <- function(mod) {
    priors <- mod$priors
    ans <- vapply(priors, uses_matrix_parfree_par, TRUE)
    ans <- as.integer(ans)
    names(ans) <- names(priors)
    ans    
}


## NO_TESTS
#' Make integer vector of flags for whether
#' each prior uses an offset to convert
#' parfree to par
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_offset_parfree_par <- function(mod) {
    priors <- mod$priors
    ans <- vapply(priors, uses_offset_parfree_par, TRUE)
    ans <- as.integer(ans)
    names(ans) <- names(priors)
    ans    
}
