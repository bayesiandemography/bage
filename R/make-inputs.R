
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
infer_age_var <- function(formula) {
    p_valid_age <- "^age$|^agegroup$|^agegp$|^ageyear$|^ageyears$|^ageinterval$"
    names <- attr(stats::terms(formula), "term.labels")
    names_cleaned <- tolower(names)
    names_cleaned <- gsub("[^a-z]", "", names_cleaned)
    i <- grep(p_valid_age, names_cleaned)
    if (identical(length(i), 1L))
        names[[i]]
    else
        NULL
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
infer_time_var <- function(formula) {
    p_valid_time <- paste0("^time$|^period$|^year$|^month$|^quarter$|",
                           "^times$|^periods$|^years$|^months$|^quarters$|",
                           "^yearmonth$|^monthyear$|^yearquarter$|^quarteryear$")
    names <- attr(stats::terms(formula), "term.labels")
    names_cleaned <- tolower(names)
    names_cleaned <- gsub("[^a-z]", "", names_cleaned)
    i <- grep(p_valid_time, names_cleaned)
    if (identical(length(i), 1L))
        names[[i]]
    else
        NULL
}


## HAS_TESTS
#' Check for presence of main effect
#'
#' Check whether 'formula' includes a main effect
#' called 'name'
#'
#' @param name A string
#' @param formula A formula
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_main_effect <- function(name, formula) {
    term_names <- attr(stats::terms(formula), "term.labels")
    term_orders <- attr(stats::terms(formula), "order")
    i_name <- match(name, term_names, nomatch = 0L)
    if (i_name == 0L)
        FALSE
    else
        term_orders[[i_name]] == 1L
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
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_const <- function(priors) {
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
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyper <- function(priors) {
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
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns An named integer vector.
#'
#' @noRd
make_i_prior <- function(priors) {
    vapply(priors, function(x) x$i_prior, 0L)
}


## HAS_TESTS
#' Make vector of indicators showing whether
#' cell contributes to likelihood
#'
#' @param outcome Outcome array or vector.
#' @param offset Offset array or vector.
#'
#' @returns A vector of 1Ls and 0Ls.
#'
#' @noRd
make_is_in_lik <- function(outcome, offset) {
    ans <- (!is.na(outcome)
        & !is.na(offset)
        & (offset > 0))
    as.integer(ans)
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
#' @param priors A named list of priors.
#' @param terms_par Factor identifying
#' which term each element of 'par' the
#' parameter belongs to.
#'
#' @returns NULL or a list with
#' a single element called 'par'.
#'
#' @noRd
make_map <- function(priors, terms_par) {
    n <- length(terms_par)
    map_par <- rep(0, times = n)
    map_par <- split(map_par, terms_par)
    for (i_term in seq_along(priors)) {
        prior <- priors[[i_term]]
        is_known <- is_known(prior)
        if (is_known)
            map_par[[i_term]][] <- NA
    }
    map_par <- unlist(map_par, use.names = FALSE)
    is_na <- is.na(map_par)
    n_na <- sum(is_na)
    if (n_na == 0L)
        ans <- NULL
    else {
        map_par[!is_na] <- seq_len(n - n_na)
        map_par <- factor(map_par)
        ans <- list(par = map_par)
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
make_matrices_par_array <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    dim_outcome <- dim(outcome)
    ans <- apply(factors,
                 MARGIN = 2L,
                 FUN = make_matrix_par_array,
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
make_matrices_par_vec <- function(formula, data) {
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
make_matrix_par_array <- function(dim, is_in_term) {
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
#' Make matrix mapping term to outcome vector
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
make_matrix_par_vec <- function(dim, is_in_term) {
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
#' @param priors A named list of priors.
#' @param terms_par Factor identifying
#' which term each element of 'par' the
#' parameter belongs to.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_par <- function(priors, terms_par) {
    ans <- rep(0, times = length(terms_par))
    ans <- split(ans, terms_par)
    for (i_term in seq_along(priors)) {
        prior <- priors[[i_term]]
        is_known <- is_known(prior)
        if (is_known) {
            values <- values_known(prior)
            ans[[i_term]] <- values
        }
    }
    ans <- unlist(ans, use.names = FALSE)
    ans
}


## HAS_TESTS
#' Make default priors
#'
#' Make named list holding default priors.
#' Default prior is N(0, scale^2) for
#' all terms except
#' - intercept, where it is N(0, (10*scale)^2),
#' - age variable, where it is RW(scale)
#' - time variable, where it is RW(scale)
#'
#' @param formula Formula specifying model
#' @param scale A number
#' @param age_var Name of age variable, or NULL
#' @param time_var Name of time variable, or NULL
#'
#' @returns Named list of objects with class
#' 'bage_prior'.
#'
#' @noRd
make_priors <- function(formula, scale, age_var, time_var) {
    mult_intercept <- 10
    nms <- attr(stats::terms(formula), "term.labels")
    ans <- rep(list(new_bage_prior_norm(scale = scale)), times = length(nms))
    names(ans) <- nms
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        prior_intercept <- new_bage_prior_norm(scale = mult_intercept * scale)
        ans <- c(list("(Intercept)" = prior_intercept), ans)
    }
    has_age_var <- !is.null(age_var)
    if (has_age_var) {
        has_age_main_effect <- is_main_effect(name = age_var,
                                              formula = formula)
        if (has_age_main_effect)
            ans[[age_var]] <- RW(scale = scale)
    }
    has_time_var <- !is.null(time_var)
    if (has_time_var) {
        has_time_main_effect <- is_main_effect(name = time_var,
                                               formula = formula)
        if (has_time_main_effect)
            ans[[time_var]] <- RW(scale = scale)
    }
    ans
}


## HAS_TESTS
#' Calculate the scale of variation
#' in the outcome variable
#'
#' If at least 2 observations, use the
#' standard deviation; if only 1, use
#' the single value; otherwise return NA.
#'
#' Round to two significant figures.
#' (This makes printing nicer, and avoids
#' spurious precision.)
#'
#' @param outcome A vector or array holding outcomes.
#'
#' @returns A double.
#'
#' @noRd
make_scale_outcome <- function(outcome) {
    digits <- 2L
    obs <- outcome[!is.na(outcome)]
    n_obs <- length(obs)
    if (n_obs == 0L)
        ans <- NA_real_
    else if (n_obs == 1L)
        ans <- obs
    else
        ans <- stats::sd(obs)
    ans <- signif(ans, digits = digits)
    ans <- as.double(ans)
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
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A factor, the same length
#' as 'const'.
#'
#' @noRd
make_terms_const <- function(priors) {
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
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A factor, the same length
#' as 'hyper'.
#'
#' @noRd
make_terms_hyper <- function(priors) {
    nms_terms <- names(priors)
    lengths <- vapply(priors, function(x) x$n_hyper, 0L)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'par'
#'
#' Make factor vector the same length as 'par',
#' giving the name of the term
#' that the each element belongs to.
#'
#' We generate 'terms_par' when the 'bage_mod'
#' object is first created, since the number
#' and lengths of the terms is fixed from that
#' point.
#'
#' @param formula Formula specifying model
#' @param outcome Array holding values for response
#' variable
#'
#' @returns A factor.
#'
#' @noRd
make_terms_par <- function(formula, data) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    nms_dims <- rownames(factors)
    nms_terms <- colnames(factors)
    dim <- vapply(nms_dims, function(nm) length(unique(data[[nm]])), 0L)
    lengths <- apply(factors, 2L, function(i) prod(dim[i]))
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        lengths <- c(1L, lengths)
        nms_terms <- c("(Intercept)", nms_terms)
    }
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}



   
                   
                   
    
    
    
