
## HAS_TESTS
#' Derive default prior from name and length of term
#'
#' @param nm_term Name of model term
#' @param var_age Name of age variable, or NULL
#' @param var_time Name of time variable, or NULL
#' @param length_par Number of elements in term
#'
#' @returns A list of objects of class "bage_prior"
#'
#' @noRd
default_prior <- function(nm_term, var_age, var_time, length_par) {
    scale_intercept <- 10
    is_intercept <- nm_term == "(Intercept)"
    is_length_1 <- length_par == 1L
    is_age_time <- nm_term %in% c(var_age, var_time)
    if (is_intercept)
        NFix(sd = scale_intercept)
    else if (is_length_1)
        NFix()
    else if (is_age_time)
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
#' @returns A named list.
#'
#' @noRd
make_agesex <- function(mod) {
    priors <- mod$priors
    var_age <- mod$var_age
    var_sexgender <- mod$var_sexgender
    nms <- names(priors)
    ans <- lapply(nms,
                  make_agesex_inner,
                  var_age = var_age,
                  var_sexgender = var_sexgender)
    names(ans) <- nms
    ans
}


## HAS_TESTS
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
#' WARNING. This must be the name that
#' is used internally, which is not necessarily
#' the one that appears in the original
#' formula, as base::terms() and friends
#' switch dimension order.
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
        if (is.null(var_age))
            NULL
        else if (identical(nm_split, var_age))
            "age"
        else
            "other"
    }
    else if (n == 2L) {
        if (is.null(var_age) || is.null(var_sexgender))
            NULL
        else if (identical(nm_split, c(var_age, var_sexgender)))
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
#' Make 'const_season'
#'
#' Make vector to hold constants for seasonal effect.
#'
#' We generate 'const_season' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when seasonal effect changes  via 'set_season'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_const_season <- function(mod) {
    has_season <- has_season(mod)
    if (has_season)
        mod$scale_season
    else
        0
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
#' Make vector containing hyper-parameters for
#' seasonal effect
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_hyper_season <- function(mod) {
    0 ## standard deviation
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
#' Return one-based index of time main effect
#'
#' Returns 0 when 'var_time' is NULL, or
#' no time main effect in model.
#'
#' @param mod Object of class "bage_mod".
#'
#' @returns Non-negative integer
#'
#' @noRd
make_idx_time <- function(mod) {
    var_time <- mod$var_time
    priors <- mod$priors
    if (is.null(var_time))
        ans <- 0L 
    else {
        nms <- names(priors)
        ans <- match(var_time, nms)
    }
    ans
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
#' Make levels associated with each element of 'par'
#'
#' Make levels for each term, eg ages, times.
#' 'make_levels_par' works with the matrices
#' used to map levels to the outcome, to
#' ensure that the levels are correct (rather than
#' relying on undocumented properties of 'xtabs' etc),
#' though this makes the function a bit complicated.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_par <- function(mod) {
    formula <- mod$formula
    matrices_par_outcome <- mod$matrices_par_outcome
    outcome <- mod$outcome
    data <- mod$data
    nms <- names(matrices_par_outcome)
    n <- length(nms)
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    if (is.array(outcome))
        dim_levels <- expand.grid(dimnames(outcome))
    else
        dim_levels <- data[rownames(factors)]
    ans <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
        nm <- nms[[i]]
        if (nm == "(Intercept)")
            ans[[i]] <- "(Intercept)"
        else {
            i_dim <- factors[, nm, drop = TRUE]
            paste_dot <- function(...) paste(..., sep = ".")
            term_levels <- do.call(paste_dot, dim_levels[i_dim])
            matrix_par <- matrices_par_outcome[[i]]
            i_term_level <- apply(matrix_par, 2L, function(x) match(1L, x))
            ans[[i]] <- term_levels[i_term_level]
        }
    }
    ans <- unlist(ans, use.names = FALSE)
    ans
}


## HAS_TESTS
#' Make mapping used by MakeADFun
#'
#' Make 'map' argument to be passed to MakeADFun.
#' Return value is non-NULL if
#' (i) any priors are "bage_prior_known", or
#' (ii) 'scale_disp' is 0, or
#' (iii) the model does not include season effects.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns NULL or a named list
#'
#' @noRd
make_map <- function(mod) {
    priors <- mod$priors
    scale_disp <- mod$scale_disp
    n_season <- mod$n_season
    ## determine whether any parameters fixed
    is_known <- vapply(priors, is_known, FALSE)
    is_parfree_fixed <- any(is_known)
    is_disp_fixed <- scale_disp == 0
    is_season_fixed <- n_season == 0L
    ## return NULL if nothing fixed
    if (!is_parfree_fixed && !is_disp_fixed && !is_season_fixed)
        return(NULL)
    ## otherwise construct named list
    ans <- list()
    if (is_parfree_fixed)
        ans$parfree <- make_map_parfree_fixed(mod)
    if (is_disp_fixed)
        ans$log_disp <- factor(NA)
    if (is_season_fixed) {
        ans$par_season <- make_map_par_season_fixed(mod)
        ans$hyper_season <- make_map_hyper_season_fixed(mod)
    }
    ans
}


## HAS_TESTS
#' Make 'hyper_season' component of 'map'
#' argument to MakeADFun
#'
#' Only called when model does not have
#' seasonal effect (implying that 'hyper_season'
#' must be treated as fixed.)
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of NAs
#'
#' @noRd
make_map_hyper_season_fixed <- function(mod) {
    factor(NA)
}


## HAS_TESTS
#' Make 'par_season' component of 'map'
#' argument to MakeADFun
#'
#' Only called when model does not have
#' seasonal effect (implying that 'par_season'
#' must be treated as fixed.)
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of NAs
#'
#' @noRd
make_map_par_season_fixed <- function(mod) {
    par_season <- make_par_season(mod)
    n_par <- length(par_season)
    ans <- rep(NA, times = n_par)
    ans <- factor(ans)
    ans
}


## HAS_TESTS
#' Make 'parfree' component of 'map'
#' argument to MakeADFun
#'
#' Only called when model has Known prior.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor vector of NAs
#'
#' @noRd
make_map_parfree_fixed <- function(mod) {
    priors <- mod$priors
    is_known <- vapply(priors, is_known, FALSE)
    lengths <- make_lengths_parfree(mod)
    ans <- ifelse(is_known, NA, 0L)
    ans <- rep(ans, times = lengths)
    n <- length(ans)
    is_na <- is.na(ans)
    n_na <- sum(is_na)
    ans[!is_na] <- seq_len(n - n_na)
    ans <- factor(ans)
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


## HAS_TESTS
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
#' @param vname_offset Name of the offset variable.
#' @param data A data frame
#'
#' @returns An vector of doubles.
#'
#' @noRd
make_offset_vec <- function(vname_offset, data) {
    nms_data <- names(data)
    ans <- data[[match(vname_offset, nms_data)]]
    ans <- as.double(ans)
    ans
}


## HAS_TESTS
#' Make combined vector offsets using in converting
#' parfree to par
#'
#' Make combined vector of offsets used in converting free parameters
#' for main effects or interactions to
#' full parameter vectors. Note that in TMB itself,
#' the combined vector is split into pieces using
#' terms_parfree.
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named vector of doubles.
#'
#' @noRd
make_offsets_parfree_par <- function(mod) {
    priors <- mod$priors
    levels_par <- make_levels_par(mod)
    terms_par <- make_terms_par(mod)
    agesex <- make_agesex(mod)
    levels_par <- split(levels_par, terms_par)
    ans <- .mapply(make_offset_parfree_par,
                   dots = list(prior = priors,
                               levels_par = levels_par,
                               agesex = agesex),
                   MoreArgs = list())
    names(ans) <- names(priors)
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
#' seasonal effect
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_par_season <- function(mod) {
    matrix <- mod$matrix_season_outcome
    n <- ncol(matrix)
    rep(0, times = n)
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
#' @param lengths_par Number of elements in each term
#'
#' @returns Named list of objects with class
#' 'bage_prior'.
#'
#' @noRd
make_priors <- function(formula, var_age, var_time, lengths_par) {
    nms_terms <- attr(stats::terms(formula), "term.labels")
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept)
        nms_terms <- c("(Intercept)", nms_terms)
    ans <- .mapply(default_prior,
                   dots = list(nm_term = nms_terms,
                               length_par = lengths_par),
                   MoreArgs = list(var_age = var_age,
                                   var_time = var_time))
    names(ans) <- nms_terms
    ans
}        


## HAS_TESTS
#' Make 'random' argument to MakeADFun function
#'
#' Return value always includes "parfree".
#' Also contains "par_season" if the model has
#' a seasonal effect.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A character vector
#'
#' @noRd
make_random <- function(mod) {
    has_season <- has_season(mod)
    ans <- "parfree"
    if (has_season)
        ans <- c(ans, "par_season")
    ans
}


## HAS_TESTS
#' Make a matrix of B-spline basis functions
#'
#' Based on Eilers and Marx (1996). Flexible Smoothing
#' with B-splines and Penalties.
#' Statistical Science, 11(2), 89-121.
#'
#' @param length_par Number of elements in main
#' effect.
#' @param n_spline Number of columns in spline matrix
#'
#' @returns Matrix with 'length_par' rows and 'n_spline' columns
#'
#' @noRd
make_spline_matrix <- function(length_par, n_spline) {
    n_interval <- n_spline - 3L
    interval_length <- (length_par - 1L) / n_interval
    start <- 1 - 3 * interval_length
    end <- length_par + 3 * interval_length
    x <- seq(from = start, to = end, by = 0.001)
    base <- splines::bs(x = x, df = n_spline + 5L)
    i_keep <- findInterval(seq_len(length_par), x)
    j_keep <- seq.int(from = 3L, length.out = n_spline)
    ans <- base[i_keep, j_keep]
    colmeans <- colMeans(ans)
    ans <- ans - rep(colmeans, each = nrow(ans))
    Matrix::sparseMatrix(i = row(ans),
                         j = col(ans),
                         x = as.double(ans))
}


## HAS_TESTS
#' Make sparse matrix describing the relationship
#' between a dimension and the outcome array
#'
#' @param d Integer. Length of dimension
#' @param is_in Logical vector. Whether the dimension
#' is in the array.
#'
#' @returns A sparse matrix
#'
#' @noRd
make_submatrix <- function(d, is_in) {
    i <- seq_len(d)
    j <- if (is_in) i else rep.int(1L, times = d)
    Matrix::sparseMatrix(i = i, j = j)
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


## HAS_TESTS
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


## HAS_TESTS
#' Make integer vector of flags for whether
#' each prior uses hyper-parameters
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_hyper <- function(mod) {
    priors <- mod$priors
    has_hyper <- function(x) x$n_hyper > 0L
    ans <- vapply(priors, has_hyper, TRUE)
    ans <- as.integer(ans)
    names(ans) <- names(priors)
    ans
}


## HAS_TESTS
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


## HAS_TESTS
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
