
## HAS_TESTS
#' Check that 'by' argument consists of terms
#' named in 'formula'
#'
#' @param nms_by Character vector of variable names, or NULL
#' @param formula Model formula. Assumed to be valid.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_by_in_formula <- function(by, formula) {
    if (!is.null(by)) {
        terms <- terms(formula)
        terms <- stats::delete.response(terms)
        nms_dims <- rownames(attr(terms, "factors"))
        nms_invalid <- setdiff(by, nms_dims)
        n_invalid <- length(nms_invalid)
        if (n_invalid > 0L) {
            cli::cli_abort(c(paste("{.arg by} includes {n_invalid} dimension{?s} not",
                                   "included in {.arg formula}."),
                             i = "Dimension{?s} in {.arg by}: {.val {by}}.",
                             i = "Dimension{?s} in {.arg formula}: {.val {nms_dims}}.",
                             i = paste("Dimension{?s} in {.arg by} but not in",
                                       "{.arg formula}: {.val {nms_invalid}}.")))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'by' argument does not include
#' time dimension
#'
#' Applied if 'by' and 'var_time' both non-NULL.
#'
#' @param nms_by Character vector of variable names, or NULL
#' @param var_time Name of time variable, or NULL.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_by_excludes_time <- function(by, var_time) {
    if (!is.null(by) && !is.null(var_time)) {
        if (var_time %in% by) {
            cli::cli_abort(c("{.arg by} includes time dimension.",
                             i = "Dimension{?s} in {.arg by}: {.val {by}}.",
                             i = "Time dimension ({.var var_time}): {.val {var_time}}."))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check a logical flag
#'
#' @param x TRUE or FALSE
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_flag <- function(x) {
    nm <- deparse1(substitute(x))
    if (!identical(length(x), 1L))
        cli::cli_abort(c("{.arg {nm}} does not have length 1",
                         i = "{.arg {nm}} has length {length(x)}."))
    if (!is.logical(x))
        cli::cli_abort(c("{.arg {nm}} does not have class {.cls logical}.",
                         i = "{.arg {nm}} has class {.cls {class(x)}}"))
    if (is.na(x))
        cli::cli_abort("{.arg {nm}} is {.val {NA}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that formula for prior meets basic
#' formatting requirements
#'
#' @param A formula
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_format_prior_formula <- function(formula) {
    if (!inherits(formula, "formula")) {
        msg <- "{.arg formula} not a formula."
        if (inherits(formula, "bage_prior"))
            info  <- "{.arg formula} should have format {.code <term> ~ <prior>}."
        else
            info <- "{.arg formula} has class {.cls {class(formula)}}."
        msg <- c(msg, i = info)
        cli::cli_abort(msg)
    }
    n <- length(formula)
    if (n < 3L)
        cli::cli_abort(c("{.arg formula} has too few elements.",
                         i = "{.arg formula} should have format {.code <term> ~ <prior>}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'formula' has predictors
#'
#' @param formula A formula.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_has_predictors <- function(formula) {
    has_predictors <- is.matrix(attr(stats::terms(formula), "factors"))
    if (!has_predictors)
        cli::cli_abort(c("{.arg formula} does not include any predictors.",
                         i = "{.arg formula}: {.code {deparse1(formula)}}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'formula' has response
#'
#' @param formula A formula.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_has_response <- function(formula) {
    has_response <- attr(stats::terms(formula), "response") > 0L
    if (!has_response)
        cli::cli_abort(c("{.arg formula} does not include a response variable.",
                         i = "{.arg formula}: {.code {deparse1(formula)}}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that formula has a variable
#'
#' Check that 'formula' contains the variable 'name'
#' (as a main effect or interaction, or both)
#'
#' @param name A string
#' @param formula A formula
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_formula_has_variable <- function(name, formula) {
    factors <- attr(stats::terms(formula), "factors")
    varnames <- rownames(factors)[-1L]
    if (!(name %in% varnames))
        cli::cli_abort(c("{.arg formula} does not have variable {.val {name}}.",
                         i = "{.arg formula}: {deparse1(formula)}"))
    invisible(TRUE)
}


## HAS_TESTS
#' Check formula does not have function calls
#'
#' @param formula A formula.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_no_functions <- function(formula) {
    terms <- terms(formula)
    variables <- attr(terms, "variables")
    is_call <- vapply(variables[-1L], is.call, TRUE)
    i_call <- match(TRUE, is_call, nomatch = 0L)
    if (i_call > 0L) {
        str_formula <- deparse1(formula)
        str_call <- deparse1(variables[-1L][[i_call]])
        cli::cli_abort(c("{.arg formula} contains a function call.",
                         i = "{.arg formula}: {.code {str_formula}}.",
                         i = "Function call: {.code {str_call}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check variables used in 'formula' occur in 'data'
#'
#' @param formula A formula.
#' @param data A data frame.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_vnames_in_data <- function(formula, data) {
    nms_formula <- rownames(attr(stats::terms(formula), "factors"))
    nms_data <- names(data)
    is_in_data <- nms_formula %in% nms_data
    i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
    if (i_not_in_data > 0L) {
        nm_var <- nms_formula[[i_not_in_data]]
        cli::cli_abort(c("Variable {.var {nm_var}} from {.arg formula} not found in {.arg data}.",
                         i = "{.arg formula}: {.code {deparse(formula)}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check, based on its name, that a term is a main effect
#'
#' @param nm Name of the term.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_main_effect <- function(nm, prior) {
    nms_dims <- strsplit(nm, split = ":")[[1L]]
    is_main_effect <- length(nms_dims) == 1L
    if (!is_main_effect)
        cli::cli_abort(c("{.var {str_call_prior(prior)}} prior cannot be used for {.var {nm}} term.",
                         i = "{.var {str_call_prior(prior)}} prior can only be used with main effects."))
    invisible(TRUE)
}
    
        
## HAS_TESTS
#' Check that term has has least 'min' elements
#'
#' @param length_par Number of elements
#' @param min Minimum number of elements
#' @param nm Name of term
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_length_par_gt <- function(length_par, min, nm, prior) {
    if (length_par < min)
        cli::cli_abort(c(paste("{.var {str_call_prior(prior)}} prior cannot be",
                               "used for {.var {nm}} term."),
                         i = paste("{.var {str_call_prior(prior)}} prior can only be",
                                   "used with terms that have at least {min} element{?s}."),
                         i = "{.var {nm}} term has {length_par} element{?s}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'mod_est' and 'mod_sim' meet minimum
#' requirements for 'report_sim'
#'
#' @param mod_est,mod_sim Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mod_est_est_compatible <- function(mod_est, mod_sim) {
    ## same class
    if (!identical(class(mod_est)[[1L]], class(mod_sim)[[1L]]))
        cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different classes.",
                         i = "{.arg mod_est} has class {.cls {class(mod_est)}}.",
                         i = "{.arg mod_sim} has class {.cls {class(mod_sim)}}."))
    ## outcome variables are same
    nm_outcome_sim <- deparse1(mod_est$formula[[2L]])
    nm_outcome_est <- deparse1(mod_sim$formula[[2L]])
    if (!identical(nm_outcome_sim, nm_outcome_est))
        cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different outcome variables.",
                         i = "Outcome variable for {.arg mod_est}: {.val {nm_outcome_sim}}.",
                         i = "Outcome variable for {.arg mod_sim}: {.val {nm_outcome_est}}."))
    ## apart from outcome variable, data are the same
    data_sim <- mod_est$data
    data_est <- mod_sim$data
    nms_sim <- names(data_sim)
    nms_est <- names(data_est)
    if (!identical(sort(nms_sim), sort(nms_est)))
        cli::cli_abort(c(paste("Data for {.arg mod_est} and {.arg mod_sim} have",
                               "different variables."),
                         i = "Data for {.arg mod_est} has variables {.val {nms_sim}}.",
                         i = "Data for {.arg mod_sim} has variables {.val {nms_est}}."))
    for (nm in setdiff(nms_sim, nm_outcome_sim)) {
        var_sim <- data_sim[[nm]]
        var_est <- data_est[[nm]]
        is_same <- var_sim == var_est
        i_not_same <- match(FALSE, is_same, nomatch = 0L)
        if (i_not_same > 0L) {
            val_sim <- var_sim[[i_not_same]]
            val_est <- var_est[[i_not_same]]
            cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different data.",
                             i = paste("In row {i_not_same} of data for {.arg mod_est},",
                                       "variable {.var {nm}} has value {.val {val_sim}}."),
                             i = paste("In row {i_not_same} of data for {.arg mod_sim},",
                                       "variable {.var {nm}} has value {.val {val_est}}.")))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'n' argument
#'
#' @param n A whole number
#' @param n_arg Name for 'n' to be used in error messages
#' @param min,max Minimum and maximum values 'n' can take
#' @param null_ok Whether passing NULL (and skipping tests) is allowed
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n, n_arg, min, max, null_ok) {
    if (null_ok && is.null(n)) 
        return(invisible(TRUE))
    if (!is.numeric(n))
        cli::cli_abort(c("{.arg {n_arg}} is non-numeric.",
                         i = "{.arg {n_arg}} has class {.cls {class(n)}}."))
    if (length(n) != 1L)
        cli::cli_abort(c("{.arg {n_arg}} does not have length 1.",
                         i = "{.arg {n_arg}} has length {length(n)}."))
    if (is.na(n))
        cli::cli_abort("{.arg {n_arg}} is {.val {NA}}.")
    if (is.infinite(n))
        cli::cli_abort("{.arg {n_arg}} is {.val {Inf}}.")
    if (!isTRUE(all.equal(round(n), n)))
        cli::cli_abort(c("{.arg {n_arg}} is not an integer.",
                         i = "{.arg {n_arg}} is {.val {n}}."))
    if (n < min)
        cli::cli_abort(c("{.arg {n_arg}} is less than {min}.",
                         i = "{.arg {n_arg}} is {.val {n}}."))
    if (!is.null(max) && (n > max))
        cli::cli_abort(c("{.arg {n_arg}} is greater than {max}.",
                         i = "{.arg {n_arg}} is {.val {n}}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check offset occurs in 'data'
#'
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_in_data <- function(vname_offset, nm_offset, data) {
    nms_data <- names(data)
    if (!(vname_offset %in% nms_data))
        stop(gettextf("%s variable [%s] not found in '%s'",
                      nm_offset,
                      vname_offset,
                      "data"),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that offset variable has no
#' negative values
#'
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_nonneg <- function(vname_offset, nm_offset, data) {
    offset <- data[[vname_offset]]
    if (any(offset < 0, na.rm = TRUE))
        stop(gettextf("%s variable [%s] has negative values",
                      nm_offset,
                      vname_offset),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check offset not in formula
#'
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param formula Formula specifying model
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_not_in_formula <- function(vname_offset, nm_offset, formula) {
    nms_formula <- rownames(attr(stats::terms(formula), "factors"))
    if (vname_offset %in% nms_formula)
        stop(gettextf("%s variable [%s] included in formula '%s'",
                      nm_offset,
                      vname_offset,
                      deparse1(formula)),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable less than
#' or equal to offset variable
#'
#' @param formula A formula
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_le_offset <- function(formula,
                                 vname_offset,
                                 data) {
    nm_response <- deparse1(formula[[2L]])
    response <- data[[nm_response]]
    offset <- data[[vname_offset]]
    is_gt_offset <- !is.na(response) & !is.na(offset) & (response > offset)
    i_gt_offset <- match(TRUE, is_gt_offset, nomatch = 0L)
    if (i_gt_offset > 0L) {
        stop(gettextf("'%s' [%s] is greater than '%s' [%s]",
                      nm_response,
                      response[[i_gt_offset]],
                      vname_offset,
                      offset[[i_gt_offset]]),
             call. = FALSE)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable always zero when
#' offset variable is zero
#'
#' @param formula A formula
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_zero_if_offset_zero <- function(formula,
                                           vname_offset,
                                           data) {
    nm_response <- deparse1(formula[[2L]])
    response <- data[[nm_response]]
    offset <- data[[vname_offset]]
    response_pos <- response > 0
    offset_pos <- offset > 0
    is_pos_nonpos <- !is.na(response) & !is.na(offset) & response_pos & !offset_pos
    i_pos_nonpos <- match(TRUE, is_pos_nonpos, nomatch = 0L)
    if (i_pos_nonpos > 0L) {
        stop(gettextf("'%s' [%s] is non-zero but '%s' is zero",
                      nm_response,
                      response[[i_pos_nonpos]],
                      vname_offset),
             call. = FALSE)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable has no
#' negative values
#'
#' @param formula A formula
#' @param data A data frame
#' @param nm_distn Name of the disribution (eg "pois")
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_response_nonneg <- function(formula, data, nm_distn) {
    nm_response <- deparse1(formula[[2L]])
    response <- data[[nm_response]]
    if (any(response < 0, na.rm = TRUE))
        stop(gettextf("distribution is \"%s\" but response variable [%s] has negative values",
                      nm_distn,
                      nm_response),
             call. = FALSE)
    invisible(TRUE)
}
        
    
## HAS_TESTS
#' Check a scale term
#'
#' Check that `x` is a positive or non-negative
#' finite scalar.
#'
#' @param x A positive or non-negative number.
#' @param x_arg Name for `x` to be
#' used in error messages.
#' @param zero_ok Whether 'x' can be zero.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_scale <- function(x, x_arg, zero_ok) {
    if (!is.numeric(x))
        cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                         i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
    if (length(x) != 1L)
        cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                         i = "{.arg {x_arg}} has length {.val {length(x)}}."))
    if (is.na(x))
        cli::cli_abort("{.arg {x_arg}} is {.val {NA}}.")
    if (is.infinite(x))
        cli::cli_abort("{.arg {x_arg}} is infinite.")
    if (zero_ok) {
        if (x < 0)
            cli::cli_abort(c("{.arg {x_arg}} is negative.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    else {
        if (x <= 0)
            cli::cli_abort(c("{.arg {x_arg}} is non-positive.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'widths' consists of numbers between 0 and 1
#'
#' @param width s
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_widths <- function(widths) {
    if (!is.numeric(widths))
        cli::cli_abort(c("{.arg widths} is non-numeric",
                         i = "{.arg widths} has class {.cls {class(widths)}}."))
    if (length(widths) == 0L)
        cli::cli_abort("{.arg widths} has length 0.")
    n_na <- sum(is.na(widths))
    if (n_na > 0L)
        cli::cli_abort("{.arg widths} has {cli::qty(n_na)} NA{?s}.")
    n_out <- sum((widths <= 0) | (widths > 1))
    if (n_out > 0L)
        cli::cli_abort(c("{.arg widths} has {cli::qty(n_out)} value{?s} not in interval (0, 1]",
                         i = "{.arg widths}: {.val {widths}}"))
    invisible(TRUE)
}
