
## HAS_TESTS
#' Check and tidy a scale term
#'
#' Check that `x` is a positive
#' finite scalar, and coerce to double.
#'
#' @param x A positive number.
#' @param x_arg Name for `x` to be
#' used in error messages.
#'
#' @return `x`, coerced to double.
#'
#' @noRd
check_and_tidy_scale <- function(x, x_arg) {
    if (!is.numeric(x))
        cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                         i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
    if (length(x) != 1L)
        cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                         i = "{.arg {x_arg}} has length {length(x)}."))
    if (is.na(x))
        cli::cli_abort("{.arg {x_arg}} is NA.")
    if (is.infinite(x))
        cli::cli_abort("{.arg {x_arg}} is infinite.")
    if (x <= 0)
        cli::cli_abort(c("{.arg {x_arg}} is non-positive.",
                         i = "{.arg {x_arg}} equals {x}."))
    as.double(x)
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
    checkmate::assert_formula(formula)
    if (length(formula) < 3L)
        stop(gettextf("prior formula '%s' has too few elements",
                      deparse1(formula)),
             call. = FALSE)
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
        stop(gettextf("formula '%s' does not have any predictors",
                      deparse1(formula)),
             call. = FALSE)
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
        stop(gettextf("formula '%s' does not have a response variable",
                      deparse1(formula)),
             call. = FALSE)
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
        stop(gettextf("formula '%s' does not have variable \"%s\"",
                      deparse1(formula),
                      name),
             call. = FALSE)
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
    if (i_not_in_data > 0L)
        stop(gettextf("variable '%s' from formula '%s' not found in '%s'",
                      nms_formula[[i_not_in_data]],
                      deparse1(formula),
                      "data"),
             call. = FALSE)
    invisible(TRUE)
}


## NO_TESTS
#' Check 'n' argument
#'
#' @param n_spline A whole number greater
#' than or equal to 'min', and possibly NULL.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n, min, null_ok) {
    if (null_ok && is.null(n)) 
        return(invisible(TRUE))
    if (!is.numeric(n))
        cli::cli_abort(c("{.arg n} is non-numeric.",
                         i = "{.arg n} has class {.cls {class(n)}}."))
    if (length(n) != 1L)
        cli::cli_abort(c("{.arg n} does not have length 1.",
                         i = "{.arg n} has length {length(n)}."))
    if (is.na(n))
        cli::cli_abort("{.arg n} is NA.")
    if (is.infinite(n))
        cli::cli_abort("{.arg n} is infinite.")
    if (!isTRUE(all.equal(round(n), n)))
        cli::cli_abort(c("{.arg n} is not an integer.",
                         i = "{.arg n} equals {n}."))
    if (n < min)
        cli::cli_abort(c("{.arg n} is less than {min}.",
                         i = "{.arg n} equals {n}."))
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





        
    
