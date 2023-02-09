
## HAS_TESTS
#' Check and tidy scale
#'
#' Check that 'scale' is a positive
#' finite number, and coerce to double.
#'
#' @param scale A number
#'
#' @return The parameter, as a double.
#'
#' @noRd
check_and_tidy_scale <- function(scale) {
    checkmate::assert_number(scale,
                             lower = 0,
                             finite = TRUE)
    scale <- as.double(scale)
    if (isTRUE(all.equal(scale, 0)))
        stop(gettextf("'%s' equals %d",
                      "scale",
                      0L),
             call. = FALSE)
    scale
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
#' Check that response variable always zero when
#' offset variable is zero
#'
#' @param formula A formula
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
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
        stop(gettextf("'%s' is non-zero [%s] but '%s' is zero",
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





        
    
