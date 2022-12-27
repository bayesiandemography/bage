

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
                      deparse(formula)),
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
                      deparse(formula)),
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
                      deparse(formula),
                      "data"),
             call. = FALSE)
    invisible(TRUE)
}

