
#' @importFrom generics augment
#' @export
generics::augment

#' Data and values from a fitted model
#'
#' @section Warning:
#' The tidymodels
#' [website](https://www.tidymodels.org/learn/develop/broom/)
#' states that the augment function may change soon.
#' The method here will be updated to match the new interface.
#' 
#' @param x A fitted `bage_mod` object.
#' @param interval Width of credible interval.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package],
#' consisting of the data, plus extra columns.
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod <- fit(mod)
#' tidy(mod)
#' @export
augment <- function(x, interval, ...) {
    NULL
}



#' @importFrom generics tidy
#' @export
generics::tidy

#' Components from a fitted model
#'
#' @param x A fitted `bage_mod` object.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @seealso [glimpse()] provides less detailed information,
#' and [augment()] provides more detailed.
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod <- fit(mod)
#' tidy(mod)
#' @export
tidy.bage_mod <- function(x, ...) {
    is_fitted <- !is.null(mod$est)
    if (!is_fitted)
        stop(gettext("model has not been fitted yet : need to call function 'fit'?"),
             call. = FALSE)                     
    terms_est <- make_terms_est(x)
    term <- names(terms_est)
    df <- vapply(terms_est, length, 0L)
    std.dev <- vapply(terms_est, stats::sd, 0)
    tibble::tibble(term, df, std.dev)
}


