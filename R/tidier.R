
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
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package],
#' consisting of the original `data` argument,
#' to the original model function (eg [mod_pois()])
#' plus four new columns:
#' - `.fitted` Point estimates (posterior medians) of
#' the rate, probability, or mean.
#' - `.lower`, `.upper` Lower and upper bounds of
#' 95% credible intervals for the rate, probability,
#' or mean.
#' - `.observed` Direct estimates of the rate,
#' probability, or mean.
#'
#' @examples
#' mod_pois(injuries ~ age + sex + year,
#'          data = injuries,
#'          exposure = popn) |>
#'   fit() |>
#'   augment()
#' @export
augment.bage_mod <- function(x, ...) {
    data <- x$data
    draws <- make_draws_fitted(x)
    quantiles <- matrixStats::rowQuantiles(draws,
                                           probs = c(0.025, 0.5, 0.975))
    data$.fitted <- quantiles[, 2L]
    data$.lower <- quantiles[, 1L]
    data$.upper <- quantiles[, 3L]
    data$.observed <- make_observed(x)
    data
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
    is_fitted <- !is.null(x$est)
    if (!is_fitted)
        stop(gettext("model has not been fitted yet : need to call function 'fit'?"),
             call. = FALSE)                     
    terms_est <- make_terms_est(x)
    term <- names(terms_est)
    df <- vapply(terms_est, length, 0L)
    std.dev <- vapply(terms_est, stats::sd, 0)
    tibble::tibble(term, df, std.dev)
}


