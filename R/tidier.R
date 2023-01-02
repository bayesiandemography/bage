
#' @importFrom generics tidy
#' @export
generics::tidy

#' Summarise model output via 'tidy'
#'
#' Provide information on a model, including
#' overall performance, and individual-level
#' terms.
#'
#' @param x A `bage_sysmod` object.
#' @param ... Unused, included for generic consistency only.
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
    terms_est <- make_terms_est(x)
    term <- names(terms_est)
    df <- vapply(terms_est, length, 0L)
    std.dev <- vapply(terms_est, sd, 0)
    tibble::tibble(term, df, std.dev)
}
