
## HAS_TESTS
#' Create New RR3 Data Model
#'
#' Data model assumes that outcome has been
#' randomly rounded to base 3.
#'
#' @returns Object of class 'bage_mod_rr3'
#'
#' @noRd
new_bage_datamod_outcome_rr3 <- function() {
  ans <- list(nm = "rr3")
  class(ans) <- c("bage_datamod_outcome_rr3", "bage_datamod_outcome", "bage_datamod")
  ans
}
