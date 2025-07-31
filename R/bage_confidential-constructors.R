
## HAS_TESTS
#' Create New RR3 Confidentialization Procedure
#'
#' Confidentialization procedure assumes that outcome has been
#' randomly rounded to base 3.
#'
#' @returns Object of class 'bage_confidential_rr3'
#'
#' @noRd
new_bage_confidential_rr3 <- function() {
  ans <- list(nm = "rr3")
  class(ans) <- c("bage_confidential_rr3", "bage_confidential")
  ans
}
