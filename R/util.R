
## HAS_TESTS
#' Check Whether Currently in Test or Snapshot
#'
#' Based on testthat::is_testing() and testthat::is_snapshot()
#' 
#' @returns TRUE or FALSE
#'
#' @noRd
is_not_testing_or_snapshot <- function() {
  is_testing <- identical(Sys.getenv("TESTTHAT"), "true")
  is_snapshot <- identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")
  !is_testing && !is_snapshot
}
