
## HAS_TESTS
#' Turn a row from a data frame into a key=val string,
#' with mark-up expected by cli::cli_abort
#'
#' @param row A single row from a data frame
#'
#' @returns A string
#'
#' @noRd
make_str_key <- function(row) {
    key <- names(row)
    key <- sprintf("{.arg %s}", key)
    val <- vapply(row, format, "")
    val <- sprintf("{.val %s}", val)
    ans <- paste(key, val, sep = "=")
    ans <- paste(ans, collapse = ", ")
    ans
}

  
  
