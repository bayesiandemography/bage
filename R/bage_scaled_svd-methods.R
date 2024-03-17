
## 'print' --------------------------------------------------------------------

#' @export
print.bage_ssvd <- function(x, ...) {
    cat("<Object of class \"", class(x), "\">\n", sep = "")
    invisible(x)
}
