
## HAS_TESTS
#' Create object to hold data from scaled SVDs
#'
#' Create an object of class `bage_scaled_svd` to
#' hold scaled SVDs (singular value decompositions.)
#'
#' `data` has the following columns:
#'
#' - `sexgender` Labels for sexes/genders. The column
#'   can also be called `sex` or `gender`.
#' - `matrix` List column of matrices created by
#'   [scaled_svd_comp()]. Must have rownames.
#'   Must not have NAs. Each matrix has 10 columns.
#' - `offset` List column of vectors created by
#'   [scaled_svd_comp()]. Must have names, which
#'   are identical to the rownames of the corresponding
#'   element of `matrix`.
#'   
#' @param data A data frame. See Details for description.
#'
#' @returns An object of class `bage_scaled_svd`.
#'
#' @export
scaled_svd <- function(data) {
    nms_valid <- list(c("sex", "matrix", "offset"),
                      c("gender", "matrix", "offset"),
                      c("sexgender", "matrix", "offset"))
    n_comp <- 10L
    ## 'data' is a data frame
    if (!is.data.frame(data))
        cli::cli_abort(c("{.arg data} is not a data frame.",
                         i = "{.arg data} has class {.cls {class(data)}}."))
    ## names in 'data' unique
    nms_data <- names(data)
    i_dup <- match(TRUE, duplicated(nms_data), nomatch = 0L)
    if (i_dup > 0L)
        cli::cli_abort("{.arg data} has duplicated name: {.val {nms_data[[i_dup]]}}.")
    ## names in 'data' valid
    is_nms_valid <- FALSE
    for (nms in nms_valid) {
        if (setequal(nms_data, nms)) {
            is_nms_valid <- TRUE
            break
        }
    }
    if (!is_nms_valid)
        cli::cli_abort(c("{.arg data} does not have expected variables.",
                         i = "{.arg data} has variables {.val {nms_data}}."))
    nms_data <- sub("^sex$|^gender$", "sexgender", nms_data)
    names(data) <- nms_data
    ## 'sexgender' column has no NAs
    i_na_sex <- match(TRUE, is.na(data$sexgender), nomatch = 0L)
    if (i_na_sex > 0L)
        cli::cli_abort("Element {i_na_sex} of sex/gender variable is {.val {NA}}.")
    ## 'matrix' and 'offset' are both list columns, the elements of which are numeric, with no NAs
    for (nm in c("matrix", "offset")) {
        value <- data[[nm]]
        if (!is.list(value))
            cli::cli_abort(c("{.var {nm}} is not a list column.",
                             i = "{.var {nm}} has class {.cls {class(value)}}."))
        is_num <- vapply(value, is.numeric, TRUE)
        i_nonnum <- match(FALSE, is_num, nomatch = 0L)
        if (i_nonnum > 0L)
            cli::cli_abort(c("Element {i_nonnum} of {.var {nm}} is non-numeric.",
                             i = "Element {i_nonnum} has class {.cls {class(value[[i_nonnum]])}}."))
        has_na_val <- vapply(value, anyNA, TRUE)
        i_na_val <- match(TRUE, has_na_val, nomatch = 0L)
        if (i_na_val > 0L)
            cli::cli_abort("Element {i_na_val} of {.var {nm}} has NA.")
    }
    ## 'matrix' consists of matrices
    is_mat <- vapply(data$matrix, is.matrix, TRUE)
    i_nonmat <- match(FALSE, is_mat, nomatch = 0L)
    if (i_nonmat > 0L)
        cli::cli_abort(c("Element {i_nonmat} of {.var matrix} is not a matrix.",
                         i = "Element {i_nonmat} has class {.cls {class(data$matrix[[i_nonmat]])}}."))
    ## 'sexgender' includes levels ".Joint" and ".Total"
    for (nm in c(".Joint", ".Total")) {
        if (!(nm %in% data$sexgender))
            cli::cli_abort("Sex/gender variable does not have category {.val {nm}}.")
    }
    ## If, aside from ".Joint" and ".Total", 'sexgender' has
    ## two levels, then try reformatting these remaining levels
    ## to "Female", "Male". If unsuccessful, leave untouched.
    is_not_jt <- !(data$sexgender %in% c(".Joint", ".Total"))
    sexgender_other <- data$sexgender[is_not_jt]
    has_two_other_labels <- length(unique(sexgender_other)) == 2L
    if (has_two_other_labels) {
        sexgender_reformatted <- tryCatch(poputils::reformat_sex(sexgender_other,
                                                                 factor = FALSE),
                                          error = function(e) e)
        if (!inherits(sexgender_reformatted, "error"))
            data$sexgender[is_not_jt] <- sexgender_reformatted
    }
    ## nrow(matrix) == length(offset)
    nrow_matrix <- vapply(data$matrix, nrow, 1L)
    length_offset <- vapply(data$offset, length, 1L)
    is_eq <- nrow_matrix == length_offset
    i_uneq <- match(FALSE, is_eq, nomatch = 0L)
    if (i_uneq > 0L)
        cli::cli_abort(c("{.var matrix} and {.var offset} not consistent.",
                         i = "Element {i_uneq} of {.var matrix} has {nrow_matrix[[i_uneq]]} rows.",
                         i = "Element {i_uneq} of {.var offset} has length {length_offset[[i_uneq]]}."))
    ## elements of 'matrix' all have 'n_comp' columns
    ncol_matrix <- vapply(data$matrix, ncol, 1L)
    is_eq_n_comp <- ncol_matrix == n_comp
    i_neq_n_comp <- match(FALSE, is_eq_n_comp, nomatch = 0L)
    if (i_neq_n_comp > 0L)
        cli::cli_abort(c("Element {i_neq_n_comp} of {.var matrix} does not have {n_comp} columns.",
                         i = "Element {i_neq_n_comp} has {ncol_matrix[[i_neq_n_comp]]} columns."))
    ## elements of 'matrix' have rownames
    rn_matrix <- lapply(data$matrix, rownames)
    is_rn_null <- vapply(rn_matrix, is.null, TRUE)
    i_rn_null <- match(TRUE, is_rn_null, nomatch = 0L)
    if (i_rn_null > 0L)
        cli::cli_abort("Element {i_rn_null} of {.var matrix} does not have rownames.")
    ## elements of 'offset' have names
    nm_offset <- lapply(data$offset, names)
    is_nm_null <- vapply(nm_offset, is.null, TRUE)
    i_nm_null <- match(TRUE, is_nm_null, nomatch = 0L)
    if (i_nm_null > 0L)
        cli::cli_abort("Element {i_nm_null} of {.var offset} does not have names.")
    ## colnames in matrix match names in offset
    is_same_nm <- mapply(identical, rn_matrix, nm_offset)
    i_diff_nm <- match(FALSE, is_same_nm, nomatch = 0L)
    if (i_diff_nm > 0L)
        cli::cli_abort(c("{.var matrix} and {.var offset} not consistent.",
                         i = "Element {i_diff_nm} of {.var matrix} has rownames {rn_matrix[[i_diff_nm]]}.",
                         i = "Element {i_diff_nm} of {.var offset} has names {nm_offset[[i_diff_nm]]}."))
    ## create object
    data <- tibble::as_tibble(data)
    data <- data[c("sexgender", "matrix", "offset")]
    ans <- list(data = data)
    class(ans) <- "bage_scaled_svd"
    ans
}
