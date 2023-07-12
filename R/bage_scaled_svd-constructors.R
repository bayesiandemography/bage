
## NO_TESTS - TESTS INCOMPLETE
#' Create object to hold data from scaled SVDs
#'
#' Create an object of class `bage_scaled_svd` to
#' hold scaled SVDs (singular value decompositions.)
#'
#' `data` has the following columns:
#'
#' - `type` Type of decomposition. Choices are "total",
#'   "joint", and "independent". See details.
#' - `labels_age` Age labels for individual rows of
#'    matrices within `matrix` and individual elements
#'    of vectors within `offset`.
#' - `labels_sexgender` Sex/gender labels for individual rows of
#'    matrices within `matrix` and individual elements
#'    of vectors within `offset`, or `NULL`.
#'    Only non-`NULL` when `sexgender` is `".Joint"`
#'    (which is when both are needed to identify rows.)
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
    nms_valid <- c("type",
                   "labels_age",
                   "labels_sexgender",
                   "matrix",
                   "offset")
    type_valid <- c("total", "joint", "indep")
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
    if (!setequal(nms_data, nms_valid))
        cli::cli_abort(c("{.arg data} does not have expected variables.",
                         i = "{.arg data} has variables {.val {nms_data}}."))
    ## 'type' has no NAs
    i_na_type <- match(TRUE, is.na(data$type), nomatch = 0L)
    if (i_na_type > 0L)
        cli::cli_abort("Element {i_na_type} of {.var type} is {.val {NA}}.")
    ## 'type' has values "total", "joint", and "indep"
    if (!setequal(data$type, type_valid))
        cli::cli_abort(c("{.var type} does not have valid categories.",
                         i = "Actual categories: {unique(data$type)}.",
                         i = "Required categories: {type_valid}."))
    ## 'labels_age', 'labels_sexgender', 'matrix', and 'offset'
    ## are all list columns with no NAs
    for (nm in c("labels_age", "labels_sexgender", "matrix", "offset")) {
        value <- data[[nm]]
        if (!is.list(value))
            cli::cli_abort(c("{.var {nm}} is not a list column.",
                             i = "{.var {nm}} has class {.cls {class(value)}}."))
        has_na_val <- vapply(value, anyNA, TRUE)
        i_na_val <- match(TRUE, has_na_val, nomatch = 0L)
        if (i_na_val > 0L)
            cli::cli_abort("Element {i_na_val} of {.var {nm}} has NA.")
    }
    ## 'labels_sexgender' is NULL iff 'type' is "total"
    is_lsg_null <- vapply(data$labels_sexgender, is.null, TRUE)
    is_total <- data$type == "total"
    i_neq_lsg_total <- match(FALSE, is_lsg_null == is_total, nomatch = 0L)
    if (i_neq_lsg_total > 0L) {
        val_type <- data$type[[i_neq_lsg_joint]]
        val_lsg <- if (is_lsg_null[[i_neq_lsg_total]]) "NULL" else "non-NULL"
        cli::cli_abort(c("{.var type} and {.var labels_sexgender} are inconsistent.",
                         i = "Element {i_neq_lsg_type} of {.var type} is {.val val_type}.",
                         i = "Element {i_neq_lsg_type} of {.var labels_sexgender} is {val_lsg}."))
    }
    ## if 'labels_sexgender' non-NULL, then length of element
    ## equals length of element of 'labels_age'
    len_age <- lengths(data$labels_age)
    len_sg <- lengths(data$labels_sexgender)
    is_neq_age_sg <- (len_age != len_sg) & is_lsg_nonnull
    i_neq_age_sg <- match(TRUE, is_neq_age_sg, nomatch = 0L)
    if (i_neq_age_sg > 0L) {
        val_age <- len_age[[i_neq_age_sg]]
        val_sg <- len_sg[[i_neq_age_sg]]
        cli::cli_abort(c("{.var labels_age} and {.var labels_sexgender} are inconsistent.",
                         i = "Element {i_neq_age_sg} of {.var labels_age} has length {val_age}.",
                         i = "Element {i_neq_age_sg} of {.var labels_sexgender} has length {val_sg}."))
    }
    ## elements of 'labels_age' and 'offset' have same length
    len_off <- lengths(data$offset)
    is_neq_age_off <- len_age != len_off
    i_neq_age_off <- match(TRUE, is_neq_age_off, nomatch = 0L)
    if (i_neq_age_off > 0L) {
        val_age <- len_age[[i_neq_age_off]]
        val_off <- len_off[[i_neq_age_off]]
        cli::cli_abort(c("{.var labels_age} and {.var offset} are inconsistent.",
                         i = "Element {i_neq_age_off} of {.var labels_age} has length {val_age}.",
                         i = "Element {i_neq_age_off} of {.var offset} has length {val_off}."))
    }
    ## all elements of 'matrix' and 'offset' are numeric
    for (nm in c("matrix", "offset")) {
        value <- data[[nm]]
        is_num <- vapply(value, is.numeric, TRUE)
        i_nonnum <- match(FALSE, is_num, nomatch = 0L)
        if (i_nonnum > 0L)
            cli::cli_abort(c("Element {i_nonnum} of {.var {nm}} is non-numeric.",
                             i = "Element {i_nonnum} has class {.cls {class(value[[i_nonnum]])}}."))
    }
    ## 'matrix' consists of matrices
    is_mat <- vapply(data$matrix, is.matrix, TRUE)
    i_nonmat <- match(FALSE, is_mat, nomatch = 0L)
    if (i_nonmat > 0L)
        cli::cli_abort(c("Element {i_nonmat} of {.var matrix} is not a matrix.",
                         i = "Element {i_nonmat} has class {.cls {class(data$matrix[[i_nonmat]])}}."))
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
    data <- data[c("type", "labels_age", "labels_sexgender", "matrix", "offset")]
    ans <- list(data = data)
    class(ans) <- "bage_scaled_svd"
    ans
}
