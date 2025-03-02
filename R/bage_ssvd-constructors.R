
## HAS_TESTS
#' Create Object to Hold Data from a Scaled SVD
#'
#' Create an object of class `"bage_ssvd"` to
#' hold results from a scaled [Singular Value Decomposition][base::svd()]
#' (SVD) with `n_comp` components.
#'
#' `data` has the following columns:
#'
#' - `type` Type of decomposition. Choices are "total",
#'   "joint", and "indep".
#' - `labels_age` Age labels for individual rows of
#'    matrices within `matrix` and individual elements
#'    of vectors within `offset`.
#' - `labels_sexgender` Sex/gender labels for individual rows of
#'    matrices within `matrix` and individual elements
#'    of vectors within `offset`, or `NULL`.
#'    `NULL` when `sexgender` is `"total"`, since in
#'    this case results average across sexes/genders.
#' - `matrix` List column of sparse matrices.
#'   Must have rownames.
#'   Must not have NAs. When `type` is `"total"` or `"joint"`,
#'   each matrix has `n_comp` columns. When `"type"` is `"indep"`,
#'   each matrix has `2 * n_comp` columns.
#' - `offset` List column of vectors.
#'   Must have names, which
#'   are identical to the rownames of the corresponding
#'   element of `matrix`.
#'
#' `data` would normally be constructed using a function
#' in package [bssvd](https://bayesiandemography.github.io/bssvd/).
#'   
#' @param data A data frame. See Details for description.
#' 
#' @returns An object of class `"bage_ssvd"`.
#'
#' @seealso
#' - `bssvd::data_ssvd_hfd()` Prepare data from Human Fertility Database
#' - `bssvd::data_ssvd_hmd()` Prepare data from Human Mortality Database
#' - `bssvd::data_ssvd_lfp()` Prepare OECD data on labor force participation
#'
#' @examples
#' \dontrun{
#' data <- data_ssvd_hmd("hmd_statistics_20240226.zip")
#' HMD <- ssvd(data)
#' }
#' @noRd
ssvd <- function(data) {
  nms_valid <- c("type",
                 "labels_age",
                 "labels_sexgender",
                 "matrix",
                 "offset")
  type_valid <- c("total", "joint", "indep")
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
  is_valid_type <- data$type %in% type_valid
  i_invalid_type <- match(FALSE, is_valid_type, nomatch = 0L)
  if (i_invalid_type > 0L) 
    cli::cli_abort(c("{.var type} has invalid category.",
                     i = "Actual categories: {.val {unique(data$type)}}.",
                     i = "Valid categories: {.val {type_valid}}."))
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
    val_type <- data$type[[i_neq_lsg_total]]
    val_lsg <- if (is_lsg_null[[i_neq_lsg_total]]) "NULL" else "non-NULL"
    cli::cli_abort(c("{.var type} and {.var labels_sexgender} are inconsistent.",
                     i = "Element {i_neq_lsg_total} of {.var type} is {.val {val_type}}.",
                     i = "Element {i_neq_lsg_total} of {.var labels_sexgender} is {.val {val_lsg}}."))
  }
  ## if 'labels_sexgender' non-NULL, then length of element
  ## equals length of element of 'labels_age'
  len_age <- lengths(data$labels_age)
  len_sg <- lengths(data$labels_sexgender)
  is_neq_age_sg <- (len_age != len_sg) & !is_lsg_null
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
  ## 'matrix' consists of sparse matrices
  is_mat <- vapply(data$matrix, methods::is, TRUE, "sparseMatrix")
  i_nonmat <- match(FALSE, is_mat, nomatch = 0L)
  if (i_nonmat > 0L)
    cli::cli_abort(c("Element {i_nonmat} of {.var matrix} is not a sparse matrix.",
                     i = "Element {i_nonmat} has class {.cls {class(data$matrix[[i_nonmat]])}}."))
  ## all elements of 'offset' are numeric
  is_num <- vapply(data$offset, is.numeric, TRUE)
  i_nonnum <- match(FALSE, is_num, nomatch = 0L)
  if (i_nonnum > 0L)
    cli::cli_abort(c("Element {i_nonnum} of {.var offset} is non-numeric.",
                     i = "Element {i_nonnum} has class {.cls {class(data$offset[[i_nonnum]])}}."))
  ## nrow(matrix) == length(offset)
  nrow_matrix <- vapply(data$matrix, nrow, 1L)
  length_offset <- vapply(data$offset, length, 1L)
  is_eq <- nrow_matrix == length_offset
  i_uneq <- match(FALSE, is_eq, nomatch = 0L)
  if (i_uneq > 0L)
    cli::cli_abort(c("{.var matrix} and {.var offset} not consistent.",
                     i = "Element {i_uneq} of {.var matrix} has {nrow_matrix[[i_uneq]]} rows.",
                     i = "Element {i_uneq} of {.var offset} has length {length_offset[[i_uneq]]}."))
  ## elements of 'matrix' have 'n_comp' columns if type is "total" or "joint"
  ## and 2 * 'n_comp' columns if type is "indep"
  ncol_matrix <- vapply(data$matrix, ncol, 1L)
  ncol_total_1 <- ncol_matrix[is_total][[1L]]
  ncol_expected <- ifelse(data$type == "indep", 2L * ncol_total_1, ncol_total_1)
  is_ncol_expect <- ncol_matrix == ncol_expected
  i_ncol_unex <- match(FALSE, is_ncol_expect, nomatch = 0L)
  if (i_ncol_unex > 0L)
    cli::cli_abort(c("Elements of {.var matrix} have incompatible numbers of columns.",
                     i = "Columns for first matrix of type {.val total}: {.val {ncol_total_1}}.",
                     i = paste("Columns for element {.val {i_ncol_unex}} of {.var matrix}:",
                               "{.val {ncol_matrix[[i_ncol_unex]]}}."),
                     i = paste("Element {.val {i_ncol_unex}} of {.var matrix} has type",
                               "{.val {data$type[[i_ncol_unex]]}}.")))
  ## elements of 'matrix' have rownames
  rn_matrix <- lapply(data$matrix, rownames)
  is_rn_null <- vapply(rn_matrix, is.null, TRUE)
  i_rn_null <- match(TRUE, is_rn_null, nomatch = 0L)
  if (i_rn_null > 0L)
    cli::cli_abort("Element {i_rn_null} of {.var matrix} does not have rownames.")
  ## elements of 'offset' have names
  nm_offset_mod <- lapply(data$offset, names)
  is_nm_null <- vapply(nm_offset_mod, is.null, TRUE)
  i_nm_null <- match(TRUE, is_nm_null, nomatch = 0L)
  if (i_nm_null > 0L)
    cli::cli_abort("Element {i_nm_null} of {.var offset} does not have names.")
  ## rownames in matrix match names in offset
  is_same_nm <- mapply(identical, rn_matrix, nm_offset_mod)
  i_diff_nm <- match(FALSE, is_same_nm, nomatch = 0L)
  if (i_diff_nm > 0L)
    cli::cli_abort(c("{.var matrix} and {.var offset} not consistent.",
                     i = "Element {i_diff_nm} of {.var matrix} has rownames {.val {rn_matrix[[i_diff_nm]]}}.",
                     i = "Element {i_diff_nm} of {.var offset} has names {.val {nm_offset_mod[[i_diff_nm]]}}."))
  ## create object
  data <- tibble::as_tibble(data)
  data <- data[c("type", "labels_age", "labels_sexgender", "matrix", "offset")]
  ans <- list(data = data)
  class(ans) <- "bage_ssvd"
  ans
}
