
## Functions used only with 'bage_scaled_svd' objects
## that aren't strictly methods

## HAS_TESTS
#' Extract a matrix or offset from a 'bage_scaled_svd' object
#'
#' @param scaled_svd Object of class 'bage_scaled_svd'
#' @param joint Whether SVDs for sexes/genders were
#' carried out jointly or separately
#' @param agesex String describing age main effect
#' or age-sex/gender interaction. One of
#' "age", "age:sex", "sex:ag", "age:other",
#' "age:sex:other", "sex:age:other", "other".
#' @param get_matrix Whether to return matrix
#' or offset
#' @param n_comp Number of components of matrix
#' to return
#'
#' @returns A matrix or vector.
#'
#' @noRd
get_matrix_or_offset_svd <- function(scaled_svd,
                                     levels_age,
                                     levels_sexgender,
                                     joint,
                                     agesex,
                                     get_matrix,
                                     n_comp) {
  n_comp_max <- 10L
  data <- scaled_svd$data
  type <- data$type
  labels_age <- data$labels_age
  labels_sexgender <- data$labels_sexgender
  ## determine type of SVD required
  if (agesex %in% c("age", "age:other"))
    type_req <- "total"
  else if (agesex %in% c("age:sex", "sex:age",
                         "age:sex:other", "sex:age:other")) {
    type_req <- if (joint) "joint" else "indep"
  }
  else
    cli::cli_abort("Internal error: unexpected value for {.var agesex}.") ## nocov
  ## find matched age labels
  is_type_req <- type == type_req
  labels_age_type <- labels_age[is_type_req]
  labels_age_clean <- tryCatch(poputils::reformat_age(levels_age, factor = FALSE),
                               error = function(e) e)
  if (inherits(labels_age_clean, "error"))
    cli::cli_abort(c("Unable to parse age labels when processing SVD prior.",
                     i = labels_age_clean$message))
  is_matched <- vapply(labels_age_type,
                       setequal,
                       TRUE,
                       y = labels_age_clean)
  if (!any(is_matched))
    cli::cli_abort(c(paste("Unable to align age labels from {.arg data} with",
                           "age labels from {.arg scaled_svd}."),
                     i = "Age labels from {.arg data} before re-formatting: {levels_age}.",
                     i = "Age labels from {.arg data} after re-formatting: {labels_age_clean}."))
  i_matched <- which(is_matched)
  ## if sex/gender present, make sure that sex/gender labels match
  if (type_req %in% c("joint", "indep")) {
    labels_sexgender_clean <- tryCatch(poputils::reformat_sex(levels_sexgender, factor = FALSE),
                                       error = function(e) e)
    if (inherits(labels_sexgender_clean, "error"))
      cli::cli_abort(c("Unable to parse sex/gender labels when processing SVD prior.",
                       i = labels_sexgender_clean$message))
    labels_sexgender_type <- labels_sexgender[is_type_req][[i_matched]]
    if (!setequal(labels_sexgender_type, labels_sexgender_clean))
      cli::cli_abort(c(paste("Unable to align sex/gender labels from {.arg data} with",
                             "sex/gender labels from {.arg scaled_svd}."),
                       i = paste("Sex/gender labels from {.arg data} before re-formatting:",
                                 "{levels_sexgender}."),
                       i = paste("Sex/gender labels from {.arg data} after re-formatting:",
                                 "{labels_sexgender_clean}."),
                       i = paste("Sex/gender labels from {.arg scaled_svd}:",
                                 "{unique(labels_sexgender_type)}.")))
  }
  ## extract matrix or offset
  if (get_matrix) {
    ans <- data$matrix[is_type_req][[i_matched]]
    cols <- seq_len(n_comp)
    if (type_req == "indep") {
      cols_extra <- seq.int(from = 0,
                            by = n_comp_max,
                            length.out = ncol(ans) %/% n_comp_max)
      cols_extra <- rep(cols_extra, each = n_comp)
      cols <- cols + cols_extra
    }
    ans <- ans[, cols, drop = FALSE]
    nms_ans <- rownames(ans)
  }
  else {
    ans <- data$offset[is_type_req][[i_matched]]
    nms_ans <- names(ans)
  }
  ## align matrix/offset to term
  if (type_req == "total")
    i <- match(labels_age_clean, nms_ans)
  else {
    age_varies_fastest <- grepl("^age", agesex)
    n_age <- length(labels_age_clean)
    n_sexgender <- length(labels_sexgender_clean)
    if (age_varies_fastest)
      labels_clean <- paste(rep(labels_sexgender_clean, each = n_age),
                            labels_age_clean,
                            sep = ".")
    else
      labels_clean <- paste(labels_sexgender_clean,
                            rep(labels_age_clean, each = n_sexgender),
                            sep = ".")
    i <- match(labels_clean, nms_ans)
  }
  ## return result
  if (get_matrix)
    ans[i, , drop = FALSE]
  else
    ans[i]
}


