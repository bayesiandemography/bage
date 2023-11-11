
## Functions used only with 'bage_scaled_svd' objects
## that aren't strictly methods

## HAS_TESTS
#' Extract a matrix or offset from a 'bage_scaled_svd' object
#'
#' @param scaled_svd Object of class 'bage_scaled_svd'
#' @param levels_effect Labels for an age main effect or
#' interaction between age and sex/gender
#' @param indep Whether SVDs for sexes/genders were
#' carried out jointly or separately
#' @param agesex String describing age main effect
#' or age-sex/gender interaction. One of
#' "age", "age:sex", "sex:age", "other".
#' @param get_matrix Whether to return matrix
#' or offset
#' @param n_comp Number of components of matrix
#' to return
#'
#' @returns A matrix or vector.
#'
#' @noRd
get_matrix_or_offset_svd <- function(scaled_svd,
                                     levels_effect,
                                     indep,
                                     agesex,
                                     get_matrix,
                                     n_comp) {
    n_comp_max <- 10L
    data <- scaled_svd$data
    type <- data$type
    labels_age <- data$labels_age
    labels_sexgender <- data$labels_sexgender
    ## determine type of SVD required
    if (identical(agesex, "age"))
        type_req <- "total"
    else if ((agesex %in% c("age:sex", "sex:age")) && !indep)
        type_req <- "joint"
    else if ((agesex %in% c("age:sex", "sex:age")) && indep)
        type_req <- "indep"
    else
        cli::cli_abort(paste("Internal error: unexpected combination of",
                             "{.var agesex} and {.var indep}."))
    ## get labels that are in same format as 'levels_effect'
    is_type_req <- type == type_req
    if (identical(type_req, "total"))
        labels_all <- labels_age[is_type_req]
    else {
        is_age_first <- identical(agesex, "age:sex")
        if (is_age_first)
            dots <- list(x = labels_age, y = labels_sexgender)
        else
            dots <- list(x = labels_sexgender, y = labels_age)
        dots <- lapply(dots, "[", is_type_req)
        labels_all <- .mapply(function(x, y) paste(x, y, sep = "."),
                              dots = dots,
                              MoreArgs = list())
    }
    ## find matched labels
    is_matched_labels <- FALSE
    for (i_all in seq_along(labels_all)) {
        labels <- labels_all[[i_all]]
        is_matched_labels <- setequal(labels, levels_effect)
        if (is_matched_labels) {
            labels_matched <- labels
            break
        }
    }
    if (!is_matched_labels) {
        if (type_req == "total")
            msg <- c(paste("Labels for age main effect not consistent",
                           "with labels for age in {.var scaled_svd}."),
                     i = "Labels for age main effect: {.val {levels_effect}}.")
        else
            msg <- c(paste("Labels for interaction between age and sex/gender",
                           "not consistent with labels for age and sex/gender in",
                           "{.var scaled_svd}."),
                     i = "Labels for interaction: {.val {levels_effect}}.")
        cli::cli_abort(msg)
    }
    ## get order of levels in term
    i_levels_effect <- match(levels_effect, labels_matched)
    ## extract matrix or offset and align to labels in main effect / interaction
    if (get_matrix) {
        ans <- data$matrix[is_type_req][[i_all]]
        cols <- seq_len(n_comp)
        if (type_req == "indep") {
            cols_extra <- seq.int(from = 0,
                                  by = n_comp_max,
                                  length.out = ncol(ans) %/% n_comp_max)
            cols_extra <- rep(cols_extra, each = n_comp)
            cols <- cols + cols_extra
        }
        ans <- ans[i_levels_effect, cols, drop = FALSE]
    }
    else {
        ans <- data$offset[is_type_req][[i_all]]
        ans <- ans[i_levels_effect]
    }
    ## return result
    ans
}


