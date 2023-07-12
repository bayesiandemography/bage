
get_matrix_or_offset_svd <- function(scaled_svd,
                                     levels_par,
                                     indep,
                                     agesex,
                                     get_matrix,
                                     n_comp) {
    data <- scaled_svd$data
    type <- data$type
    labels_age <- data$labels_age
    labels_sexgender <- data$labels_age
    ## determine type of values required
    if (agesex == "age")
        type_req <- "total"
    else if ((agesex %in% c("age:sex", "sex:age")) && !indep)
        type_req <- "joint"
    else if ((agesex %in% c("age:sex", "sex:age")) && indep)
        type_req <- "indep"
    else
        cli::cli_abort(paste("Internal error: unexpected combination of",
                             "{.var agesex} and {.var indep}."))
    ## get labels that are in same format as 'levels_par'
    is_type_req <- type == type_req
    if (type == "total")
        labels_all <- labels_age[is_type_req]
    else {
        is_age_first <- agesex == "age:sex"
        if (is_age_first)
            dots <- list(x = labels_age, y = labels_sexgender)
        else
            dots <- list(x = labels_sexgender, y = labels_age_all)
        dots <- lapply(dots, "[", is_type_req)
        labels_all <- .mapply(function(x, y) paste(x, y, sep = "."),
                              dots = dots,
                              MoreArgs = list())
    }
    ## find matched labels
    is_matched_labels <- FALSE
    for (i_all in seq_along(labels_all)) {
        labels <- labels_all[[i_all]]
        is_matched_labels <- setequal(labels, levels_par)
        if (is_matched_labels) {
            labels_matched <- labels
            break
        }
    }
    if (!is_matched_labels) {
        if (type == "total")
            msg <- c(paste("Labels for age main effect not consistent",
                           "with labels for age in {.var scaled_svd}."),
                     i = "Labels for age main effect: {levels_par}")
        else
            msg <- c(paste("Labels for interaction between age and sex/gender",
                           "not consistent with labels for age and sex/gender in",
                           "{.var scaled_svd}."),
                     i = "Labels for interaction: {levels_par}")
        cli::cli_abort(msg)
    }
    ## get order of levels in term
    i_levels_par <- match(levels_par, labels_matched)
    ## extract matrix or offset and align to term
    if (get_matrix) {
        cols <- seq_len(n_comp)
        ans <- data$matrix[[i_all]]
        ans <- ans[i_levels_par, cols, drop = FALSE]
    }
    else {
        ans <- data$offset[[i]]
        ans <- ans[i_levels_par]
    }
    ## return result
    ans
}


