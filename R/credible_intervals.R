
#' Calculate credible intervals and medians.
#'
#' Calculate credible intervals and medians
#' for a measure variable,
#' within every combination of  the
#' classification variables. Output from \code{credible_intervals}
#' follows the 'simple names' format of function
#' \code{point_interval} in package \code{ggdist},
#' and should work with \code{ggdist} plotting functions.
#' \code{credible_intervals} is less flexible but faster
#' than \code{point_interval}.
#'
#' \code{length(measure_var}} should equal
#' \code{nrow(classif_vars}}. 
#' \code{classif_vars} should contain multiple
#' copies of each combination of the classifying
#' variables. Each copy typically represents a
#' draw from a simulation.
#'
#' \code{credible_intervals} does not assume that
#' \code{classif_vars} and \code{measure_var}
#' have been sorted.
#'
#' @param classif_vars A data frame with classification
#' variable(s).
#' @param measure_var A numeric vector with measurements.
#' @param width Width(s) of credible interval(s).
#' A numeric vector, with values in \code{(0,1]}.
#' @param na_rm Whether to remove \code{NA}s
#' from \code{measure_var} before calculating
#' quantiles.
#' 
#' @return A data frame with \code{nrow(unique(classif_vars))}
#' rows and \code{ncol(classif_vars)+4} columns.
#'
#' @seealso Quantiles are calculated using
#' function \code{\link[matrixStats]{rowQuantiles}},
#' which is very fast.
#'
#' @export
credible_intervals <- function(classif_vars, measure_var, width = 0.95,
                               na_rm = FALSE) {
    nm_measure_var <- checkmate::vname(measure_var)
    checkmate::assert_data_frame(classif_vars, min.cols = 1L)
    checkmate::assert_numeric(measure_var) 
    checkmate::assert_numeric(width,
                              lower = 0,
                              upper = 1,
                              min.len = 1L,
                              unique = TRUE)
    if (any(width == 0))
        stop(gettextf("'%s' has element equal to %d",
                      "width", 0L),
             call. = FALSE)
    checkmate::assert_flag(na_rm)
    n_measure <- length(measure_var)
    nrow_classif <- nrow(classif_vars)
    ncol_classif <- ncol(classif_vars)
    if (length(measure_var) != nrow(classif_vars))
        stop(gettextf("length of '%s' [%d] not equal to number of rows of '%s' [%d]",
                      "measure_var",
                      n_measure,
                      "classif_vars",
                      nrow_classif),
             call. = FALSE)
    if (!na_rm && anyNA(measure_var))
        stop(gettextf("'%s' contains NAs but '%s' is %s",
                      "measure_var", "na_rm", FALSE),
             call. = FALSE)
    measure_name <- 
        counts_comb <- aggregate(x = rep(1L, nrow_classif),
                                 by = classif_vars,
                                 FUN = unique,
                                 simplify = FALSE)
    classif_vars_ans <- counts_comb[seq_len(ncol_classif)]
    row_nums <- counts_comb[[ncol_classif + 1L]]
    if (nrow(ans) > 1L) {
        n_rep_comb <- vapply(row_nums, length, 0L)
        if (any(n_rep_comb[-1L] != n_rep_comb[[1L]]))
            stop(gettext("some combinations of values in '%s' occur more often than others",
                         "classif_vars"),
                 call. = FALSE)
    }
    m <- measure_var[unlist(row_nums)]
    m <- matrix(m, ncol = n_rep_comb, byrow = TRUE)
    n_width <- length(width)
    width <- sort(width)
    ans <- vector(mode = "list", length = n_width)
    for (i in seq_len(n_width)) {
        w_i <- width[[i]]
        if (i == 1L)
            prob <- c(0.5 * w_i,
                      0.5,
                      1 - 0.5 * w_i)
        else
            prob <- c(0.5 * w_i,
                      1 - 0.5 * w_i)
        vals <- matrixStats::rowQuantiles(m, prob = prob, na.rm = na_rm)
        if (i == 1L) {
            lower_i <- vals[ , 1L]
            median <- vals[ , 2L]
            upper_i <- vals[ , 3L]
        }
        else {
            lower_i <- vals[ , 1L]
            upper_i <- vals[ , 2L]
        }
        ans[[i]] <- cbind(classif_vars_ans,
                          w_i,
                          lower_i,
                          median,
                          upper_i)
    }
    ans <- do.call(rbind, ans)
    names(ans)[ncol_classif + 1:4] <- c(".width",
                                        ".lower",
                                        nm_measure_var,
                                        ".upper")
    ans
}
