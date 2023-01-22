
#' Construct transformations based on SVD
#' of age-sex-specific rates
#'
#' Apply the [Singular Value Decomposition][base::svd()]
#' (SVD) to a dataset of ages-sex-specific rates,
#' probabilities, or means, and use the results
#' to construct transformations that allow
#' age-sex profiles to be represented more
#' parsimoniously.
#'
#' Data frame `x` must have columns named
#' `"age`", `"sex"`, plus at least one
#' more column of classification variables.
#' It must also have a column called `"value"`
#' holding the rates, probabilities, or means.
#'
#' The number of components used by the SVD
#' is governed by argument `n`. 
#'
#' Rates are typically transformed to the
#' log scale, and probabilities to the logit scale,
#' before the SVD is applied. The type of
#' transformation is specified through the
#' `scale` argument (which defaults to `"log"`.)
#'
#' When a log or logit scale is used, `svd_transform`
#' converts zeros in the `value` column to
#' values just above zero before transforming.
#' When a logit scale is used, `svd_transform`
#' also converts ones to values just
#' below one. The substitute
#' values are calculated by fitting a main-effects
#' model to the data, and then shifting the predicted
#' value down slightly (in the case of zeros),
#' or up slightly (in the case of ones.)
#'
#' @param x A data frame with rates, probabilities,
#' or means classified by age, sex, and other
#' variables.
#' @param n Number of components of SVD.
#' @param scale `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#'
#' @returns A named list, each element of which
#' consists of a vector and a matrix.
#'
#' @export
svd_transform <- function(x, n, scale = c("log", "logit", "none")) {
    ## check inputs
    checkmate::assert_data_frame(x,
                                 any.missing = FALSE,
                                 min.cols = 4L)
    nms_x <- names(x)
    nms_req <- c("age", "sex", "value")
    is_nm_found <- nms_req %in% nms_x
    i_nm_not_found <- match(FALSE, is_nm_found, nomatch = 0L)
    if (i_nm_not_found > 0L)
        stop(gettextf("'%s' does not have a variable called \"%s\"",
                      "x",
                      nms_req[[i_nm_not_found]]),
             call. = FALSE)
    value <- x$value
    checkmate::assert_numeric(value,
                              lower = 0,
                              finite = TRUE,
                              any.missing = FALSE)
    nms_classif <- setdiff(nms, c("age", "sex", "value"))
    x$classif <- do.call(x[nms_classif], paste)
    x <- x[c("sex", "age", "classif", "value")]
    is_dup <- duplicated(x[c("age", "sex", "classif")])
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L)
        stop(gettextf("'%s' has duplicate combination of classification variables : %s",
                      "x",
                      paste(x[c("age", "sex", "classif")], collapse = " ")),
             call. = FALSE)
    levels <- lapply(x[c("sex", "age", "classif")], unique)
    classif_expected <- do.call(paste, expand.grid(levels))
    classif_actual <- do.call(paste, x[c("sex", "age", "classif")])
    is_cl_found <- classif_expected %in% classif_actual
    i_cl_not_found <- match(FALSE, is_cl_found, nomatch = 0L)
    if (i_cl_not_found > 0L)
        stop(gettextf("'%s' missing combination of classification variables : %s",
                      "x",
                      classif_expected[[i_cl_not_found]]),
             call. = FALSE)
    n <- checkmate::assert_count(n,
                                 positive = TRUE,
                                 coerce = TRUE)
    scale <- match.arg(scale)
    ## create matrices from the data
    x <- x[order(x$sex, x$age), ]
    x$sex_age <- paste(x$sex, x$age)
    ans_concat <- xtabs(value ~ sex_age + classif, data = x)
    make_ans_one_sex <- function(x)
        xtabs(value ~ age, data = x, subset = sex == s)
    ans_single <- lapply(unique(x$sex), make_ans_one_sex)
    ans <- c(list(Concat = ans_concat), ans_single)
    coerce_to_matrix <- function(m)
        matrix(m, dim = dim(m), dimnames = dimnames(m)) 
    ans <- lapply(ans, coerce_to_matrix)
    ## transform to log or logit scale if necessary
    if (scale == "log") {
        ans <- lapply(ans, replace_zeros)
        ans <- lapply(ans, log)
    }
    if (scale == "logit") {
        logit <- function(x) log(1 / (1 - x))
        ans <- lapply(ans, replace_zeros_ones)
        ans <- lapply(ans, logit)
    }
    ## apply svd
    ans <- lapply(ans, scaled_svd, n = n)
    ## return
    ans
}


## HAS_TESTS
#' Prepare a matrix of rates for 
#' a singular value decomposition
#'
#' Prepare a matrix of rates, \code{x},
#' for a singular value decomposition,
#' typically via function \code{\link{scaled_svd}}.
#' The preparation consists of
#' valdity checking, replacing any
#' zeros with small modelled values,
#' and then taking logs.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version
#' of \code{x}.
#'
#' @seealso \code{\link{scaled_svd}},
#' \code{\link{prepare_svd_probs}}
#' @export
prepare_svd_rates <- function(x) {
    checkmate::assert_matrix(x,
                             min.rows = 1L,
                             min.cols = 1L)
    checkmate::assert_numeric(x,
                              lower = 0,
                              finite = TRUE,
                              any.missing = FALSE)
    x <- replace_zeros(x)
    log(x)
}


## HAS_TESTS
#' Prepare a matrix of probabilities for 
#' a singular value decomposition
#'
#' Prepare a matrix of probabilities or
#' proportions, \code{x},
#' for a singular value decomposition,
#' typically via function \code{\link{scaled_svd}}.
#' The preparation consists of
#' valdity checking, replacing any
#' zeros and ones with modelled values,
#' and then transforming to a logit scale.
#'
#' @param x A matrix of probabilities
#' or proportions.
#'
#' @return A modified version
#' of \code{x}.
#'
#' @seealso \code{\link{scaled_svd}},
#' \code{\link{prepare_svd_probs}}
#' @export
prepare_svd_probs <- function(x) {
    checkmate::assert_matrix(x,
                             min.rows = 1L,
                             min.cols = 1L)
    checkmate::assert_numeric(x,
                              lower = 0,
                              upper = 1,
                              any.missing = FALSE)
    x <- replace_zeros_ones(x)
    log(x / (1 - x))
}


#' Given a matrix of rates, calculate
#' transform using SVD
#'
#' @param x Matrix of rates,
#' probabilities, or means
#' @param n Number of components
#'
#' @returns A named list with two
#' elements: a vector called 'translate',
#' and a matrix called 'transform'.
#'
#' @noRd
scaled_svd <- function(x, n) {
    svd <- svd(x = x,
               nu = n,
               nv = n)
    U <- svd$u
    s <- seq_len(n)
    D <- diag(svd$d[s])
    V <- svd$v
    mean_V <- colMeans(V)
    translate <- as.numeric(U %*% D %*% mean_V)
    sd_V <- apply(V, MARGIN = 2L, FUN = stats::sd)
    sd_V <- diag(sd_V)
    transform <- U %*% D %*% sd_V
    list(translate = translate,
         transform = transform)
}
    

## HAS_TESTS
#' Replace zeros in a matrix 'x' of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace zeros in \code{x}, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no infinite values.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros <- function(x) {
    is_zero <- x == 0
    if (any(is_zero)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
    }
    x
}

## HAS_TESTS
#' Replace zeros and ones in a matrix 'x' of
#' estimated probabilities
#'
#' Based on a simple main effects model,
#' replace zeros and ones in \code{x},
#' a matrix of estimated probabilities.
#' The replacement values should typically be
#' near 0 or 1.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no values abouve one.
#'
#' @param x A matrix of probabilities.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros_ones <- function(x) {
    is_zero <- x == 0
    is_one <- x == 1
    if (any(is_zero) || any(is_one)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
        x[is_one] <- 0.5 + 0.5 * predicted[is_one]
    }
    x
}

    
    
