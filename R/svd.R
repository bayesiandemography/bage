
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

    
## HAS_TESTS    
#' Given a matrix of rates, calculate
#' transform using SVD
#'
#' @param x Matrix of rates,
#' probabilities, or means
#' @param n Number of components
#'
#' @returns A named list with two
#' elements: matrix called 'transform',
#' and a vector called 'translate'.
#'
#' @noRd
scaled_svd <- function(x, n) {
    svd <- svd(x = x,
               nu = n,
               nv = n)
    U <- svd$u
    D <- diag(svd$d[seq_len(n)])
    V <- svd$v
    m <- colMeans(V)
    S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
    transform <- U %*% D %*% S
    translate <- as.numeric(U %*% D %*% m)
    dn <- dimnames(x)
    if (!is.null(dn[[1L]])) {
        dimnames(transform) <- c(dn[1L], list(component = seq_len(n)))
        names(translate) <- dn[[1L]]
    }
    list(transform = transform,
         translate = translate)
}



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
#' Data frame `x` must have
#' - a column named `"age`"
#' - a column named `"sex"` or `"gender"`
#' - one or more columns containing additional
#' classifying variables
#' - a column called `"value"` holding the rates,
#' probabilities, or means.
#'
#' The number of components used by the SVD
#' is governed by argument `n`, which defaults
#' to 5.
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
#' By default, `svd_transform` creates a
#' transformation for each level of the sex/gender
#' variable. If a value is supplied for the
#' `concat` argument, then `svd_transform`
#' also creates a transformation that concatenates
#' age levels for each sex/gender included
#' in the value supplied for `concat`. See below for
#' an example. Concatenated transforms are useful,
#' for instance, when jointly modelling female
#' and male rates.
#'
#' @param x A data frame with rates, probabilities,
#' or means classified by age, sex/gender, and other
#' variables.
#' @param n Number of components of SVD.
#' @param scale `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param concat Levels of the sex/gender
#' variable to use for a `"concat"`
#' transformation.
#'
#' @returns A named list, with one element
#' for each level of the sex/gender variable,
#' plus one more element if a value for
#' `concat` is supplied.
#' Each element in the return value
#' is itself a named list,
#' holding a matrix called `"transform"`
#' and a vector called `"translate"`.
#'
#' @examples
#' set.seed(0)
#' x <- expand.grid(age = 0:10,
#'                  gender = c("Female", "Male", "Total"),
#'                  year = 2020:2021,
#'                  country = LETTERS)
#' x$value <- rgamma(n = nrow(x), shape = 0.2)
#'
#' ## "Female", "Male", and "Total"
#' tr <- svd_transform(x)
#' str(tr)
#'
#' ## "Female", "Male", "Total", and "concat"
#' tr_concat <- svd_transform(x, concat = c("Female", "Male"))
#' str(tr_concat)
#' @export
svd_transform <- function(x,
                          n = 5,
                          scale = c("log", "logit", "none"),
                          concat = NULL) {
    ## check inputs and rearrange variables in 'x'
    checkmate::assert_data_frame(x,
                                 any.missing = FALSE,
                                 min.cols = 4L)
    nms_x <- names(x)
    i_age <- grep("age", nms_x, ignore.case = TRUE)
    i_sexgender <- grep("sex|gender", nms_x, ignore.case = TRUE)
    i_value <- grep("value", nms_x, ignore.case = TRUE)
    if (length(i_age) == 0L)
        stop(gettextf("'%s' does not have an \"%s\" variable",
                      "x", "age"),
             call. = FALSE)
    if (length(i_sexgender) == 0L)
        stop(gettextf("'%s' does not have a \"%s\" variable or a \"%s\" variable",
                      "x", "sex", "gender"),
             call. = FALSE)
    if (length(i_sexgender) == 2L)
        stop(gettextf("'%s' has a sex \"%s\" and a \"%s\" variable",
                      "x", "sex", "gender"),
             call. = FALSE)
    if (length(i_value) == 0L)
        stop(gettextf("'%s' does not have a \"%s\" variable",
                      "x", "value"),
             call. = FALSE)
    value <- x$value
    checkmate::assert_numeric(value,
                              finite = TRUE,
                              any.missing = FALSE)
    i_classif <- setdiff(seq_along(x), c(i_age, i_sexgender, i_value))
    classif <- do.call(paste, x[i_classif])
    x <- cbind(x[c(i_sexgender, i_age)], classif, x[i_value])
    is_dup <- duplicated(x[1:3])
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L)
        stop(gettextf("'%s' has a duplicate combination of the classifying variables : %s",
                      "x",
                      paste(x[1:3], collapse = " ")),
             call. = FALSE)
    levels <- lapply(x[1:3], unique)
    classif_expected <- do.call(paste, expand.grid(levels))
    classif_actual <- do.call(paste, x[1:3])
    is_cl_found <- classif_expected %in% classif_actual
    i_cl_not_found <- match(FALSE, is_cl_found, nomatch = 0L)
    if (i_cl_not_found > 0L)
        stop(gettextf("'%s' is missing a combination of the classifying variables : %s",
                      "x",
                      classif_expected[[i_cl_not_found]]),
             call. = FALSE)
    n <- checkmate::assert_count(n,
                                 positive = TRUE,
                                 coerce = TRUE)
    n_age <- length(unique(x[[2L]]))
    if (n > n_age)
        stop(gettextf("value for '%s' [%d] greater than number of distinct age groups [%d]",
                      "n",
                      n,
                      n_age),
             .call = FALSE)
    scale <- match.arg(scale)
    levels_sexgender <- unique(x[[1L]])
    has_concat <- length(concat) > 0L
    if (has_concat) {
        checkmate::assert_character(concat,
                                    min.chars = 1L,
                                    any.missing = FALSE,
                                    unique = TRUE)
        is_con_found <- concat %in% levels_sexgender
        i_con_not_found <- match(FALSE, is_con_found, nomatch = 0L)
        if (i_con_not_found > 0L)
            stop(gettextf("value \"%s\" in '%s' not found in variable '%s' : valid values are %s",
                          concat[[i_con_not_found]],
                          "concat",
                          nms_x[[i_sexgender]],
                          paste(sprintf("\"%s\"", levels_sexgender), collapse = ", ")),
                 call. = FALSE)
    }
    if (scale %in% c("log", "logit")) {
        if (any(x$value < 0))
            stop(gettextf("'%s' is \"%s\" but '%s' has negative values",
                          "scale",
                          scale,
                          "value"),
                 call. = FALSE)
    }
    if (scale  == "logit") {
        if (any(x$value > 1))
            stop(gettextf("'%s' is \"%s\" but '%s' has values greater than 1",
                          "scale",
                          scale,
                          "value"),
                 call. = FALSE)
    }
    ## create matrices from the data
    x <- x[order(x[[1L]], x[[2L]]), , drop = FALSE]
    make_ans_one_sexgender <- function(level_sexgender) {
        x_tmp <- x[x[[1L]] == level_sexgender, , drop = FALSE]
        x_tmp <- droplevels(x_tmp)
        tapply(x_tmp[[4L]], x_tmp[2:3], sum)
    }
    ans <- lapply(levels_sexgender, make_ans_one_sexgender)
    names(ans) <- levels_sexgender
    if (has_concat) {
        x_tmp <- x[x[[1L]] %in% concat, , drop = FALSE]
        x_tmp <- droplevels(x_tmp)
        x_tmp <- x[order(match(x_tmp[[1L]], concat)), , drop = FALSE]
        x_tmp[[2L]] <- paste(x_tmp[[1L]], x_tmp[[2L]], sep = ".")
        ans_concat <- tapply(x_tmp[[4L]], x_tmp[2:3], sum)
        ans <- c(ans, list(concat = ans_concat))
    }
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
