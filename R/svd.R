
## HAS_TESTS
#' Replace zeros in a matrix of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace zeros in `x`, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that `x` is a valid numeric
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



#' Construct transformations based on a SVD
#' of age-specific rates
#'
#' Apply the [Singular Value Decomposition][base::svd()]
#' (SVD) to a dataset of age-specific rates,
#' probabilities, or means, and use the results
#' to construct transformations that allow
#' age profiles to be represented
#' parsimoniously. The transformations have
#' two components: a matrix called `transform`,
#' and a vector called `translate`.
#'
#' Data frame `x` must have
#' - an age variable
#' - at least one non-age classification variable,
#' used to distinguish age profiles,
#' eg country or time
#' - a measurement variable holding the rates,
#' probabilities, or means.
#' `x` can also have a classification
#' variable that appears in the resultswill appear in the outcomes,
#' typically sex or gender.
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
#' When a log or logit scale is used, `svd_transform()`
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
#' transformation for each level of the `by`
#' variable. If a value is supplied for the
#' `concat` argument, then `svd_transform`
#' also creates a transformation that concatenates
#' age categories for each level of the `by` variable
#' included in `concat`. For instance, if the `by` variable
#' is sex, and `concat` is `c("Female", "Male")`,
#' then `svd_transform()` creates a transform
#' based on all combinations of age and sex, which
#' can be useful for modelling the joint distribution
#' of female and male rates.
#'
#' @param x A data frame with rates, probabilities,
#' or means classified by age, sex/gender, and other
#' variables.
#' @param age Name of the age variable in `x`.
#' If no value supplied, then `svd_transform()`
#' takes a guess.
#' @param `by` Name of additional classification
#' variable to be included in outputs. Optional.
#' @param measure Name of the measurement variable.
#' @param n Number of components of SVD. Defaults
#' to 5.
#' @param scale `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param concat Levels of the `by`
#' variable to use for a `"concat"`
#' transformation.
#'
#' @returns A [tibble][tibble::tibble-package],
#' the `transform` and `translate` columns of which
#' are list columns containing matrices.
#'
#' @examples
#' set.seed(0)
#' x <- expand.grid(age = 0:10,
#'                  sex = c("Female", "Male", "Total"),
#'                  year = 2020:2021,
#'                  country = LETTERS)
#' x$value <- rgamma(n = nrow(x), shape = 0.2)
#'
#' ## "Female", "Male", and "Total"
#' ans <- svd_transform(x)
#' ans
#' ans$transform[[1]]
#'
## #' ## "Female", "Male", "Total", and "concat"
## #' ans_concat <- svd_transform(x, concat = c("Female", "Male"))
## #' ans_concat
## #' ans_concat$transform[[1]]
## #' @export
## svd_transform <- function(x,
##                           age = NULL,
##                           by = NULL,
##                           measure,
##                           n = 5,
##                           scale = c("log", "logit", "none"),
##                           concat = NULL) {
##     ## check 'x'
##     checkmate::assert_data_frame(x,
##                                  any.missing = FALSE,
##                                  min.cols = 3L)
##     nms_x <- names(x)
##     ## check 'measure'
##     checkmate::assert_string(measure, min.chars = 1L)
##     i_measure <- match(measure, nms_x, nomatch = 0L)
##     if (i_measure == 0L)
##         stop(gettextf("could not find variable \"%s\" in '%s'",
##                       measure,
##                       "x"),
##              call. = FALSE)
##     ## check 'by'
##     if (is.null(by))
##         i_by <- NULL
##     else {
##         checkmate::assert_string(by, min.chars = 1L)
##         if (identical(by, measure))
##             stop(gettextf("'%s' and '%s' have same value [\"%s\"]",
##                           "by",
##                           "meausure",
##                           by),
##                  call. = FALSE)
##         i_by <- match(by, nms_x, nomatch = 0L)
##         if (i_by == 0L)
##             stop(gettextf("could not find variable \"%s\" in '%s'",
##                           by,
##                           "x")
##                  call. = FALSE)
##     }
##     ## check 'age'
##     if (is.null(age)) {
##         age <- find_age(nms_x[-c(i_measure, i_by)])
##         if (is.null(age))
##             stop(gettextf("could not find %s variable in '%s'",
##                           "age",
##                           "x"),
##                  call. = FALSE)
##     }
##     else {
##         checkmate::assert_string(age, min.chars = 1L)
##         if (identical(age, measure))
##             stop(gettextf("'%s' and '%s' have same value [\"%s\"]",
##                           "age",
##                           "measure",
##                           measure),
##                  call. = FALSE)
##         if (!is.null(by) && identical(age, by))
##             stop(gettextf("'%s' and '%s' have same value [\"%s\"]",
##                           "age",
##                           "by",
##                           by),
##                  call. = FALSE)
##         i_age <- match(age, nms_x, nomatch = 0L)
##         if (i_age == 0L)
##             stop(gettextf("could not find variable \"%s\" in '%s'",
##                           age,
##                           "x")
##                  call. = FALSE)
##     }
##     ## check measure variable
##     checkmate::assert_numeric(x[[i_measure]],
##                               finite = TRUE,
##                               any.missing = FALSE,
##                               .var.name = "measure")
##     ## check 'n'
##     n <- checkmate::assert_count(n,
##                                  positive = TRUE,
##                                  coerce = TRUE)
##     n_age <- length(unique(x[[age]]))
##     if (n > n_age)
##         stop(gettextf("value for '%s' [%d] greater than number of distinct age groups [%d]",
##                       "n",
##                       n,
##                       n_age),
##              .call = FALSE)
##     ## check 'scale'
##     scale <- match.arg(scale)
##     if (scale %in% c("log", "logit")) {
##         if (any(x[[i_measure]] < 0))
##             stop(gettextf("'%s' is \"%s\" but '%s' has negative values",
##                           "scale",
##                           scale,
##                           measure),
##                  call. = FALSE)
##     }
##     if (scale  == "logit") {
##         if (any(x[[i_measure]] > 1))
##             stop(gettextf("'%s' is \"%s\" but '%s' has values greater than 1",
##                           "scale",
##                           scale,
##                           measure),
##                  call. = FALSE)
##     }
##     ## check remaining classification variables
##     i_classif <- setdiff(seq_along(x), c(i_age, i_by, i_measure))
##     classif <- do.call(paste, x[i_classif])
##     classif_vars <- x[c(i_by, i_age), classif]
##     is_dup <- duplicated(classif_vars)
##     i_dup <- match(TRUE, is_dup, nomatch = 0L)
##     if (i_dup > 0L)
##         stop(gettextf("'%s' has a duplicate combination of the classifying variables : %s",
##                       "x",
##                       paste(classif_vars[i_dup, ], collapse = ", ")),
##              call. = FALSE)
##     levels <- lapply(classif_vars, unique)
##     classif_expected <- do.call(paste, expand.grid(levels))
##     classif_actual <- do.call(paste, classif_vars)
##     is_cl_found <- classif_expected %in% classif_actual
##     i_cl_not_found <- match(FALSE, is_cl_found, nomatch = 0L)
##     if (i_cl_not_found > 0L)
##         stop(gettextf("'%s' is missing a combination of the classifying variables : %s",
##                       "x",
##                       classif_expected[[i_cl_not_found]]),
##              call. = FALSE)
##     ## check 'concat'
##     has_concat <- length(concat) > 0L
##     if (has_concat) {
##         checkmate::assert_character(concat,
##                                     min.chars = 1L,
##                                     any.missing = FALSE,
##                                     unique = TRUE)
##         if (is.null(by))
##             stop(gettextf("value supplied for '%s' but not for '%s'",
##                           "concat",
##                           "by"),
##                  call. = FALSE)
##         is_con_found <- concat %in% x[[i_by]]
##         i_con_not_found <- match(FALSE, is_con_found, nomatch = 0L)
##         if (i_con_not_found > 0L)
##             stop(gettextf("value \"%s\" in '%s' not found in variable '%s' : valid values are %s",
##                           concat[[i_con_not_found]],
##                           "concat",
##                           nms_x[[i_by]],
##                           paste(sprintf("\"%s\"", unique(x[[i_by]]), collapse = ", "))),
##                  call. = FALSE)
##     }
##     ## create array from the data
##     if (!is.factor(x[[i_age]]))
##         x[[i_age]] <- factor(x[[i_age]], levels = unique(x[[i_age]]))
##     if (is.null(by))
##         formula <- as.formula(sprintf("%s ~ %s + %s", measure, age, classif))
##     else {
##         if (has_concat) {
##             x_tmp <- x[x[[i_by]] %in% concat, , drop = FALSE]
##             x_tmp <- droplevels(x_tmp)
##             x_tmp <- x_tmp[order(match(x_tmp[[i_by]], concat)), , drop = FALSE]
##             x_tmp[[i_by]] <- paste(x_tmp[[i_by]], x_tmp[[i_age]], sep = ".")
##             x <- rbind(x, x_tmp)
##         }
##         formula <- as.formula(sprintf("%s ~ %s + %s + %s", measure, by, age, classif))
##     }
##     a <- xtabs(formula, data = x)
##     ## turn into tibble
##     if (is.null(by))
##         ans <- tibble(a = list(a))
##     else {
##         ans <- tibble(by = unique(x[[i_by]]),
##                       a = apply(a, 1L, function(x) x, simplify = FALSE))
##         names(ans)[[1L]] <- by
##     }
##     ## transform to log or logit scale if necessary
##     if (scale == "log") {
##         ans$a <- lapply(ans$a, replace_zeros)
##         ans$a <- lapply(ans$a, log)
##     }
##     if (scale == "logit") {
##         logit <- function(x) log(1 / (1 - x))
##         ans$a <- lapply(ans$a, replace_zeros_ones)
##         ans$a <- lapply(ans$a, logit)
##     }
##     ## apply svd
##     ans$svd <- lapply(ans$a, scaled_svd, n = n)
##     ## tidy up
##     ans$transform <- lapply(ans$svd, function(x) x$transform)
##     ans$translate <- lapply(ans$svd, function(x) x$translate)
##     ans <- ans[-match("svd", names(ans))]
##     ## return
##     ans
## }
