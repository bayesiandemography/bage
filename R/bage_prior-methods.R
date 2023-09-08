
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293



## 'draw_vals_hyper' ----------------------------------------------------------

#' Draw values for hyper-parameters
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyper <- function(prior, n_sim) {
  UseMethod("draw_vals_hyper")
}

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_ar1 <- function(prior, n_sim) {
    coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(coef = coef,
         sd = sd)
}

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_known <- function(prior, n_sim)
    list()

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_norm <- function(prior, n_sim) {
    sd <- draw_val_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_normfixed <- function(prior, n_sim)
    list()

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_rw <- function(prior, n_sim)
    sd <- draw_val_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2 <- function(prior, n_sim) {
    sd <- draw_val_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}


## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_spline <- function(prior, n_sim) {
    sd <- draw_val_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## NO_TESTS
#' @export
draw_vals_hyper.bage_prior_svd <- function(prior, n_sim)
    list()


## 'draw_vals_par' ------------------------------------------------------------

## NO_TESTS
#' @export
draw_vals_par <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
  UseMethod("draw_vals_par")
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_ar1 <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    coef <- vals_hyper$coef
    sd <- vals_hyper$sd
    draw_vals_ar1(coef = coef,
                  sd = sd,
                  levels = levels_par)
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_known <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    values <- prior$specific$values
    n_par <- length(levels_par)
    matrix(values,
           nrow = n_par,
           ncol = n_sim,
           dimnames = list(levels_par, seq_len(n_sim)))
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_norm <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    sd <- vals_hyper$sd
    n_par <- length(levels_par)
    n <- n_par * n_sim
    sd <- rep(sd, each = n_par)
    ans <- stats::rnorm(n = n, sd = sd)
    ans <- matrix(ans,
                  nrow = n_par,
                  ncol = n_sim,
                  dimnames = list(levels_par, seq_len(n_sim)))
    ans
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_normfixed <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    sd <- prior$specific$sd
    n_par <- length(levels_par)
    n <- n_par * n_sim
    ans <- stats::rnorm(n = n, sd = sd)
    ans <- matrix(ans,
                  nrow = n_par,
                  ncol = n_sim,
                  dimnames = list(levels_par, seq_len(n_sim)))
    ans
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_rw <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    sd <- vals_hyper$sd
    sd_intercept <- prior$specific$sd_intercept
    draw_vals_rw(sd = sd,
                 sd_intercept = sd_intercept,
                 labels = levels_par)
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_rw2 <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    sd <- vals_hyper$sd
    sd_intercept <- prior$specific$sd_intercept
    sd_slope <- prior$specific$sd_slope
    draw_vals_rw2(sd = sd,
                  sd_intercept = sd_intercept,
                  sd_slope = sd_slope,
                  labels = levels_par)
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_spline <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    sd <- vals_hyper$sd
    sd_intercept <- prior$specific$sd_intercept
    sd_slope <- prior$specific$sd_slope
    m <- make_matrix_parfree_par(prior = prior,
                                 levels_par = levels_par,
                                 agesex = NULL)
    n_par <- ncol(m)
    par <- draw_vals_rw2(sd = sd,
                         sd_intercept = sd_intercept,
                         sd_slope = sd_slope,
                         labels = levels_par)
    m %*% par
}

## NO_TESTS
#' @export
draw_vals_par.bage_prior_svd <- function(prior, vals_hyper, levels_par, agesex, n_sim) {
    scaled_svd <- prior$specific$scaled_svd
    indep <- prior$specific$indep
    n_comp <- prior$specific$n
    m <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                  levels_par = levels_par,
                                  indep = indep,
                                  agesex = agesex,
                                  get_matrix = TRUE,
                                  n_comp = n_comp)
    b <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                  levels_par = levels_par,
                                  indep = indep,
                                  agesex = agesex,
                                  get_matrix = FALSE,
                                  n_comp = n_comp)
    z <- stats::rnorm(n = n_comp * n_sim)
    z <- matrix(nrow = n_par, ncol = n_sim)
    ans <- m %*% z + b
    dimnames(ans) <- list(levels_par, seq_len(n_sim))
    ans    
}                             
    





## 'is_known' -----------------------------------------------------------------

#' Test whether a prior treats an intercept,
#' main effect, or interaction as known and fixed
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_known <- function(prior) {
    UseMethod("is_known")
}

## HAS_TESTS
#' @export
is_known.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
is_known.bage_prior_known <- function(prior) TRUE


## 'is_prior_ok_for_term' -----------------------------------------------------

#' Test whether a prior can be used with
#' a particular effect or interaction
#'
#' @param prior Object of class 'bage_prior'
#' @param nm Name of term.
#' @param length_par Number of elements in term.
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#'
#' @returns TRUE or raises an error
#'
#' @noRd
is_prior_ok_for_term <- function(prior, nm, length_par, agesex) {
    UseMethod("is_prior_ok_for_term")
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ar1 <- function(prior, nm, length_par, agesex) {
    check_is_main_effect(nm = nm,
                         prior = prior)
    check_length_par_gt(length_par = length_par,
                        min = 2L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_known <- function(prior, nm, length_par, agesex) {
    values <- prior$specific$values
    n_values <- length(values)
    if (n_values != length_par) {
        str <- str_call_prior(prior)
        cli::cli_abort(c("{.var {str}} prior for {.var {nm}} term invalid.",
                         i = "Prior specifies {n_values} element{?s}.",
                         i = "{.var {nm}} has {length_par} element{?s}."))
    }
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_norm <- function(prior, nm, length_par, agesex) {
    check_length_par_gt(length_par = length_par,
                        min = 2L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_normfixed <- function(prior, nm, length_par, agesex) {
    check_length_par_gt(length_par = length_par,
                        min = 1L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw <- function(prior, nm, length_par, agesex) {
    check_is_main_effect(nm = nm, prior = prior)
    check_length_par_gt(length_par = length_par,
                        min = 2L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2 <- function(prior, nm, length_par, agesex) {
    check_is_main_effect(nm = nm, prior = prior)
    check_length_par_gt(length_par = length_par,
                        min = 3L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_spline <- function(prior, nm, length_par, agesex) {
    check_is_main_effect(nm = nm, prior = prior)
    check_length_par_gt(length_par = length_par,
                        min = 2L,
                        nm = nm,
                        prior = prior)
    invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd <- function(prior, nm, length_par, agesex) {
    has_agesex <- !is.null(agesex)
    n_dim <- length(strsplit(nm, split = ":")[[1L]])
    str <- str_call_prior(prior)
    msg1 <- "Problem with {.var {str}} prior for {.var {nm}} term."
    ## note that 'agesex' is always "other" if n_dim > 2
    if (!has_agesex && (n_dim == 1L))
        cli::cli_abort(c(msg1,
                         i = paste("Can't use {.var {str}} prior for main effect when age",
                                   "variable not yet identified."),
                         i = paste("Please use function {.fun set_var_age} to",
                                   "identify age variable.")))
    else if (!has_agesex && (n_dim == 2L))
        cli::cli_abort(c(msg1,
                         i = paste("Can't use {.var {str}} prior for interaction when",
                                   "age or sex/gender variable not yet identified."),
                         i = paste("Please use function {.fun set_var_age}",
                                   "or {.fun set_var_sexgender} to identify age",
                                   "or sex/gender variables.")))
    else if (identical(agesex, "other"))
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior can only be used",
                                   "with age main effects or with interactions between",
                                   "age and sex/gender.")))
    else
        check_length_par_gt(length_par = length_par,
                            min = 2L,
                            nm = nm,
                            prior = prior)
    invisible(TRUE)
}


## 'levels_hyper' -------------------------------------------------------------

#' Names of hyper-parameters
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyper <- function(prior) {
    UseMethod("levels_hyper")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_ar1 <- function(prior)
    c("coef", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_known <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_norm <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_normfixed <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2 <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_spline <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd <- function(prior)
    character()


## 'make_matrix_parfree_par' --------------------------------------------------

#' Make matrix mapping parfree to par
#'
#' Make matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param levels_par Vector of labels for term
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_parfree_par <- function(prior, levels_par, agesex) {
    UseMethod("make_matrix_parfree_par")
}

## HAS_TESTS
#' @export
make_matrix_parfree_par.bage_prior <- function(prior, levels_par, agesex) {
    n <- length(levels_par)
    Matrix::.sparseDiagonal(n)
}

## HAS_TESTS
#' @export
make_matrix_parfree_par.bage_prior_spline <- function(prior, levels_par, agesex) {
    n_spline <- prior$specific$n
    if (is.null(n_spline)) {
        n_spline <- 0.7 * length(levels_par)
        n_spline <- ceiling(n_spline)
        n_spline <- max(n_spline, 4L)
    }
    length_par <- length(levels_par)
    matrix <- make_spline_matrix(n_spline = n_spline,
                                 length_par = length_par)
}

## HAS_TESTS
#' @export
make_matrix_parfree_par.bage_prior_svd <- function(prior, levels_par, agesex) {
    scaled_svd <- prior$specific$scaled_svd
    indep <- prior$specific$indep
    n_comp <- prior$specific$n
    matrix <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                       levels_par = levels_par,
                                       indep = indep,
                                       agesex = agesex,
                                       get_matrix = TRUE,
                                       n_comp = n_comp)
}                             

    
## 'make_offset_parfree_par' --------------------------------------------------

#' Make offset used in converting parfree to par
#'
#' Make offset used in converting
#' free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param ... Other arguments
#'
#' @returns A vector.
#'
#' @noRd
make_offset_parfree_par <- function(prior, levels_par, agesex) {
    UseMethod("make_offset_parfree_par")
}

## HAS_TESTS
#' @export
make_offset_parfree_par.bage_prior <- function(prior, levels_par, agesex) {
    n <- length(levels_par)
    rep(0, times = n)
}

## HAS_TESTS
#' @export
make_offset_parfree_par.bage_prior_svd <- function(prior, levels_par, agesex) {
    scaled_svd <- prior$specific$scaled_svd
    indep <- prior$specific$indep
    matrix <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                       levels_par = levels_par,
                                       indep = indep,
                                       agesex = agesex,
                                       get_matrix = FALSE,
                                       n_comp = NULL)
}


## 'uses_matrix_parfree_par' --------------------------------------------------

#' Whether prior uses matrix to convert parfree to par
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_matrix_parfree_par <- function(prior) {
    UseMethod("uses_matrix_parfree_par")
}

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior_svd <- function(prior) TRUE


## 'uses_offset_parfree_par' --------------------------------------------------

#' Whether prior uses offset to convert parfree to par
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_offset_parfree_par <- function(prior) {
    UseMethod("uses_offset_parfree_par")
}

## HAS_TESTS
#' @export
uses_offset_parfree_par.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_offset_parfree_par.bage_prior_svd <- function(prior) TRUE


## 'str_call_prior' -----------------------------------------------------------

#' Create string describing prior
#'
#' Creates string describing prior that
#' (inspired by printing of objects in Python)
#' looks like a call to the constructor function
#'
#' @param prior An object of class "bage_prior"
#'
#' @returns A string
#'
#' @noRd
str_call_prior <- function(prior) {
    UseMethod("str_call_prior")
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_ar1 <- function(prior) {
    min <- prior$specific$min
    max <- prior$specific$max
    scale <- prior$specific$scale
    args <- character(3)
    if (min != 0.8)
        args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
        args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
        args[[3L]] <- sprintf("s=%s", scale)
    args <- args[nzchar(args)]
    args <- paste(args, collapse = ", ")
    sprintf("AR1(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_known <- function(prior) {
    values <- values_known(prior)
    n <- length(values)
    if (n == 1L)
        inner <- sprintf("%s", values)
    else if (n <= 5)
        inner <- sprintf("c(%s)", paste(values, collapse = ","))
    else
        inner <- sprintf("c(%s,...,%s)", values[[1L]], values[[n]])
    sprintf("Known(%s)", inner)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_norm <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "N()"
    else
        sprintf("N(s=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_normfixed <- function(prior) {
    sd <- prior$specific$sd
    if (isTRUE(all.equal(sd, 1)))
        "NFix()"
    else
        sprintf("NFix(sd=%s)", sd)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW()"
    else
        sprintf("RW(s=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2 <- function(prior) {
    scale <- prior$specific$scale
    flat <- prior$const[[3L]] < 1
    if (isTRUE(all.equal(scale, 1))) {
        if (flat)
            "RW2(flat=TRUE)"
        else
            "RW2()"
    }
    else {
        if (flat)
            sprintf("RW2(s=%s,flat=TRUE)", scale)
        else
            sprintf("RW2(s=%s)", scale)
    }
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_spline <- function(prior) {
    n <- prior$specific$n
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1))) {
        if (is.null(n))
            "Spline()"
        else
            sprintf("Spline(n=%d)", n)
    }
    else {
        if (is.null(n))
            sprintf("Spline(s=%s)", scale)
        else
            sprintf("Spline(n=%d,s=%s)", n, scale)
    }
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd <- function(prior) {
    nm_scaled_svd <- prior$specific$nm_scaled_svd
    n <- prior$specific$n
    indep <- prior$specific$indep
    if (isTRUE(all.equal(n, 5L))) {
        if (isTRUE(indep))
            sprintf("SVD(%s)", nm_scaled_svd)
        else
            sprintf("SVD(%s,indep=FALSE)", nm_scaled_svd)
    }
    else {
        if (isTRUE(indep))
            sprintf("SVD(%s,n=%d)", nm_scaled_svd, n)
        else
            sprintf("SVD(%s,n=%d,indep=FALSE)", nm_scaled_svd, n)
    }
}


## 'transform_hyper' ----------------------------------------------------------

#' Transform to convert working TMB version
#' of parameter back to original units
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A list of functions.
#'
#' @noRd
transform_hyper <- function(prior) {
    UseMethod("transform_hyper")
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ar1 <- function(prior)
    list(function(x) ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (exp(x) + 1)),
         exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_known <- function(prior)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_norm <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_normfixed <- function(prior)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2 <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_spline <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd <- function(prior)
    list()


## 'uses_matrix_parfree_par' --------------------------------------------------

#' Whether prior uses matrix to transform parfree
#' to par
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE
#'
#' @noRd
uses_matrix_parfree_par <- function(prior) {
    UseMethod("uses_matrix_parfree_par")
}

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_parfree_par.bage_prior_svd <- function(prior) TRUE


## 'uses_offset_parfree_par' --------------------------------------------------

#' Whether prior uses offset to transform parfree
#' to par
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE
#'
#' @noRd
uses_offset_parfree_par <- function(prior) {
    UseMethod("uses_offset_parfree_par")
}

## HAS_TESTS
#' @export
uses_offset_parfree_par.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_offset_parfree_par.bage_prior_svd <- function(prior) TRUE


## 'values_known' -------------------------------------------------------------

#' Given a prior that treats a term as known,
#' extract the known values
#'
#' @param An object of class 'bage_prior'
#'
#' @returns A vector of doubles.
#'
#' @noRd
values_known <- function(prior) {
    UseMethod("values_known")
}

## HAS_TESTS
#' @export
values_known.bage_prior_known <- function(prior) prior$specific$values


