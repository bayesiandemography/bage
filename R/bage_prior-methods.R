
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'draw_vals_effect' ------------------------------------------------------------

#' Draw Values for Main Effect or Interactions
#'
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list with values of ordinary hyper-parameters
#' @param vals_hyperrand Named list with values of hyper-parameters
#' that can be treated as random effects
#' @param levels_effect Character vector with labels for effect
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param n_sim Number of draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_effect <- function(prior,
                             vals_hyper,
                             vals_hyperrand,
                             levels_effect,
                             agesex,
                             matrix_along_by,
                             n_sim) {
  UseMethod("draw_vals_effect")
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_ar <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           levels_effect,
                                           agesex,
                                           matrix_along_by,
                                           n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  n <- length(levels_effect)
  ans <- draw_vals_ar(n = n, coef = coef, sd = sd)
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans
}

## NO_TESTS
#' @export
draw_vals_effect.bage_prior_compose <- function(prior,
                                                vals_hyper,
                                                vals_hyperrand,
                                                levels_effect,
                                                agesex,
                                                matrix_along_by,
                                                n_sim) {
  priors <- prior$specific$priors
  ans <- .mapply(draw_vals_effect,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             vals_hyperrand = vals_hyperrand),
                 MoreArgs = list(levels_effect = levels_effect,
                                 agesex = agesex,
                                 matrix_along_by = matrix_along_by))
  Reduce("+", ans)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_iar <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           levels_effect,
                                           agesex,
                                           matrix_along_by,
                                           n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  s <- rep(seq_len(n_sim), each = n_by)
  coef <- coef[, s]
  sd <- sd[s]
  ans <- draw_vals_ar(n = n_along, coef = coef, sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_ilin <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             agesex,
                                             matrix_along_by,
                                             n_sim) {
  mslope <- vals_hyperrand$mslope
  sd <- vals_hyper$sd
  draw_vals_ilin(mslope = mslope,
                 sd = sd,
                 matrix_along_by = matrix_along_by,
                 labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_iseas <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              levels_effect,
                                              agesex,
                                              matrix_along_by,
                                              n_sim) {
  n <- prior$specific$n
  sd <- vals_hyper$sd
  draw_vals_iseas(n = n,
                  sd = sd,
                  matrix_along_by = matrix_along_by,
                  labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_known <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              levels_effect,
                                              agesex,
                                              matrix_along_by,
                                              n_sim) {
  values <- prior$specific$values
  n_effect <- length(levels_effect)
  matrix(values,
         nrow = n_effect,
         ncol = n_sim,
         dimnames = list(levels_effect, seq_len(n_sim)))
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_lin <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            agesex,
                                            matrix_along_by,
                                            n_sim) {
  slope <- vals_hyper$slope
  sd <- vals_hyper$sd
  draw_vals_lin(slope = slope,
                sd = sd,
                labels = levels_effect)
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_norm <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             agesex,
                                             matrix_along_by,
                                             n_sim) {
  sd <- vals_hyper$sd
  n_effect <- length(levels_effect)
  n <- n_effect * n_sim
  sd <- rep(sd, each = n_effect)
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans,
                nrow = n_effect,
                ncol = n_sim,
                dimnames = list(levels_effect, seq_len(n_sim)))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_normfixed <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  levels_effect,
                                                  agesex,
                                                  matrix_along_by,
                                                  n_sim) {
  sd <- prior$specific$sd
  n_effect <- length(levels_effect)
  n <- n_effect * n_sim
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans,
                nrow = n_effect,
                ncol = n_sim,
                dimnames = list(levels_effect, seq_len(n_sim)))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           levels_effect,
                                           agesex,
                                           matrix_along_by,
                                           n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw(sd = sd,
               labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2 <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            agesex,
                                            matrix_along_by,
                                            n_sim) {
  sd <- vals_hyper$sd
  sd_slope <- prior$specific$sd_slope
  draw_vals_rw2(sd = sd,
                sd_slope = sd_slope,
                labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_seas <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             agesex,
                                             matrix_along_by,
                                             n_sim) {
  n <- prior$specific$n
  sd <- vals_hyper$sd
  draw_vals_seas(n = n,
                 sd = sd,
                 labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_spline <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               levels_effect,
                                               agesex,
                                               matrix_along_by,
                                               n_sim) {
  sd <- vals_hyper$sd
  sd_slope <- prior$specific$sd_slope
  m <- make_matrix_effectfree_effect(prior = prior,
                                     levels_effect = levels_effect,
                                     agesex = NULL)
  labels <- seq_len(ncol(m))
  effect <- draw_vals_rw2(sd = sd,
                          sd_slope = sd_slope,
                          labels = labels)
  m %*% effect
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            agesex,
                                            matrix_along_by,
                                            n_sim) {
  scaled_svd <- prior$specific$scaled_svd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n
  m <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                levels_effect = levels_effect,
                                indep = indep,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  b <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                levels_effect = levels_effect,
                                indep = indep,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  n_par <- ncol(m)
  z <- stats::rnorm(n = n_par * n_sim)
  z <- matrix(nrow = n_par, ncol = n_sim)
  ans <- m %*% z + b
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans    
}                             


## 'draw_vals_hyper' ----------------------------------------------------------

#' Draw values for hyper-parameters
#'
#' @param prior Object of class 'bage_prior'
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyper <- function(prior, n_sim) {
  UseMethod("draw_vals_hyper")
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_ar <- function(prior, n_sim) {
    coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(coef = coef,
         sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_iar <- function(prior, n_sim) {
    coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(coef = coef,
         sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_ilin <- function(prior, n_sim) {
  slope <- draw_vals_slope(prior = prior,
                           n_sim = n_sim)
  msd <- draw_vals_msd(prior = prior,
                       n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(slope = slope,
       sd = sd,
       msd = msd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_iseas <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_known <- function(prior, n_sim)
    list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_lin <- function(prior, n_sim) {
  slope <- draw_vals_slope(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(slope = slope,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_norm <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_normfixed <- function(prior, n_sim)
    list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2 <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_seas <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_spline <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd <- function(prior, n_sim)
    list()


## 'draw_vals_hyperrand' ------------------------------------------------------

#' Draw Values for Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyperrand <- function(prior, vals_hyper, matrix_along_by, n_sim) {
  UseMethod("draw_vals_hyperrand")
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior <- function(prior, vals_hyper, matrix_along_by, n_sim) {
  list()
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_ilin <- function(prior, vals_hyper, matrix_along_by, n_sim) {
  slope <- vals_hyper$slope
  msd <- vals_hyper$msd
  mslope <- draw_vals_mslope(slope = slope,
                             msd = msd,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  list(mslope = mslope)
}


## 'has_hyperrand' ------------------------------------------------------

#' Has Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
has_hyperrand <- function(prior) {
  UseMethod("has_hyperrand")
}

## HAS_TESTS
#' @export
has_hyperrand.bage_prior <- function(prior) FALSE


## HAS_TESTS
#' @export
has_hyperrand.bage_prior_compose <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_ilin <- function(prior) TRUE


## 'indices_priors' ----------------------------------------------------------------

#' Information on Priors Making Up Compose Prior
#'
#' Creates index vector describing priors
#' making up an object of class 'bage_prior_compose'.
#'
#' @param prior An object of class "bage_prior"
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#'
#' @returns An integer vector
#'
#' @noRd
indices_priors <- function(prior, matrix_along_by) {
    UseMethod("indices_priors")
}

## HAS_TESTS
#' @export
indices_priors.bage_prior <- function(prior, matrix_along_by) {
  integer()
}

## HAS_TESTS
#' @export
indices_priors.bage_prior_compose <- function(prior, matrix_along_by) {
  priors <- prior$specific$priors
  n_prior <- length(priors)
  n_effect <- length(matrix_along_by)
  levels_hyper <- lapply(priors, levels_hyper)
  lengths_hyper <- lengths(levels_hyper)
  levels_hyperrand <- lapply(priors, levels_hyperrand)
  lengths_hyperrand <- lengths(levels_hyperrand)
  lengths_hyperrand[-n_prior] <- lengths_hyperrand[-n_prior] + n_effect
  consts <- lapply(priors, function(x) x$const)
  lengths_consts <- lengths(consts)
  ans <- integer()
  hyper_start <- 0L
  hyperrand_start <- 0L
  consts_start <- 0L
  for (i_prior in seq_len(n_prior)) {
    hyper_length <- lengths_hyper[[i_prior]]
    hyperrand_length <- lengths_hyperrand[[i_prior]]
    consts_length <- lengths_consts[[i_prior]]
    i_prior_index = priors[[i_prior]]$i_prior
    ans <- c(ans,
             hyper_start = hyper_start,
             hyper_length = hyper_length,
             hyperrand_start = hyperrand_start,
             hyperrand_length = hyperrand_length,
             consts_start = consts_start,
             consts_length = consts_length,
             i_prior = i_prior_index)
    hyper_start <- hyper_start + hyper_length
    hyperrand_start <- hyperrand_start + hyperrand_length
    consts_start <- consts_start + consts_length
  }
  ans
}


## 'is_comparable_prior' ------------------------------------------------------

#' Test Whether Priors can be Meaningfully Compared
#' in a Simulation
#'
#' Criteria are currently quite conservative:
#' priors must have the same class, and exactly
#' the same number of hyper-parameters. This could
#' in principle be relaxed in future.
#'
#' Note that if any two priors are non-compable,
#' then *no* hyper-parameters are compared in the
#' simulation.
#'
#' @param prior1,prior2 Two objects of class "bage_prior".
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
is_comparable_prior <- function(prior1, prior2) {
    UseMethod("is_comparable_prior")
}

## HAS_TESTS
#' @export
is_comparable_prior.bage_prior <- function(prior1, prior2) {
    is_same_class(prior1, prior2)
}

## HAS_TESTS
#' @export
is_comparable_prior.bage_prior_rw2 <- function(prior1, prior2) {
    if (!is_same_class(prior1, prior2))
        return(FALSE)
    sd_slope_1 <- prior1$const[["sd_slope"]]
    sd_slope_2 <- prior2$const[["sd_slope"]]
    isTRUE(all.equal(sd_slope_1, sd_slope_2))
}

## HAS_TESTS
#' @export
is_comparable_prior.bage_prior_spline <- function(prior1, prior2) {
    if (!is_same_class(prior1, prior2))
        return(FALSE)
    n_1 <- prior1$specific[["n"]]
    n_2 <- prior2$specific[["n"]]
    isTRUE(all.equal(n_1, n_2))
}

## HAS_TESTS
#' @export
is_comparable_prior.bage_prior_svd <- function(prior1, prior2) {
    if (!is_same_class(prior1, prior2))
        return(FALSE)
    n_1 <- prior1$specific[["n"]]
    n_2 <- prior2$specific[["n"]]
    if (!isTRUE(all.equal(n_1, n_2)))
        return(FALSE)
    indep_1 <- prior1$specific[["indep"]]
    indep_2 <- prior2$specific[["indep"]]
    isTRUE(all.equal(indep_1, indep_2))
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
#' @param matrix_along_by Matrix with mapping to along, by dimensions
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param is_in_compose Whether prior is being used as an
#' argument in a call to a 'compose' function
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#'
#' @returns TRUE or raises an error
#'
#' @noRd
is_prior_ok_for_term <- function(prior,
                                 nm,
                                 matrix_along_by,
                                 var_time,
                                 var_age,
                                 is_in_compose,
                                 agesex) {
    UseMethod("is_prior_ok_for_term")
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ar <- function(prior,
                                               nm,
                                               matrix_along_by,
                                               var_time,
                                               var_age,
                                               is_in_compose,
                                               agesex) {
  check_is_main_effect(nm = nm,
                       prior = prior)
  length_effect <- length(matrix_along_by)
  n <- prior$specific$n
  check_length_effect_ge(length_effect = length_effect,
                         min = n,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_compose <- function(prior,
                                                    nm,
                                                    matrix_along_by,
                                                    var_time,
                                                    var_age,
                                                    is_in_compose,
                                                    agesex) {
  priors <- prior$specific$priors
  nm_compose <- prior$specific$nm
  nm_split <- strsplit(nm, split = ":")[[1L]]
  if (nm_compose == "compose_time") {
    if (!(var_time %in% nm_split)) {
      msg <- c("Problem with call to {.fun bage::compose_time}.",
               i = "Term {.val {nm}} does not include a time dimension.")
      if (!is.null(var_time))
        msg <- c(msg, i = "Time dimension: {.val {var_time}}.")
      cli::cli_abort(msg)
    }
  }
  vapply(priors,
         is_prior_ok_for_term,
         TRUE,
         nm = nm,
         matrix_along_by = matrix_along_by,
         var_time = var_time,
         var_age = var_age,
         is_in_compose = TRUE,
         agesex = agesex)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_iar <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                is_in_compose,
                                                agesex) {
  check_is_interaction(nm = nm,
                       prior = prior)
  length_along <- nrow(matrix_along_by)
  n <- prior$specific$n
  check_length_along_ge(length_along = length_along,
                        min = n,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ilin <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 is_in_compose,
                                                 agesex) {
  check_is_interaction(nm = nm, prior = prior)
  length_along <- nrow(matrix_along_by)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_iseas <- function(prior,
                                                  nm,
                                                  matrix_along_by,
                                                  var_time,
                                                  var_age,
                                                  is_in_compose,
                                                  agesex) {
  if (!is_in_compose) {
    str <- str_call_prior(prior)
    cli::cli_abort(c("{.var {str}} prior cannot be used on its own.",
                     i = "{.var {str}} prior can only be inside 'compose' function."))
  }
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_known <- function(prior,
                                                  nm,
                                                  matrix_along_by,
                                                  var_time,
                                                  var_age,
                                                  is_in_compose,
                                                  agesex) {
  values <- prior$specific$values
  n_values <- length(values)
  length_effect <- length(matrix_along_by)
  if (n_values != length_effect) {
    str <- str_call_prior(prior)
    cli::cli_abort(c("{.var {str}} prior for {.var {nm}} term invalid.",
                     i = "Prior specifies {n_values} element{?s}.",
                     i = "{.var {nm}} has {length_effect} element{?s}."))
  }
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_lin <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                is_in_compose,
                                                agesex) {
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_norm <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 is_in_compose,
                                                 agesex) {
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_normfixed <- function(prior,
                                                      nm,
                                                      matrix_along_by,
                                                      var_time,
                                                      var_age,
                                                      is_in_compose,
                                                      agesex) {
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 1L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw <- function(prior,
                                               nm,
                                               matrix_along_by,
                                               var_time,
                                               var_age,
                                               is_in_compose,
                                               agesex) {
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2 <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                is_in_compose,
                                                agesex) {
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 3L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_seas <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 is_in_compose,
                                                 agesex) {
  if (!is_in_compose) {
    str <- str_call_prior(prior)
    cli::cli_abort(c("{.var {str}} prior cannot be used on its own.",
                     i = "{.var {str}} prior can only be inside 'compose' function."))
  }
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_spline <- function(prior,
                                                   nm,
                                                   matrix_along_by,
                                                   var_time,
                                                   var_age,
                                                   is_in_compose,
                                                   agesex) {
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                is_in_compose,
                                                agesex) {
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
  else {
    length_effect <- length(matrix_along_by)
    check_length_effect_ge(length_effect = length_effect,
                           min = 2L,
                           nm = nm,
                           prior = prior)
  }
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
levels_hyper.bage_prior_ar <- function(prior) {
  n <- prior$specific$n
  rep(c("coef", "sd"), times = c(n, 1L))
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_compose <- function(prior) {
  priors <- prior$specific$priors
  ans <- lapply(priors, levels_hyper)
  unlist(ans)
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_iar <- function(prior) {
  n <- prior$specific$n
  rep(c("coef", "sd"), times = c(n, 1L))
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_ilin <- function(prior) {
  c("slope", "sd", "msd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_iseas <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_known <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_lin <- function(prior)
    c("slope", "sd")

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
levels_hyper.bage_prior_seas <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_spline <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd <- function(prior)
    character()


## 'levels_hyperrand' ---------------------------------------------------------

#' Names of Hyper-Parameters that Can Be Treated as Random Effects
#'
#' Most priors don't have hyper-parameters that can be treated
#' as random effects, so default value is a character vector
#' of length 0.
#' 
#' @param prior An object of class 'bage_prior'.
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyperrand <- function(prior, matrix_along_by) {
    UseMethod("levels_hyperrand")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior <- function(prior, matrix_along_by) {
  character()
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_compose <- function(prior, matrix_along_by) {
  priors <- prior$specific$priors
  ans <- lapply(priors, levels_hyperrand, matrix_along_by = matrix_along_by)
  n_prior <- length(priors)
  n_effect <- length(matrix_along_by)
  for (i_prior in seq_len(n_prior - 1L))
    ans[[i_prior]] <- c(rep("effect", times = n_effect), ans[[i_prior]])
  unlist(ans, use.names = FALSE)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_ilin <- function(prior, matrix_along_by) {
  n_by <- ncol(matrix_along_by)
  rep.int("mslope", times = n_by)
}


## 'make_matrix_effectfree_effect' --------------------------------------------------

#' Make matrix mapping effectfree to effect
#'
#' Make matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param levels_effect Vector of labels for term
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_effectfree_effect <- function(prior, levels_effect, agesex) {
    UseMethod("make_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior <- function(prior, levels_effect, agesex) {
    n <- length(levels_effect)
    Matrix::.sparseDiagonal(n)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_spline <- function(prior, levels_effect, agesex) {
    n_spline <- prior$specific$n
    if (is.null(n_spline)) {
        n_spline <- 0.7 * length(levels_effect)
        n_spline <- ceiling(n_spline)
        n_spline <- max(n_spline, 4L)
    }
    length_effect <- length(levels_effect)
    ans <- make_spline_matrix(n_spline = n_spline,
                              length_effect = length_effect)
    rownames(ans) <- levels_effect
    ans
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd <- function(prior, levels_effect, agesex) {
    scaled_svd <- prior$specific$scaled_svd
    indep <- prior$specific$indep
    n_comp <- prior$specific$n
    get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                             levels_effect = levels_effect,
                             indep = indep,
                             agesex = agesex,
                             get_matrix = TRUE,
                             n_comp = n_comp)
}                             

    
## 'make_offset_effectfree_effect' --------------------------------------------------

#' Make offset used in converting effectfree to effect
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
make_offset_effectfree_effect <- function(prior, levels_effect, agesex) {
    UseMethod("make_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior <- function(prior, levels_effect, agesex) {
    n <- length(levels_effect)
    rep(0, times = n)
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd <- function(prior, levels_effect, agesex) {
    scaled_svd <- prior$specific$scaled_svd
    indep <- prior$specific$indep
    matrix <- get_matrix_or_offset_svd(scaled_svd = scaled_svd,
                                       levels_effect = levels_effect,
                                       indep = indep,
                                       agesex = agesex,
                                       get_matrix = FALSE,
                                       n_comp = NULL)
}


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
str_call_prior.bage_prior_ar <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  nm <- specific$nm
  if (nm == "AR") {
    args <- character(2L)
    if (n != 2L)
      args[[1L]] <- sprintf("n=%d", n)
    if (scale != 1)
      args[[2L]] <- sprintf("s=%s", scale)
  }
  else if (nm == "AR1") {
    args <- character(3L)
    if (min != 0.8)
      args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      args[[3L]] <- sprintf("s=%s", scale)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'nm'.")
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## NO_TESTS
#' @export
str_call_prior.bage_prior_compose <- function(prior) {
  priors <- prior$specific$priors
  nm <- prior$specific$nm
  str_priors <- vapply(priors, str_call_prior, "")
  str_priors <- paste(names(priors), str_priors, sep = "=")
  str_priors <- paste(str_priors, collapse = ", ")
  sprintf("%s(%s)", nm, str_priors)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_iar <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  nm <- specific$nm
  if (nm == "IAR") {
    args <- character(2L)
    args[[1L]] <- sprintf("n=%d", n)
    if (scale != 1)
      args[[2L]] <- sprintf("s=%s", scale)
  }
  else if (nm == "IAR1") {
    args <- character(3L)
    if (min != 0.8)
      args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      args[[3L]] <- sprintf("s=%s", scale)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'nm'.")
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_ilin <- function(prior) {
  scale <- prior$specific$scale
  sd_slope <- prior$specific$sd_slope
  mscale <- prior$specific$mscale
  along <- prior$specific$along
  args <- character(4L)
  if (scale != 1)
    args[[1L]] <- sprintf("s=%s", scale)
  if (sd_slope != 1)
    args[[2L]] <- sprintf("sd=%s", sd_slope)
  if (mscale != 1)
    args[[3L]] <- sprintf("ms=%s", mscale)
  if (!is.null(along))
    args[[4L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ILin(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_iseas <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  along <- prior$specific$along
  args <- character(3L)
  args[[1L]] <- sprintf("n=%d", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  if (!is.null(along))
    args[[3L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ISeas(%s)", args)
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
str_call_prior.bage_prior_lin <- function(prior) {
    scale <- prior$specific$scale
    sd_slope <- prior$specific$sd_slope
    if (isTRUE(all.equal(scale, 1))) {
        if (isTRUE(all.equal(sd_slope, 1)))
            "Lin()"
        else
            sprintf("Lin(sd=%s)", sd_slope)
    }
    else {
        if (isTRUE(all.equal(sd_slope, 1)))
            sprintf("Lin(s=%s)", scale)
        else
            sprintf("Lin(s=%s,sd=%s)", scale, sd_slope)
    }
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
  sd_slope <- prior$specific$sd_slope
  args <- character(2L)
  if (scale != 1)
    args[[1L]] <- sprintf("s=%s", scale)
  if (sd_slope != 1)
    args[[2L]] <- sprintf("sd=%s", sd_slope)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_seas <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  args <- character(2L)
  args[[1]] <- sprintf("n=%d", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_spline <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  args <- character(2L)
  if (!is.null(n))
    args[[1L]] <- sprintf("n=%s", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Sp(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd <- function(prior) {
  nm_scaled_svd <- prior$specific$nm_scaled_svd
  n <- prior$specific$n
  indep <- prior$specific$indep
  args <- character(3L)
  args[[1L]] <- nm_scaled_svd
  if (n != 5)
    args[[2L]] <- sprintf("n=%s", n)
  if (!indep)
    args[[3L]] <- "indep=FALSE"
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD(%s)", args)
}


## 'transform_hyper' ----------------------------------------------------------

#' Transform to convert working TMB version
#' of parameter back to original units
#'+
#' @param prior An object of class 'bage_prior'.
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#'
#' @returns A list of functions.
#'
#' @noRd
transform_hyper <- function(prior, matrix_along_by) {
    UseMethod("transform_hyper")
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ar <- function(prior, matrix_along_by) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n, 1L))
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_iar <- function(prior, matrix_along_by) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n, 1L))
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ilin <- function(prior, matrix_along_by) {
  n_by <- ncol(matrix_along_by)
  c(list(slope = identity),
    rep(list(mslope = identity), times = n_by),
    sd = exp,
    msd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_iseas <- function(prior, matrix_along_by) {
  list(sd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_known <- function(prior, matrix_along_by)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_lin <- function(prior, matrix_along_by)
    list(slope = identity, sd = exp)
    
## HAS_TESTS
#' @export
transform_hyper.bage_prior_norm <- function(prior, matrix_along_by)
    list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_normfixed <- function(prior, matrix_along_by)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw <- function(prior, matrix_along_by)
    list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2 <- function(prior, matrix_along_by)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_seas <- function(prior, matrix_along_by)
    list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_spline <- function(prior, matrix_along_by)
    list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd <- function(prior, matrix_along_by)
    list()


## 'use_for_compose_cyclical' -------------------------------------------------

## HAS_TESTS
#' Whether a Prior can Be Used as a 'cyclical' Argument in 'compose_time' and 'compose_age'
#'
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_compose_cyclical <- function(prior) {
  UseMethod("use_for_compose_cyclical")
}

#' @export
use_for_compose_cyclical.bage_prior <- function(prior) FALSE

#' @export
use_for_compose_cyclical.bage_prior_ar <- function(prior) TRUE

#' @export
use_for_compose_cyclical.bage_prior_iar <- function(prior) TRUE


## 'use_for_compose_error' -------------------------------------------------

## HAS_TESTS
#' Whether a Prior can Be Used as a 'error' Argument in 'compose_time' and 'compose_age'
#'
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_compose_error <- function(prior) {
  UseMethod("use_for_compose_error")
}

#' @export
use_for_compose_error.bage_prior <- function(prior) FALSE

#' @export
use_for_compose_error.bage_prior_norm <- function(prior) TRUE


## 'use_for_compose_seasonal' -------------------------------------------------

## HAS_TESTS
#' Whether a Prior can Be Used as a 'seasonal' Argument in 'compose_time'
#'
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_compose_seasonal <- function(prior) {
  UseMethod("use_for_compose_seasonal")
}

#' @export
use_for_compose_seasonal.bage_prior <- function(prior) FALSE

#' @export
use_for_compose_seasonal.bage_prior_seas <- function(prior) TRUE

#' @export
use_for_compose_seasonal.bage_prior_iseas <- function(prior) TRUE


## 'use_for_compose_trend' ----------------------------------------------------

## HAS_TESTS
#' Whether a Prior can Be Used as a 'trend' Argument in 'compose_time'
#'
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_compose_trend <- function(prior) {
  UseMethod("use_for_compose_trend")
}

#' @export
use_for_compose_trend.bage_prior <- function(prior) FALSE

#' @export
use_for_compose_trend.bage_prior_ilin <- function(prior) TRUE

#' @export
use_for_compose_trend.bage_prior_lin <- function(prior) TRUE

#' @export
use_for_compose_trend.bage_prior_rw <- function(prior) TRUE

#' @export
use_for_compose_trend.bage_prior_rw2 <- function(prior) TRUE

#' @export
use_for_compose_trend.bage_prior_spline <- function(prior) TRUE



## 'use_for_interaction' ------------------------------------------------------

## HAS_TESTS
#' Whether a Prior is Used Exclusively to Model Interactions
#'
#' If a prior is used to model interactions *and* main effects,
#' then the function returns `FALSE`.
#' 
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_interaction <- function(prior) {
  UseMethod("use_for_interaction")
}

#' @export
use_for_interaction.bage_prior <- function(prior) FALSE

#' @export
use_for_interaction.bage_prior_iar <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_ilin <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_iseas <- function(prior) TRUE


## 'use_for_main_effect' ------------------------------------------------------

## HAS_TESTS
#' Whether a Prior is Used Exclusively to Model Main Effects
#'
#' If a prior is used to model main effects *and* interactions,
#' then the function returns `FALSE`.
#' 
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_main_effect <- function(prior) {
  UseMethod("use_for_main_effect")
}

#' @export
use_for_main_effect.bage_prior <- function(prior) FALSE

#' @export
use_for_main_effect.bage_prior_ar <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_lin <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_rw <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_rw2 <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_spline <- function(prior) TRUE


## 'uses_along' ---------------------------------------------------------------

#' Whether Prior uses an 'along' Dimension
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_along <- function(prior) {
    UseMethod("uses_along")
}

## HAS_TESTS
#' @export
uses_along.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_iar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_ilin <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_iseas <- function(prior) TRUE


## 'uses_matrix_effectfree_effect' --------------------------------------------------

#' Whether prior uses matrix to convert effectfree to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_matrix_effectfree_effect <- function(prior) {
    UseMethod("uses_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd <- function(prior) TRUE


## 'uses_offset_effectfree_effect' --------------------------------------------------

#' Whether prior uses offset to convert effectfree to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_offset_effectfree_effect <- function(prior) {
    UseMethod("uses_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd <- function(prior) TRUE



## 'uses_matrix_effectfree_effect' --------------------------------------------------

#' Whether prior uses matrix to transform effectfree
#' to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE
#'
#' @noRd
uses_matrix_effectfree_effect <- function(prior) {
    UseMethod("uses_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd <- function(prior) TRUE


## 'uses_offset_effectfree_effect' --------------------------------------------------

#' Whether prior uses offset to transform effectfree
#' to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE
#'
#' @noRd
uses_offset_effectfree_effect <- function(prior) {
    UseMethod("uses_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd <- function(prior) TRUE


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


