
## HAS_TESTS
#' Combined Stored Draws and Point EStimates from Two Models
#'
#' Term used from first model if 'use_term' is TRUE; otherwise
#' from second model. Dispersion ignored.
#'
#' @param mod Model receiving the draws
#' @param mod_inner Model for which use_term is TRUE
#' @param mod_outer Model for which use_term is FALSE
#' @param use_term Logical vector
#'
#' @returns Modified version of 'mod'
#' 
#' @noRd
combine_stored_draws_point_inner_outer <- function(mod, mod_inner, mod_outer, use_term) { 
  priors <- mod$priors
  n_term <- length(priors)
  nms_term <- names(priors)
  terms_effectfree_inner <- make_terms_effectfree(mod_inner)
  terms_effectfree_outer <- make_terms_effectfree(mod_outer)
  terms_hyper_inner <- make_terms_hyper(mod_inner)
  terms_hyper_outer <- make_terms_hyper(mod_outer)
  terms_hyperrandfree_inner <- make_terms_hyperrandfree(mod_inner)
  terms_hyperrandfree_outer <- make_terms_hyperrandfree(mod_outer)
  draws_effectfree_inner <- mod_inner$draws_effectfree
  draws_effectfree_outer <- mod_outer$draws_effectfree
  draws_hyper_inner <- mod_inner$draws_hyper
  draws_hyper_outer <- mod_outer$draws_hyper
  draws_hyperrandfree_inner <- mod_inner$draws_hyperrandfree
  draws_hyperrandfree_outer <- mod_outer$draws_hyperrandfree
  point_effectfree_inner <- mod_inner$point_effectfree
  point_effectfree_outer <- mod_outer$point_effectfree
  point_hyper_inner <- mod_inner$point_hyper
  point_hyper_outer <- mod_outer$point_hyper
  point_hyperrandfree_inner <- mod_inner$point_hyperrandfree
  point_hyperrandfree_outer <- mod_outer$point_hyperrandfree
  draws_effectfree <- vector(mode = "list", length = n_term)
  draws_hyper <- vector(mode = "list", length = n_term)
  draws_hyperrandfree <- vector(mode = "list", length = n_term)
  point_effectfree <- double()
  point_hyper <- double()
  point_hyperrandfree <- double()
  for (i_term in seq_len(n_term)) {
    nm_term <- nms_term[[i_term]]
    use_inner <- use_term[[i_term]]
    if (use_inner) {
      is_term_effectfree <- terms_effectfree_inner == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_inner[is_term_effectfree, , drop = FALSE]
      point_effectfree <- c(point_effectfree, point_effectfree_inner[is_term_effectfree])
      is_term_hyper <- terms_hyper_inner == nm_term
      draws_hyper[[i_term]] <- draws_hyper_inner[is_term_hyper, , drop = FALSE]
      point_hyper <- c(point_hyper, point_hyper_inner[is_term_hyper])
      is_term_hyperrandfree <- terms_hyperrandfree_inner == nm_term
      draws_hyperrandfree[[i_term]] <- draws_hyperrandfree_inner[is_term_hyperrandfree, , drop = FALSE]
      point_hyperrandfree <- c(point_hyperrandfree, point_hyperrandfree_inner[is_term_hyperrandfree])
    }
    else {
      is_term_effectfree <- terms_effectfree_outer == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_outer[is_term_effectfree, , drop = FALSE]
      point_effectfree <- c(point_effectfree, point_effectfree_outer[is_term_effectfree])
      is_term_hyper <- terms_hyper_outer == nm_term
      draws_hyper[[i_term]] <- draws_hyper_outer[is_term_hyper, , drop = FALSE]
      point_hyper <- c(point_hyper, point_hyper_outer[is_term_hyper])
      is_term_hyperrandfree <- terms_hyperrandfree_outer == nm_term
      draws_hyperrandfree[[i_term]] <- draws_hyperrandfree_outer[is_term_hyperrandfree, , drop = FALSE]
      point_hyperrandfree <- c(point_hyperrandfree, point_hyperrandfree_outer[is_term_hyperrandfree])
    }
  }
  draws_effectfree <- do.call(rbind, draws_effectfree)
  draws_hyper <- do.call(rbind, draws_hyper)
  draws_hyperrandfree <- do.call(rbind, draws_hyperrandfree)
  mod$draws_effectfree <- draws_effectfree
  mod$draws_hyper <- draws_hyper
  mod$draws_hyperrandfree <- draws_hyperrandfree
  mod$point_effectfree <- point_effectfree
  mod$point_hyper <- point_hyper
  mod$point_hyperrandfree <- point_hyperrandfree
  mod
}


## HAS_TESTS
#' Standardize 'fitted' So It Conforms to 'con == "by"' Constraints
#'
#' @param prior Object of class 'bage_prior'
#' @param fitted A vector, possibly an rvec
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time dimension
#' @param var_age Name of age dimension
#'
#' @returns A standardized version of 'fitted'
#'
#' @noRd
con_by_fitted <- function(prior,
                          fitted,
                          dimnames_term,
                          var_time,
                          var_age) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  m <- make_matrix_con_by(i_along = i_along,
                          dim = dim)
  m <- as.matrix(m)
  fitted <- m %*% fitted
}


## HAS_TESTS
#' Return Values for Higher-Level Parameters from Fitted Model
#'
#' @param mod A fitted object of class 'bage_mod'
#' estimates
#'
#' @returns A tibble
#'
#' @noRd
draw_vals_components_fitted <- function(mod) {
  data <- mod$data
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  tibble::tibble(term = term,
                 component = comp,
                 level = level,
                 .fitted = draws)
}


## HAS_TESTS
#' Default Method for Fitting a Model
#'
#' @param object A `bage_mod` object.
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param aggregate Whether to aggregate outcome and offset variables
#' @param optimizer Which optimizer to use
#' @param quiet Whether to suppress warnings and trace information
#' from optimizer
#' @param start_oldpar Whether to start from old parameter values
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_default <- function(mod, aggregate, optimizer, quiet, start_oldpar) {
  t1 <- Sys.time()
  optimizer_original <- optimizer
  if (start_oldpar) {
    if (is_fitted(mod))
      oldpar <- mod$oldpar
    else
      cli::cli_abort("{.arg start_oldpar} is {.val {start_oldpar}} but model has not been fitted.")
  }
  mod <- unfit(mod)
  ## data
  if (aggregate) {
    vals_ag <- make_vals_ag(mod)
    outcome <- vals_ag$outcome
    offset <- vals_ag$offset
    matrices_effect_outcome <- vals_ag$matrices_effect_outcome
  }
  else {
    vals_in_lik <- make_vals_in_lik(mod)
    outcome <- vals_in_lik$outcome
    offset <- vals_in_lik$offset
    matrices_effect_outcome <- vals_in_lik$matrices_effect_outcome
  }
  dimnames_terms <- mod$dimnames_terms
  terms_effect <- make_terms_effects(dimnames_terms)
  i_lik <- make_i_lik_mod(mod)
  terms_effectfree <- make_terms_effectfree(mod)
  uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(mod)
  matrices_effectfree_effect <- make_matrices_effectfree_effect(mod)
  uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(mod)
  offsets_effectfree_effect <- make_offsets_effectfree_effect(mod)
  i_prior <- make_i_prior(mod)
  uses_hyper <- make_uses_hyper(mod)
  terms_hyper <- make_terms_hyper(mod)
  uses_hyperrandfree <- make_uses_hyperrandfree(mod)
  terms_hyperrandfree <- make_terms_hyperrandfree(mod)
  const <- make_const(mod)
  terms_const <- make_terms_const(mod)
  matrices_along_by_effectfree <- make_matrices_along_by_effectfree(mod)
  mean_disp <- mod$mean_disp
  has_disp <- mean_disp > 0
  data <- list(i_lik = i_lik,
               outcome = outcome,
               offset = offset,
               terms_effect = terms_effect,
               terms_effectfree = terms_effectfree,
               uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
               matrices_effectfree_effect = matrices_effectfree_effect,
               uses_offset_effectfree_effect = uses_offset_effectfree_effect,
               offsets_effectfree_effect = offsets_effectfree_effect,
               matrices_effect_outcome = matrices_effect_outcome,
               i_prior = i_prior,
               uses_hyper = uses_hyper,
               terms_hyper = terms_hyper,
               uses_hyperrandfree = uses_hyperrandfree,
               terms_hyperrandfree = terms_hyperrandfree,
               consts = const, ## 'const' is reserved word in C
               terms_consts = terms_const,
               matrices_along_by_effectfree = matrices_along_by_effectfree,
               mean_disp = mean_disp)
  ## parameters
  if (start_oldpar)
    parameters <- oldpar
  else {
    effectfree <- make_effectfree(mod)
    hyper <- make_hyper(mod)
    hyperrandfree <- make_hyperrandfree(mod)
    log_disp <- c(disp = 0)
    parameters <- list(effectfree = effectfree,   
                       hyper = hyper,
                       hyperrandfree = hyperrandfree,
                       log_disp = log_disp)
  }
  ## MakeADFun
  map <- make_map(mod)
  random <- make_random(mod)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ## optimise
  t2 <- Sys.time()
  if (optimizer %in% c("multi", "nlminb")) {
    if (quiet) {
      suppressWarnings(out <- stats::nlminb(start = f$par,
                                            objective = f$fn,
                                            gradient = f$gr,
                                            control = list(iter.max = 300L,
                                                           eval.max = 400L,
                                                           trace = 0L)))
    }
    else {
      cli::cli_alert("Output from {.fun nlminb}:")
      out <- stats::nlminb(start = f$par,
                           objective = f$fn,
                           gradient = f$gr,
                           control = list(iter.max = 300L,
                                          eval.max = 400L,
                                          trace = 1L))
    }
    iter <- out$iterations
    message <- out$message
    multi_nonconv <- identical(optimizer, "multi") && !identical(out$convergence, 0L)
    if (multi_nonconv) {
      if (!quiet)
        cli::cli_alert_info("\"nlminb\" optimizer did not converge: continuing with \"BFGS\".")
      if (has_random_effects)
        sdreport <- TMB::sdreport(f, bias.correct = FALSE, getJointPrecision = FALSE)
      else
        sdreport <- TMB::sdreport(f, bias.correct = FALSE, getReportCovariance = FALSE)
      oldpar <- as.list(sdreport, what = "Est")
      f <- TMB::MakeADFun(data = data,
                          parameters = oldpar,
                          map = map,
                          DLL = "bage",
                          random = random,
                          silent = TRUE)
      optimizer <- "BFGS"
    }
  }
  if (optimizer %in% c("BFGS", "CG")) {
    if (quiet) {
      suppressWarnings(out <- stats::optim(par = f$par,
                                           fn = f$fn,
                                           gr = f$gr,
                                           method = optimizer,
                                           control = list(maxit = 300L,
                                                          trace = 0L)))
    }
    else {
      cli::cli_alert("Output from {.fun optim}:")
      out <- stats::optim(par = f$par,
                          fn = f$fn,
                          gr = f$gr,
                          method = optimizer,
                          control = list(trace = 1L,
                                         maxit = 300L,
                                         REPORT = 1L))
    }
    if (optimizer_original == "multi")
      iter <- paste(iter, out$counts[["gradient"]], sep = "+")
    else
      iter <- out$counts[["gradient"]]
    message <- out$message
    if (is.null(message))
      message <- "<none>"
  }
  t3 <- Sys.time()
  ## extract results
  if (has_random_effects)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
  else
    sdreport <- TMB::sdreport(f) 
  est <- as.list(sdreport, what = "Est")
  check_est(est)
  if (has_random_effects) {
    prec <- sdreport$jointPrecision
    check_var_prec(x = prec, est = est)
  }
  else {
    var <- sdreport$cov.fixed
    check_var_prec(x = var, est = est)
    prec <- solve(var) ## should be very low dimension
  }
  t4 <- Sys.time()
  mod <- make_stored_draws(mod = mod,
                           est = est,
                           prec = prec,
                           map = map)
  mod <- make_stored_point(mod = mod,
                           est = est)
  t5 <- Sys.time()
  time_total <- as.numeric(difftime(t5, t1, units = "secs"))
  time_optim <- as.numeric(difftime(t3, t2, units = "secs"))
  time_report <- as.numeric(difftime(t4, t3, units = "secs"))
  converged <- identical(out$convergence, 0L)
  if (!converged)
    cli::cli_alert_warning("Optimizer did not converge.")
  mod$computations <- tibble::tibble(time_total = time_total,
                                     time_optim = time_optim,
                                     time_report = time_report,
                                     iter = iter,
                                     converged = converged,
                                     message = message)
  mod$optimizer <- optimizer_original
  mod$oldpar <- est
  mod
}


## HAS_TESTS
#' Two-Step Method for Fitting a Model
#'
#' @param object A `bage_mod` object.
#' @param quiet Whether to suppress warning messages from nlminb
#' @param vars_inner Variables used
#' in inner model.
#' @param optimizer Which optimizer to use
#' @param quiet Whether to suppress warnings from 'optimizer'
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_inner_outer <- function(mod, optimizer, quiet, vars_inner, start_oldpar) {
  if (start_oldpar)
    cli::cli_abort("{.arg start_oldpar} must be {.val {FALSE}} when using \"inner-outer\" method.")
  if (is.null(vars_inner))
    vars_inner <- make_vars_inner(mod)
  else
    check_vars_inner(vars_inner)
  use_term <- make_use_term(mod = mod,
                            vars_inner = vars_inner)
  mod_inner <- make_mod_inner(mod = mod,
                              use_term = use_term)
  mod_inner <- fit_default(mod = mod_inner,
                           optimizer = optimizer,
                           quiet = quiet,
                           start_oldpar = start_oldpar,
                           aggregate = TRUE)
  mod_outer <- make_mod_outer(mod = mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  mod_outer <- fit_default(mod = mod_outer,
                           optimizer = optimizer,
                           quiet = quiet,
                           start_oldpar = start_oldpar,
                           aggregate = TRUE)
  computations <- rbind(inner = mod_inner$computations,
                        outer = mod_outer$computations)
  mod <- combine_stored_draws_point_inner_outer(mod = mod,
                                                mod_inner = mod_inner,
                                                mod_outer = mod_outer,
                                                use_term = use_term)
  if (has_disp(mod)) {
    mod_disp <- make_mod_disp(mod)
    mod_disp <- fit_default(mod = mod_disp,
                            optimizer = optimizer,
                            quiet = quiet,
                            start_oldpar = start_oldpar,
                            aggregate = FALSE)
    computations <- rbind(computations,
                          disp = mod_disp$computations)
    mod <- transfer_draws_disp(mod = mod,
                               mod_disp = mod_disp)
  }
  computations <- cbind(model = rownames(computations),
                        computations)
  rownames(computations) <- NULL
  mod$computations <- computations
  mod$vars_inner <- vars_inner
  mod
}


## HAS_TESTS
#' Helper Function for 'generate' Methods for 'bage_prior'
#'
#' @param x Object of class 'bage_prior'
#' @param n_element Total number of elements
#' @param n_along Number of elements of 'along' variable
#' @param n_by Number of combinations of 'by' variables
#' @param n_draw Number of draws
#'
#' @returns A named list
#'
#' @noRd
generate_prior_helper <- function(x, n_element, n_along, n_by, n_draw) {
  uses_along_by <- missing(n_element)
  if (uses_along_by) {
    poputils::check_n(n = n_along,
                      nm_n = "n_along",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    poputils::check_n(n = n_by,
                      nm_n = "n_by",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
  }
  else
    poputils::check_n(n = n_element,
                      nm_n = "n_element",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
  poputils::check_n(n = n_draw,
                    nm_n = "n_draw",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  if (uses_along_by) {
    ans <- vctrs::vec_expand_grid(draw = paste("Draw", seq_len(n_draw)),
                                  by = paste("By", seq_len(n_by)),
                                  along = seq_len(n_along))
    ans$draw <- factor(ans$draw, levels = unique(ans$draw))
    ans$by <- factor(ans$by, levels = unique(ans$by))
    matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L,
                              nrow = n_along,
                              ncol = n_by,
                              dimnames = list(seq_len(n_along), seq_len(n_by)))
    levels_effect <- paste(rep(seq_len(n_by), each = n_along), seq_len(n_along), sep = ".")
  }
  else {
    ans <- vctrs::vec_expand_grid(draw = paste("Draw", seq_len(n_draw)),
                                  element = seq_len(n_element))
    ans$draw <- factor(ans$draw, levels = unique(ans$draw))
    matrix_along_by <- matrix(seq_len(n_element) - 1L,
                              nrow = n_element,
                              ncol = 1L,
                              dimnames = list(seq_len(n_element), NULL))
    levels_effect <- seq_len(n_element)
  }
  ans <- tibble::tibble(ans)
  list(ans = ans,
       matrix_along_by = matrix_along_by,
       levels_effect = levels_effect)
}


## HAS_TESTS
#' Helper Function for 'generate' Methods for 'bage_prior' that
#' Uses SVD
#'
#' @param x Object of class 'bage_prior' that uses SVD
#' @param n_elements Number of element
#' @param n_along Number of element in 'along' dimension
#' @param n_by Number of combinations of categories for 'by' variables
#' @param n_draw Number of draws
#'
#' @returns A named list
#'
#' @noRd
generate_prior_svd_helper <- function(x, n_element, n_along, n_by, n_draw) {
  uses_along_by <- missing(n_element)
  ssvd <- x$specific$ssvd
  n_comp <- x$specific$n_comp
  indep <- x$specific$indep
  if (isTRUE(indep) && !has_sexgender(ssvd))
    indep <- NULL
  if (uses_along_by)
    generate_ssvd_helper(ssvd = ssvd,
                         n_along = n_along,
                         n_by = n_by,
                         n_draw = n_draw,
                         n_comp = n_comp,
                         indep = indep,
                         age_labels = NULL)
  else
    generate_ssvd_helper(ssvd = ssvd,
                         n_element = n_element,
                         n_draw = n_draw,
                         n_comp = n_comp,
                         indep = indep,
                         age_labels = NULL)
}


## HAS_TESTS
#' Generate Values for Scaled SVD
#'
#' Used by 'generate' method for "bage_ssvd" and 'generate'
#' method for "bage_prior" (after minor rearrangement
#' by function 'generate_prior_svd_helper').
#'
#' @param svd An object of class `"bage_ssvd"`.
#' @param n_along Number of element in 'along' dimension (always 1 for SVD() prior)
#' @param n_by Number of combinations of categories for 'by' variables
#' @param n_comp Number of SVD components to be used
#' @param indep Whether to use independent SVDs for sexes/genders
#' @param n_draw Number of random draws to generate.
#' @param age_labels Vector of age labels
#'
#' @returns A named list
#'
#' @noRd
generate_ssvd_helper <- function(ssvd,
                                 n_element,
                                 n_along,
                                 n_by,
                                 n_comp,
                                 indep,
                                 n_draw,
                                 age_labels) {
  uses_along_by <- missing(n_element)
  if (uses_along_by) {
    poputils::check_n(n = n_along,
                      nm_n = "n_along",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    poputils::check_n(n = n_by,
                      nm_n = "n_by",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    n_element <- n_by * n_along
  }
  else
    poputils::check_n(n = n_element,
                      nm_n = "n_element",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
  poputils::check_n(n = n_draw,
                    nm_n = "n_draw",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  n_comp_ssvd <- get_n_comp(ssvd)
  if (is.null(n_comp))
    n_comp <- ceiling(n_comp_ssvd / 2)
  else {
    poputils::check_n(n = n_comp,
                      nm_n = "n_comp",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    if (n_comp > n_comp_ssvd)
      cli::cli_abort(c("{.arg n_comp} larger than number of components of {.arg x}.",
                       i = "{.arg n_comp}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_ssvd}}."))
  }
  n_comp <- as.integer(n_comp)
  has_indep <- !is.null(indep)
  if (has_indep) {
    check_flag(x = indep, nm_x = "indep")
    if (!has_sexgender(ssvd))
      cli::cli_abort(paste("Value supplied for {.arg indep}, but {.arg x}",
                           "does not have a sex/gender dimension."))
    type <- if (indep) "indep" else "joint"
  }
  else
    type <- "total"
  has_age <- !is.null(age_labels)
  if (has_age) {
    age_labels <- tryCatch(poputils::reformat_age(age_labels, factor = FALSE),
                           error = function(e) e)
    if (inherits(age_labels, "error"))
      cli::cli_abort(c("Problem with {.arg age_labels}.",
                       i = age_labels$message))
  }
  data <- ssvd$data
  data <- data[data$type == type, , drop = FALSE]
  if (has_age) {
    is_matched <- vapply(data$labels_age, setequal, TRUE, y = age_labels)
    if (!any(is_matched))
      cli::cli_abort("Can't find labels from {.arg age_labels} in {.arg x}.")
    i_matched <- which(is_matched)
  }
  else {
    lengths_labels <- lengths(data$labels_age)
    i_matched <- which.max(lengths_labels)
  }
  levels_age <- data$labels_age[[i_matched]]
  levels_sexgender <- data$labels_sexgender[[i_matched]]
  levels_age <- unique(levels_age)
  levels_sexgender <- unique(levels_sexgender)
  n_sexgender <- length(levels_sexgender)
  agesex <- if (has_indep) "age:sex" else "age"
  matrix <- get_matrix_or_offset_svd(ssvd = ssvd,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = TRUE,
                                     n_comp = n_comp)
  offset <- get_matrix_or_offset_svd(ssvd = ssvd,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = FALSE,
                                     n_comp = n_comp)
  n_svd <- ncol(matrix)
  I <- Matrix::.sparseDiagonal(n_element)
  ones <- Matrix::sparseMatrix(i = seq_len(n_element),
                               j = rep(1L, times = n_element),
                               x = rep(1, times = n_element))
  matrix <- Matrix::kronecker(I, matrix)
  offset <- Matrix::kronecker(ones, offset)
  offset <- as.double(offset)
  if (uses_along_by) {
    dim <- c(n_svd, n_by, n_along)
    matrix_along_by <- make_matrix_along_by_inner(i_along = 3L, dim = dim)
  }
  else {
    dim <- c(n_svd, n_element)
    matrix_along_by <- make_matrix_along_by_inner(i_along = 1L, dim = dim)
  }    
  if (uses_along_by) {
    if (has_indep)
      levels <- list(by = seq_len(n_by),
                     along = seq_len(n_along),
                     sexgender = levels_sexgender,
                     age = levels_age)
    else
      levels <- list(by = seq_len(n_by),
                     along = seq_len(n_along),
                     age = levels_age)
  }
  else {
    if (has_indep)
      levels <- list(element = seq_len(n_element),
                     sexgender = levels_sexgender,
                     age = levels_age)
    else 
      levels <- list(element = seq_len(n_element),
                     age = levels_age)
  }
  levels_draw <- list(draw = seq_len(n_draw))
  levels <- c(levels_draw, levels)
  ans <- vctrs::vec_expand_grid(!!!levels)
  ans$draw <- paste("Draw", ans$draw)
  ans$draw <- factor(ans$draw, levels = unique(ans$draw))
  if (uses_along_by) {
    ans$by <- paste("By", ans$by)
    ans$by <- factor(ans$by, levels = unique(ans$by))
  }
  if (has_indep)
    ans$sexgender <- poputils::reformat_sex(ans$sexgender)
  ans$age <- poputils::reformat_age(ans$age)
  ans <- tibble::tibble(ans)
  list(ans = ans,
       matrix = matrix,
       offset = offset,
       matrix_along_by = matrix_along_by)
}


## HAS_TESTS
#' Extract Values for Dispersion
#'
#' Works with fitted or unfitted models
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
get_disp <- function(mod) {
  if (is_fitted(mod)) {
    ans <- mod$draws_disp
    ans <- matrix(ans, nrow = 1L)
    ans <- rvec::rvec_dbl(ans)
  }
  else {
    n_draw <- mod$n_draw
    ans <- draw_vals_disp(mod, n_sim = n_draw)
  }
  ans
}


## HAS_TESTS
#' Get the Name of the Term(s) for Element(s) in the 'est'
#' Object Returned by TMB
#'
#' @param est Named list
#' @param index_term Integer vector with indices of
#' elements within unlisted version of 'est'
#'
#' @returns A character vector
#'
#' @noRd
get_term_from_est <- function(est, index_term) {
  nm_term <- lapply(est, names)
  nm_term <- unlist(nm_term, use.names = FALSE)
  ans <- nm_term[index_term]
  ans <- unique(ans)
  ans
}


## HAS_TESTS
#' Insert a Into a Data Frame
#'
#' Insert a variable into a dataframe, immediately
#' after another variable.
#'
#' Currently assumes that not inserting
#' at start of 'df'.
#'
#' @param df A data frame (including a tibble)
#' @param nm_after Name of the variable that the
#' 'x' should come after
#' @param x New variable
#' @param nm_x Name of new variable
#'
#' @returns A modified version of 'df'
#'
#' @noRd
insert_after <- function(df, nm_after, x, nm_x) {
  nms_df <- names(df)
  n_df <- length(nms_df)
  i_after <- match(nm_after, names(df))
  if (i_after < n_df) {
    s_before <- seq_len(i_after)
    s_after <- seq.int(from = i_after + 1L, to = n_df)
    ans <- vctrs::vec_cbind(df[s_before],
                            x,
                            df[s_after],
                            .name_repair = "universal_quiet")
  }
  else {
    ans <- vctrs::vec_cbind(df,
                            x,
                            .name_repair = "universal_quiet")
  }
  names(ans)[[i_after + 1L]] <- nm_x
  ans
}
  

## HAS_TESTS
#' Test Whether Two Objects Have the Same Class
#'
#' @param x,y Objects
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_class <- function(x, y)
  identical(class(x)[[1L]], class(y)[[1L]])


## HAS_TESTS
#' Derive the Name of the 'along' Dimension for Terms in Model
#'
#' Returns NA for terms that do not have an "along" dimension
#' 
#' @param mod An object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_along_mod <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  n_term <- length(priors)
  ans <- character(length = n_term)
  nms <- names(priors)
  names(ans) <- nms
  for (i_term in seq_len(n_term)) {
    prior <- priors[[i_term]]
    dimnames_term <- dimnames_terms[[i_term]]
    if (uses_along(prior)) {
      i_along <- make_i_along(prior = prior,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age)
      nm_split <- dimnames_to_nm_split(dimnames_term)
      ans[[i_term]] <- nm_split[[i_along]]
    }
    else
      ans[[i_term]] <- NA_character_
  }
  ans
}


## HAS_TESTS
#' Create combined matrix from effect to outcome
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'effect' to 'outcome'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_effect_outcome <- function(mod) {
  data <- mod$data
  dimnames_terms <- mod$dimnames_terms
  nms_terms <- names(dimnames_terms)
  matrices_effect_outcome <- make_matrices_effect_outcome(data = data,
                                                          dimnames_terms = dimnames_terms)
  Reduce(Matrix::cbind2, matrices_effect_outcome)
}


## HAS_TESTS
#' Create combined matrix from effectfree to effect
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'effectfree' to 'effect'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_effectfree_effect <- function(mod) {
    matrices <- make_matrices_effectfree_effect(mod)
    Matrix::.bdiag(matrices)
}


## HAS_TESTS
#' Create combined offset from effectfree to effect
#'
#' Combine offsets for individual terms to
#' create a offset that maps all elements of
#' 'effectfree' to 'effect'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A numeric vector
#'
#' @noRd
make_combined_offset_effectfree_effect <- function(mod) {
    offsets <- make_offsets_effectfree_effect(mod)
    do.call(c, offsets)
}


## HAS_TESTS
#' Make variable identifying component in 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_comp_components <- function(mod) {
  dimnames_terms <- mod$dimnames_terms
  terms_effects <- make_terms_effects(dimnames_terms)
  hyper <- make_hyper(mod)
  term_spline <- make_term_spline(mod)
  term_svd <- make_term_svd(mod)
  has_disp <- has_disp(mod)
  effect <- rep("effect", times = length(terms_effects))
  hyper <- rep("hyper", times = length(hyper))
  hyperrand <- make_comp_hyperrand(mod)
  spline <- rep("spline", times = length(term_spline))
  svd <- rep("svd", times = length(term_svd))
  disp <- rep("disp", times = has_disp)
  ans <- c(effect, hyper, hyperrand, spline, svd, disp)
  ans
}


## HAS_TESTS
#' Make Variable Identifying Component in 'components' - Hyperrand Only
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_comp_hyperrand <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  ans <- .mapply(comp_hyperrand,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age))
  ans <- unlist(ans)
  ans
}


## HAS_TESTS
#' Make copies of the original data to use
#' for replicate data
#'
#' @param data Original data supplied to model. A tibble.
#' @param n Number of replicates
#'
#' @returns A tibble
#'
#' @noRd
make_copies_repdata <- function(data, n) {
    n_row_data <- nrow(data)
    .replicate <- make_levels_replicate(n = n, n_row_data = n_row_data)
    .replicate <- tibble::tibble(.replicate = .replicate)
    data <- rep(list(data), times = n + 1L) # n replicates, plus original
    data <- vctrs::vec_rbind(!!!data)
    vctrs::vec_cbind(.replicate, data)
}
    

## HAS_TESTS
#' Make draws from posterior distribution of
#' all parameters other than lowest-level
#' rates/probs/means
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_draws_components <- function(mod) {
  ## effects
  effectfree <- mod$draws_effectfree
  ans_effects <- make_effects(mod = mod, effectfree = effectfree)
  ans_effects <- rvec::rvec_dbl(ans_effects)
  ## hyper
  hyper <- mod$draws_hyper
  ans_hyper <- rvec::rvec_dbl(hyper)
  ## hyperrand
  ans_hyperrand <- make_hyperrand(mod)
  ## spline
  ans_spline <- make_spline(mod = mod, effectfree = effectfree)
  ans_spline <- rvec::rvec_dbl(ans_spline)
  ## svd
  ans_svd <- make_svd(mod = mod, effectfree = effectfree)
  ans_svd <- rvec::rvec_dbl(ans_svd)
  ## combine
  ans <- c(ans_effects, ans_hyper, ans_hyperrand, ans_spline, ans_svd)
  ## disp
  if (has_disp(mod)) {
    ans_disp <- get_disp(mod)
    ans <- c(ans, ans_disp)
  }
  ## return
  ans <- unname(ans)
  ans
}


## HAS_TESTS
#' Make Draws for Dispersion Parameter
#'
#' Includes transforming back to natural units.
#'
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A numeric vector.
#' 
#' @noRd
make_draws_disp <- function(draws_post) {
  n_val <- nrow(draws_post)
  ans <- draws_post[n_val, ]
  ans <- exp(ans)
  ans
}
 

## HAS_TESTS
#' Make Draws from Free Effect Parameters
#'
#' Make draws from 'effectfree' (as opposed
#' to 'effect', the values that users see.)
#
#' @param est Named list. Output from TMB.
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_effectfree <- function(est, draws_post) {
  n_effectfree <- length(est$effectfree)
  i_effectfree <- seq_len(n_effectfree)
  draws_post[i_effectfree, , drop = FALSE]
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters
#'
#' Does not include hyperrand or disp.
#' Includes transforming back to natural units.
#'
#' @param est Named list. Output from TMB.
#' @param transforms_hyper List of transforms to be
#' applied to hyper-parameters
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyper <- function(est, transforms_hyper, draws_post) {
  n_effectfree <- length(est$effectfree)
  n_hyper <- length(est$hyper)
  i_hyper <- seq.int(from = n_effectfree + 1L, length.out = n_hyper)
  ans <- draws_post[i_hyper, , drop = FALSE]
  for (i in seq_along(transforms_hyper)) {
    transform <- transforms_hyper[[i]]
    if (!is.null(transform))
      ans[i, ] <- transform(ans[i, ])
  }
  ans
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters that can be
#' Treated as Random Effects
#'
#' @param est Named list. Output from TMB.
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyperrandfree <- function(est, draws_post) {
  n_effectfree <- length(est$effectfree)
  n_hyper <- length(est$hyper)
  n_hyperrandfree <- length(est$hyperrandfree)
  i_hyperrandfree <- seq.int(from = n_effectfree + n_hyper + 1L,
                             length.out = n_hyperrandfree)
  draws_post[i_hyperrandfree, , drop = FALSE]
}


## HAS_TESTS
#' Construct Estimates of Effects from Estimates of Free Parameters
#'
#' @param mod A fitted object of class "bage_mod"
#' @param effectfree Posterior draws for free parameters
#'
#' @returns A matrix
#' 
#' @noRd
make_effects <- function(mod, effectfree) {
  matrix_effectfree_effect <- make_combined_matrix_effectfree_effect(mod)
  offset_effectfree_effect <- make_combined_offset_effectfree_effect(mod)
  ans <- matrix_effectfree_effect %*% effectfree + offset_effectfree_effect
  ans <- Matrix::as.matrix(ans)
  ans
}


## HAS_TESTS
#' Make Initial Draws from Posterior Distribution
#'
#' Make draws of variables included in 'prec'
#' matrix of TMB model output.
#'
#' If the Cholesky decomposition of
#' 'prec' is successful, then use that.
#' Otherwise, use the eigen decomposition.
#' Insert values for parameters that were
#' fixed via the 'map' function
#'
#' @param est Named list with estimates returned by TMB
#' @param prec Precision matrix returned by TMB
#' @param map 'map' argument to TMB
#' @param n_draw Number of posterior draws
#'
#' @returns A matrix
#'
#' @noRd
make_draws_post <- function(est, prec, map, n_draw) {
  is_fixed <- make_is_fixed(est = est, map = map)
  est_unlist <- unlist(est)
  mean <- est_unlist[!is_fixed]
  CH <- Matrix::Cholesky(prec)
  is_sparse <- methods::is(CH, "dCHMsimpl") || methods::is(CH, "dCHMsuper")
  if (is_sparse) {
    t_draws_nonfixed <- sparseMVN::rmvn.sparse(n = n_draw,
                                               mu = mean,
                                               CH = CH,
                                               prec = TRUE)
    draws_nonfixed <- t(t_draws_nonfixed)
  }
  else {
    R_prec <- Matrix::expand1(CH, which = "L")
    draws_nonfixed <- rmvnorm_chol(n = n_draw,
                                   mean = mean,
                                   R_prec = R_prec)
  }
  ans <- matrix(nrow = length(is_fixed), ncol = n_draw)
  ans[!is_fixed, ] <- draws_nonfixed
  ans[is_fixed, ] <- est_unlist[is_fixed]
  ans
}


#' Make Values for Hyperrand
#'
#' Includes converting from unconstrained to constrained where necessary
#'
#' @param mod Fitted object of class 'bage_mod'
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  n_draw <- mod$n_draw
  hyperrandfree <- mod$draws_hyperrandfree
  effectfree <- mod$draws_effectfree
  terms_hyperrandfree <- make_terms_hyperrandfree(mod)
  terms_effectfree <- make_terms_effectfree(mod)
  hyperrandfree <- split(hyperrandfree, terms_hyperrandfree)
  effectfree <- split(effectfree, terms_effectfree)
  hyperrandfree <- lapply(hyperrandfree, matrix, ncol = n_draw)
  effectfree <- lapply(effectfree, matrix, ncol = n_draw)
  hyperrandfree <- lapply(hyperrandfree, rvec::rvec_dbl)
  effectfree <- lapply(effectfree, rvec::rvec_dbl)
  ans <- .mapply(make_hyperrand_one,
                 dots = list(prior = priors,
                             hyperrandfree = hyperrandfree,
                             effectfree = effectfree,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender))
  vctrs::vec_c(!!!ans)
}


## HAS_TESTS
#' Derive Values for Hyper-Paramers Involving Lines
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand_lin <- function(prior,
                               hyperrandfree,
                               effectfree,
                               dimnames_term,
                               var_time,
                               var_age,
                               var_sexgender) {
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  matrix_effectfree_effect <- make_matrix_effectfree_effect(prior = prior,
                                                            dimnames_term = dimnames_term,
                                                            var_time = var_time,
                                                            var_age = var_age,
                                                            var_sexgender = var_sexgender)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  v <- seq_len(n_along) - 0.5 * (n_along + 1)
  n_draw <- rvec::n_draw(hyperrandfree)
  trend <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    i_along <- matrix_along_by_effectfree[, i_by] + 1L
    trend[i_along] <- hyperrandfree[[i_by]] * v
  }
  error <- effectfree - trend
  trend <- as.matrix(trend) ## to cope with sparse matrix
  error <- as.matrix(error) ## to cope with sparse matrix
  trend <- matrix_effectfree_effect %*% trend
  error <- matrix_effectfree_effect %*% error
  trend <- rvec::rvec_dbl(as.matrix(trend))
  error <- rvec::rvec_dbl(as.matrix(error))
  ## calculate slope on constrained space
  n_by_constr <- ncol(matrix_along_by_effect)
  slope <- rvec::new_rvec(length = n_by_constr, n_draw = n_draw)
  for (i_by in seq_len(n_by_constr)) {
    i_1 <- matrix_along_by_effect[1L, i_by] + 1L
    i_2 <- matrix_along_by_effect[2L, i_by] + 1L
    slope[[i_by]] <- trend[[i_2]] - trend[[i_1]]
  }
  vctrs::vec_c(slope, trend, error)
}



## HAS_TESTS
#' Derive Values for Hyper-Parameters Involving Fixed Seasonal Effects and
#' Random Initial Value Random Walk
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand_randomseasfix <- function(prior,
                                         hyperrandfree,
                                         effectfree,
                                         dimnames_term,
                                         var_time,
                                         var_age,
                                         var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  matrix_effectfree_effect <- make_matrix_effectfree_effect(prior = prior,
                                                            dimnames_term = dimnames_term,
                                                            var_time = var_time,
                                                            var_age = var_age,
                                                            var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  n_draw <- rvec::n_draw(hyperrandfree)
  season <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    sum_seas <- 0
    for (i_along in seq_len(n_along)) {
      i_season <- matrix_along_by_effectfree[i_along, i_by] + 1L
      index_seas <- (i_along - 1L) %% n_seas
      is_final_seas <- index_seas == n_seas - 1L
      if (!is_final_seas) {
        i_hyper <- index_seas + (i_by - 1L) * (n_seas - 1L) + 1L
        season[[i_season]] <- hyperrandfree[[i_hyper]]
        sum_seas <- sum_seas + hyperrandfree[[i_hyper]]
      }
      else {
        season[[i_season]] <- -sum_seas
        sum_seas <- 0
      }
    }
  }
  trend <- effectfree - season
  trend <- as.matrix(trend) ## to cope with sparse matrix
  season <- as.matrix(season) ## to cope with sparse matrix
  trend <- matrix_effectfree_effect %*% trend
  season <- matrix_effectfree_effect %*% season
  trend <- rvec::rvec_dbl(as.matrix(trend))
  season <- rvec::rvec_dbl(as.matrix(season))
  vctrs::vec_c(trend, season)
}

  
## HAS_TESTS
#' Derive Values for Hyper-Parameters Involving Varying Seasonal Effects
#' and Random Initial Value Random Walk
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand_randomseasvary <- function(prior,
                                          hyperrandfree,
                                          effectfree,
                                          dimnames_term,
                                          var_time,
                                          var_age,
                                          var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  matrix_effectfree_effect <- make_matrix_effectfree_effect(prior = prior,
                                                            dimnames_term = dimnames_term,
                                                            var_time = var_time,
                                                            var_age = var_age,
                                                            var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  n_along_hyper <- length(hyperrandfree) %/% n_by
  n_draw <- rvec::n_draw(hyperrandfree)
  season <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    i_along_hyper <- 1L
    sum_seas <- 0
    for (i_along in seq_len(n_along)) {
      index_seas <- (i_along - 1L) %% n_seas
      is_last_seas <- index_seas == n_seas - 1L
      i_season <- matrix_along_by_effectfree[i_along, i_by] + 1L
      if (!is_last_seas) {
        i_hyper <- i_along_hyper + (i_by - 1L) * n_along_hyper
        i_along_hyper <- i_along_hyper + 1L
        season[[i_season]] <- hyperrandfree[[i_hyper]]
        sum_seas <- sum_seas + hyperrandfree[[i_hyper]]
      }
      if (is_last_seas) {
        season[[i_season]] <- -sum_seas
        sum_seas <- 0
      }
    }
  }
  trend <- effectfree - season
  trend <- as.matrix(trend) ## to cope with sparse matrix
  season <- as.matrix(season) ## to cope with sparse matrix
  trend <- matrix_effectfree_effect %*% trend
  season <- matrix_effectfree_effect %*% season
  trend <- rvec::rvec_dbl(as.matrix(trend))
  season <- rvec::rvec_dbl(as.matrix(season))
  vctrs::vec_c(trend, season)
}





## HAS_TESTS
#' Derive Values for Hyper-Parameters Involving Fixed Seasonal Effects and
#' Zero Initial Value Random Walk
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand_zeroseasfix <- function(prior,
                                   hyperrandfree,
                                   effectfree,
                                   dimnames_term,
                                   var_time,
                                   var_age,
                                   var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  matrix_effectfree_effect <- make_matrix_effectfree_effect(prior = prior,
                                                            dimnames_term = dimnames_term,
                                                            var_time = var_time,
                                                            var_age = var_age,
                                                            var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  n_draw <- rvec::n_draw(hyperrandfree)
  season <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    sum_seas <- 0
    for (i_along in seq_len(n_along)) {
      i_season <- matrix_along_by_effectfree[i_along, i_by] + 1L
      if (i_along == 1L) {
        effectfree_first <- effectfree[[i_season]]
        season[[i_season]] <- effectfree_first
      }
      else if (i_along < n_seas) {
        i_hyper <- i_along - 1L + (i_by - 1L) * (n_seas - 2L)
        season[[i_season]] <- effectfree_first + hyperrandfree[[i_hyper]]
        sum_seas <- sum_seas + hyperrandfree[[i_hyper]]
      }
      else if (i_along == n_seas) {
        season[[i_season]] <- -sum_seas - (n_seas - 1) * effectfree_first
      }
      else {
        i_season_prev <- matrix_along_by_effectfree[i_along - n_seas, i_by] + 1L
        season[[i_season]] <- season[[i_season_prev]]
      }
    }
  }
  trend <- effectfree - season
  trend <- as.matrix(trend) ## to cope with sparse matrix
  season <- as.matrix(season) ## to cope with sparse matrix
  trend <- matrix_effectfree_effect %*% trend
  season <- matrix_effectfree_effect %*% season
  trend <- rvec::rvec_dbl(as.matrix(trend))
  season <- rvec::rvec_dbl(as.matrix(season))
  vctrs::vec_c(trend, season)
}

  
## HAS_TESTS
#' Derive Values for Hyper-Parameters Involving Varying Seasonal Effects
#' and Zero Initial Value Random Walk
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec
#'
#' @noRd
make_hyperrand_zeroseasvary <- function(prior,
                                        hyperrandfree,
                                        effectfree,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  matrix_effectfree_effect <- make_matrix_effectfree_effect(prior = prior,
                                                            dimnames_term = dimnames_term,
                                                            var_time = var_time,
                                                            var_age = var_age,
                                                            var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  n_along_hyper <- length(hyperrandfree) %/% n_by
  n_draw <- rvec::n_draw(hyperrandfree)
  season <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    i_effectfree_first <- matrix_along_by_effectfree[1L, i_by] + 1L
    effectfree_first <- effectfree[[i_effectfree_first]]
    i_along_hyper <- 1L
    sum_seas <- 0
    for (i_along in seq_len(n_along)) {
      is_first_element <- i_along == 1L
      index_seas <- (i_along - 1L) %% n_seas
      is_last_seas <- index_seas == n_seas - 1L
      i_season <- matrix_along_by_effectfree[i_along, i_by] + 1L
      if (is_first_element)
        season[[i_season]] <- effectfree_first
      if (!is_first_element && !is_last_seas) {
        i_hyper <- i_along_hyper + (i_by - 1L) * n_along_hyper
        i_along_hyper <- i_along_hyper + 1L
        season[[i_season]] <- hyperrandfree[[i_hyper]] + effectfree_first
        sum_seas <- sum_seas + hyperrandfree[[i_hyper]]
      }
      if (is_last_seas) {
        season[[i_season]] <- -sum_seas - (n_seas - 1) * effectfree_first
        sum_seas <- 0
      }
    }
  }
  trend <- effectfree - season
  trend <- as.matrix(trend) ## to cope with sparse matrix
  season <- as.matrix(season) ## to cope with sparse matrix
  trend <- matrix_effectfree_effect %*% trend
  season <- matrix_effectfree_effect %*% season
  trend <- rvec::rvec_dbl(as.matrix(trend))
  season <- rvec::rvec_dbl(as.matrix(season))
  vctrs::vec_c(trend, season)
}


## HAS_TESTS
#' Make logical vector indicating whether
#' an element of 'est' is fixed
#'
#' @param est Mean values for posterior.
#' Returned by TMB function 'sdreport()'.
#' @param Named list or NULL. Argument to
#' TMB function 'MakeADFun()'.
#'
#' @returns A logical vector
#' 
#' @noRd
make_is_fixed <- function(est, map) {
    if (is.null(map)) {
        n <- length(unlist(est))
        ans <- rep(FALSE, times = n)
    }
    else {
        ans <- est
        nms_est <- names(est)
        nms_map <- names(map)
        for (nm in nms_est) {
            if (nm %in% nms_map)
                ans[[nm]] <- is.na(map[[nm]])
            else
                ans[[nm]] <- rep(FALSE, times = length(ans[[nm]]))
        }
        ans <- unlist(ans)
    }
    ans
}


## HAS_TESTS
#' Make variable identifying level within each component
#' of 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_level_components <- function(mod) {
  dimnames_terms <- mod$dimnames_terms
  effect <- make_levels_effects(dimnames_terms)
  hyper <- make_levels_hyper(mod)
  hyperrand <- make_levels_hyperrand(mod, unlist = TRUE)
  spline <- make_levels_spline(mod, unlist = TRUE)
  svd <- make_levels_svd(mod, unlist = TRUE)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  spline <- as.character(spline)
  svd <- as.character(svd)
  ans <- c(effect, hyper, hyperrand, spline, svd)
  if (has_disp(mod))
    ans <- c(ans, "disp")
  ans
}


## HAS_TESTS
#' Make levels associated with each element of 'hyper'
#'
#' Make levels for hyperparameters for each term
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_hyper <- function(mod) {
  priors <- mod$priors
  ans <- lapply(priors, levels_hyper)
  ans <- unlist(ans)
  ans
}


## HAS_TESTS
#' Make Levels Associated with Each Element of 'hyperrand'
#'
#' Make levels for hyperparameters for each term
#'
#' @param mod An object of class 'bage_mod'.
#' @param unlist Whether to return as list or vector.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_hyperrand <- function(mod, unlist) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  ans <- .mapply(levels_hyperrand,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age))
  if (unlist)
    ans <- unlist(ans)
  ans
}


## HAS_TEST
#' Make labels for replicate datasets
#'
#' @param n Number of replicate datasets
#' @param n_row_data Number of rows in a single
#' replicate dataset
#'
#' @returns A character vector with length n x n_row_dataset
#'
#' @noRd
make_levels_replicate <- function(n, n_row_data) {
    ans <- paste("Replicate", seq_len(n))
    ans <- c("Original", ans)
    ans <- factor(rep(ans, each = n_row_data),
                  levels = ans)
    ans
}


## HAS_TESTS
#' Make Levels for 'spline' Coefficients
#'
#' @param mod An object of class 'bage_mod'.
#' @param unlist Whether to return a charcter vector
#'
#' @returns A named list or a character vector
#'
#' @noRd
make_levels_spline <- function(mod, unlist) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  ans <- vector(mode = "list", length = length(priors))
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_spline(prior)) {
      dimnames_term <- dimnames_terms[[i]]
      ans[[i]] <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age)
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make Levels for Spline
#'
#' @param prior Object of class 'bage_prior_spline'
#' @param dimnames_term Dimnames for array representation
#' of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A character vector
#'
#' @noRd
make_levels_spline_term <- function(prior,
                                    dimnames_term,
                                    var_time,
                                    var_age) {
  con <- prior$specific$con
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  levels_along <- paste0("comp", seq_len(n_comp))
  dimnames_term[[i_along]] <- levels_along
  if (con == "by") {
    dimnames_term[-i_along] <- make_unconstr_dimnames_by(i_along = i_along,
                                                         dimnames_term = dimnames_term)
  }
  dimnames_to_levels(dimnames_term)
}


## HAS_TESTS
#' Make Levels for 'svd' Coefficients
#'
#' @param mod An object of class 'bage_mod'.
#'
#' @returns A named list.
#'
#' @noRd
make_levels_svd <- function(mod, unlist) {
  priors <- mod$priors
  nms_priors <- names(priors)
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  dimnames_terms <- mod$dimnames_terms
  ans <- vector(mode = "list", length = length(priors))
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_svd(prior)) {
      dimnames_term <- dimnames_terms[[i]]
      ans[[i]] <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make Levels for 'svd' Coefficients for a Single Term
#'
#' Term is assumed to have a SVD-based prior
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array representation of term
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_svd_term <- function(prior,
                                 dimnames_term,
                                 var_time,
                                 var_age,
                                 var_sexgender) {
  labels_svd <- get_labels_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender)
  nms <- names(dimnames_term)
  nms_noagesex <- setdiff(nms, c(var_age, var_sexgender))
  dimnames_noagesex <- dimnames_term[nms_noagesex]
  if (uses_along(prior)) {
    con <- prior$specific$con
    if (con == "by") {
      i_along <- match(var_time, names(dimnames_noagesex))
      dimnames_noagesex[-i_along] <- make_unconstr_dimnames_by(i_along = i_along,
                                                               dimnames_term = dimnames_noagesex)
    }
  }
  dimnames_svd <- c(list(.svd = labels_svd), dimnames_noagesex)
  dimnames_to_levels(dimnames_svd)
}


## HAS_TESTS
#' Calculate Trend Values for a Line or Lines
#'
#' @param slope Slope(s) of line(s). An rvec.
#' @param matrix_along_by Matrix mapping
#' along and by dimensions to position in estimates
#'
#' @returns An rvec
#'
#' @noRd
make_lin_trend <- function(slope,
                           matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  slope <- rep(slope, each = n_along)
  intercept <- -0.5 * (n_along + 1) * slope
  s <- rep(seq_len(n_along), times = n_by)
  ans <- intercept + slope * s
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans[i]
}


#' Make Linear Predictor from Components
#'
#' @param components Data frame with estimates for hyper-parameters
#' @param data Data frame with raw data
#' @param dimnames_terms Dimnames for array representation of terms
#'
#' @returns An rvec
#'
#' @noRd
make_linpred_comp <- function(components, data, dimnames_terms) {
  key_comp <- with(components, paste(term, component, level))
  fitted <- components$.fitted
  data_has_intercept <- "(Intercept)" %in% names(data)
  if (!data_has_intercept)
    data[["(Intercept)"]] <- "(Intercept)"
  n_draw <- rvec::n_draw(fitted)
  ans <- matrix(0, nrow = nrow(data), ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  for (i_term in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    nm <- dimnames_to_nm(dimnames_term)
    levels_term <- dimnames_to_levels(dimnames_term)
    key_term <- paste(nm, "effect", levels_term)
    indices_comp <- match(key_term, key_comp)
    val_term <- fitted[indices_comp]
    levels_data <- Reduce(paste_dot, data[nm_split])
    indices_term <- match(levels_data, levels_term)
    val_term_linpred <- val_term[indices_term]
    ans <- ans + val_term_linpred
  }
  ans
}


## HAS_TESTS
#' Make Linear Predictor from 'draws_effectfree'
#'
#' Can only be used with fitted models.
#'
#' Return value aligned to outcome, not data.
#'
#' @param mod Object of class "bage_mod"
#' @param point Whether to return point estimates
#' or draws from the posterior.
#'
#' @returns An rvec if 'point' is FALSE, otherwise a vector of doubles
#'
#' @noRd
make_linpred_raw <- function(mod, point) {
  matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
  if (point)
    effectfree <- mod$point_effectfree
  else
    effectfree <- mod$draws_effectfree
  effect <- make_effects(mod = mod, effectfree = effectfree)
  ans <- matrix_effect_outcome %*% effect
  if (point)
    ans <- as.double(ans)
  else {
    ans <- Matrix::as.matrix(ans)
    ans <- rvec::rvec(ans)
  }
  ans
}


## HAS_TESTS
#' Derive Point Estimates for Effects
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list of numeric vectors
#'
#' @noRd
make_point_est_effects <- function(mod) {
  if (!is_fitted(mod))
    cli::cli_abort("Internal error: Model not fitted.")
  point_effectfree <- mod$point_effectfree
  dimnames_terms <- mod$dimnames_terms
  terms_effects <- make_terms_effects(dimnames_terms)
  point_effects <- make_effects(mod = mod, effectfree = point_effectfree)
  point_effects <- as.double(point_effects)
  ans <- split(x = point_effects, f = terms_effects)
  ans <- ans[unique(terms_effects)] ## 'split' orders result
  ans
}


## HAS_TESTS
#' Use a precision matrix to construct scaled eigen vectors
#'
#' The scaled eigen vectors can be used to draw from a
#' multivariate normal distribution with the given
#' pecision matrix.
#'
#' @param prec A symmetric positive definite matrix.
#'
#' @returns A matrix with the same dimensions as 'prec'
#'
#' @noRd
make_scaled_eigen <- function(prec) {
    tolerance <- 1e-6
    eigen <- eigen(prec, symmetric = TRUE)
    vals <- eigen$values
    vecs <- eigen$vectors
    min_valid_val <- -tolerance * abs(vals[[1L]]) ## based on MASS::mvrnorm
    if (any(vals < min_valid_val))
        cli::cli_abort("Estimated precision matrix not positive definite.")  ## nocov
    vals <- pmax(vals, abs(min_valid_val))
    sqrt_inv_vals <- sqrt(1 / vals)
    vecs %*% diag(sqrt_inv_vals)
}


## HAS_TESTS
#' Extract Posterior Draws for Free Parameters used in Spline Priors
#'
#' @param mod Fitted object of class 'bage_mod'
#' @param mod effectfree Matrix with posterior draws of free parameters
#'
#' @returns A matrix
#'
#' @noRd
make_spline <- function(mod, effectfree) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  n_term <- length(priors)
  ans <- vector(mode = "list", length = n_term)
  lengths_effectfree <- make_lengths_effectfree(mod)
  to <- 0L
  for (i_term in seq_len(n_term)) {
    length_effectfree <- lengths_effectfree[[i_term]]
    to <- to + length_effectfree
    prior <- priors[[i_term]]
    dimnames_term <- dimnames_terms[[i_term]]
    if (is_spline(prior)) {
      s <- seq.int(to = to, length.out = length_effectfree)
      vals <- effectfree[s, , drop = FALSE]
      ans[[i_term]] <- vals
    }
  }
  ans <- do.call(rbind, ans)
  ans
}


#' Extract Posterior Draws for Free Parameters used in SVD Priors
#'
#' Where necessary, append zeros to start of time dimension.
#'
#' @param mod Fitted object of class 'bage_mod'
#' @param mod effectfree Matrix with posterior draws of free parameters
#'
#' @returns A matrix
#'
#' @noRd
make_svd <- function(mod, effectfree) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  n_term <- length(priors)
  ans <- vector(mode = "list", length = n_term)
  lengths_effectfree <- make_lengths_effectfree(mod)
  to <- 0L
  for (i_term in seq_len(n_term)) {
    length_effectfree <- lengths_effectfree[[i_term]]
    to <- to + length_effectfree
    prior <- priors[[i_term]]
    dimnames_term <- dimnames_terms[[i_term]]
    m <- make_matrix_draws_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)
    if (!is.null(m)) {
      s <- seq.int(to = to, length.out = length_effectfree)
      vals <- effectfree[s, , drop = FALSE]
      vals <- m %*% vals
      vals <- Matrix::as.matrix(vals)
      ans[[i_term]] <- vals
    }
  }
  ans <- do.call(rbind, ans)
  ans
}


## HAS_TESTS
#' Make Factor Identifying Components
#' of Spline Parameter Vector
#'
#' Make factor the same length as
#' the estimates of spline coefficients,
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A factor.
#'
#' @noRd
make_term_spline <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  is_spline <- vapply(priors, is_spline, FALSE)
  nms_terms_spline <- nms_terms[is_spline]
  levels_spline <- make_levels_spline(mod, unlist = FALSE)
  lengths_spline <- lengths(levels_spline[is_spline])
  ans <- rep(nms_terms_spline, times = lengths_spline)
  ans <- factor(ans, levels = nms_terms_spline)
  ans
}


## HAS_TESTS
#' Make Factor Identifying Components
#' of SVD Parameter Vector
#'
#' Make factor the same length as
#' the estimates of SVD coefficients,
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A factor.
#'
#' @noRd
make_term_svd <- function(mod) {
  priors <- mod$priors
  levels <- make_levels_svd(mod, unlist = FALSE)
  lengths_levels <- lengths(levels)
  nms_terms <- names(priors)
  is_svd <- vapply(priors, is_svd, FALSE)
  lengths_svd <- lengths_levels[is_svd]
  nms_svd <- nms_terms[is_svd]
  ans <- rep(nms_svd, times = lengths_svd)
  ans <- factor(ans, levels = nms_svd)
  ans
}


## HAS_TESTS
#' Make factor identifying components of 'hyperrand'
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'hyper'.
#'
#' @noRd
make_terms_hyperrand <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  levels <- make_levels_hyperrand(mod, unlist = FALSE)
  lengths <- lengths(levels)
  ans <- rep(nms_terms, times = lengths)
  ans <- factor(ans, levels = nms_terms)
  ans
}


## HAS_TESTS
#' Make Dimnames for Unconstrainted Version of By Dimensions
#'
#' @param i_along Index of 'along' dimension
#' @param dimnames_term Dimnames for array representation of term
#'
#' @returns A named list
#'
#' @noRd
make_unconstr_dimnames_by <- function(i_along, dimnames_term) {
  ans <- dimnames_term[-i_along]
  nms <- names(ans)
  for (i in seq_along(ans))
    ans[[i]] <- paste0(nms[[i]], seq_along(ans[[i]][-1L]))
  ans
}


#' Paste Two Vectors Separated by a Dot
#'
#' @param x A vector
#' @param y A vector
#'
#' @returns A character vector
#'
#' @noRd
paste_dot <- function(x, y) paste(x, y, sep = ".")


#' Given Estimates of Effect and Slope, Rescale Intercept for Lines
#'
#' @param slope Rvec of length n_by holding slope estimates
#' @param effect Rvec holding estimates for effect
#' @param matrix_along_by Mapping matrix
#'
#' @returns An rvec of length 'n_by'
#'
#' @noRd
rescale_lin_intercept <- function(slope, effect, matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- slope
  for (i_by in seq_len(n_by)) {
    i_along <- matrix_along_by[, i_by] + 1L
    mean_effect <- mean(effect[i_along])
    mean_incr <- 0.5 * (1 + n_along) * slope[[i_by]]
    ans[[i_by]] <- mean_effect - mean_incr
  }
  ans
}


## HAS_TESTS
#' Make Draws Stored as Part of Model Object
#'
#' Draws created are
#' - 'draws_effectfree',
#' - 'draws_hyper',
#' - 'draws_hyperrandfree', and, optionally,
#' - 'draws_disp'.
#'
#' Reproducibility is achieved via 'seed_stored_draws'.
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
make_stored_draws <- function(mod, est, prec, map) {
  n_draw <- mod$n_draw
  transforms_hyper <- make_transforms_hyper(mod)
  seed_stored_draws <- mod$seed_stored_draws
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_stored_draws) ## set pre-determined seed
  draws_post <- make_draws_post(est = est,
                                prec = prec,
                                map = map,
                                n_draw = n_draw)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  mod$draws_effectfree <- make_draws_effectfree(est = est,
                                                draws_post = draws_post)
  mod$draws_hyper <- make_draws_hyper(est = est,
                                      transforms_hyper = transforms_hyper,
                                      draws_post = draws_post)
  mod$draws_hyperrandfree <- make_draws_hyperrandfree(est = est,
                                                      draws_post = draws_post)
  if (has_disp(mod))
    mod$draws_disp <- make_draws_disp(draws_post)
  mod
}


## HAS_TESTS
#' Make Point Estimates Stored as Part of Model Object
#'
#' Draws created are
#' - 'point_effectfree',
#' - 'point_hyper',
#' - 'point_hyperrandfree', and, optionally,
#' - 'point_disp'.
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
make_stored_point <- function(mod, est) {
  transforms_hyper <- make_transforms_hyper(mod)
  mod$point_effectfree <- est$effectfree
  point_hyper <- est$hyper
  for (i in seq_along(point_hyper))
    point_hyper[[i]] <- transforms_hyper[[i]](point_hyper[[i]])
  mod$point_hyper <- point_hyper
  mod$point_hyperrandfree <- est$hyperrandfree
  if (has_disp(mod))
    mod$point_disp <- exp(est$log_disp)
  mod
}


## HAS_TESTS
#' Make variable identifying term within each component
#' of 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_term_components <- function(mod) {
  dimnames_terms <- mod$dimnames_terms
  effect <- make_terms_effects(dimnames_terms)
  hyper <- make_terms_hyper(mod)
  hyperrand <- make_terms_hyperrand(mod)
  spline <- make_term_spline(mod)
  svd <- make_term_svd(mod)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  spline <- as.character(spline)
  svd <- as.character(svd)
  ans <- c(effect, hyper, hyperrand, spline, svd)
  if (has_disp(mod))
    ans <- c(ans, "disp")
  ans
}


## HAS_TESTS
#' Make List of Transforms to be Applied to
#' Hyper-Parameters
#'
#' Does not include hyper-parameters
#' that can be treated as random effects
#'  ("hyperrand") or dispersion.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list, each element of which is a function or NULL
#'
#' @noRd
make_transforms_hyper <- function(mod) {
  priors <- mod$priors
  ans <- lapply(priors, transform_hyper)
  ans <- unlist(ans, recursive = FALSE)
  ans
}


## HAS_TESTS
#' Reformat Parts of Forecasted 'components' Data Frame Dealing with
#' Hyper-Parameters that are Treated as Random Effects
#'
#' @param components_forecast A data frame with forecasted
#' (time-varying) parameters
#' @param components A data frame with estimated
#' (time-varying and non-time-varying) parameters
#' @param priors List of objects of class 'bage_prior'.
#' @param dimnames_terms Dimnames for array representations of terms
#' being forecast
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A modified version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_forecast <- function(components,
                                              priors,
                                              dimnames_terms,
                                              var_time,
                                              var_age) {
  nms <- names(dimnames_terms)
  for (nm in nms) {
    prior <- priors[[nm]]
    dimnames_term <- dimnames_terms[[nm]]
    components <- infer_trend_cyc_seas_err_forecast_one(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age,
                                                        components = components)
  }
  components
}


## HAS_TESTS
#' Derive 'Components' Output for Terms with Fixed Seasonal Effect
#' - Forecasts
#'
#' Derive 'seasonal' from 'effect' and 'trend'
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasfix_forecast <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_season <- with(components, (term == nm) & (component == "season"))
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  trend <- components$.fitted[is_trend]
  effect <- components$.fitted[is_effect]
  season <- effect - trend
  components$.fitted[is_season] <- season
  components
}


## HAS_TESTS
#' Derive 'Components' Output for Terms with Varying Seasonal Effect - Forecasts
#'
#' Derive 'seasonal' from 'effect' and 'trend'
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasvary_forecast <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_season <- with(components, (term == nm) & (component == "season"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  trend <- components$.fitted[is_trend]
  effect <- components$.fitted[is_effect]
  season <- effect - trend
  components$.fitted[is_season] <- season
  components
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from a Cholesky decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param R_prec Cholesky decomposition of precision matrix
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_chol <- function(n, mean, R_prec) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + backsolve(R_prec, Z)
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from an eigen decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param scaled_eigen Matrix of scaled eigenvalues
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_eigen <- function(n, mean, scaled_eigen) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + scaled_eigen %*% Z
}


## HAS_TESTS
#' Convert Rvec Columns to Numeric Columns by Taking Means
#'
#' @param data A data frame
#'
#' @return A data frame
#'
#' @noRd
rvec_to_mean <- function(data) {
  is_rvec <- vapply(data, rvec::is_rvec, TRUE)
  data[is_rvec] <- lapply(data[is_rvec], rvec::draws_mean)
  data
}


## HAS_TESTS
#' Order 'components' Data Frame by 'term' and 'component'
#'
#' @param components A tibble - typically the output
#' from function 'components'
#'
#' @returns A reordered version of 'components'
#'
#' @noRd
sort_components <- function(components, mod) {
  levels_component <- c("effect",
                        "trend", "season", "error",
                        "spline", "svd",
                        "disp",
                        "hyper")
  formula <- mod$formula
  term <- components$term
  component <- components$component
  terms_formula <- stats::terms(formula)
  levels_term <- attr(terms_formula, "term.labels")
  if (attr(terms_formula, "intercept"))
    levels_term <- c("(Intercept)", levels_term)
  i_term <- match(term, levels_term)
  i_comp <- match(components$component, levels_component, nomatch = 0L)
  i_invalid_comp <- match(0L, i_comp, nomatch = 0L)
  if (i_invalid_comp > 0L) {
    val <- components$component[[i_invalid_comp]]
    cli::cli_abort("Internal error: {.val {val}} not a valid value for {.var component}.")  ## nocov
  }
  ord <- order(i_term, i_comp)
  components[ord, , drop = FALSE]
}
  

## HAS_TESTS
#' Transfer Draws for Dispersion between Models
#'
#'
#' @param mod Model receiving the draws
#' @param mod_disp Model giving the draws
#'
#' @returns Modified version of 'mod'
#' 
#' @noRd
transfer_draws_disp <- function(mod, mod_disp) { 
  mod$draws_disp <- mod_disp$draws_disp
  mod
}


## HAS_TESTS
#' Create Functions Needed to Transform Hyper-Parameters
#' from AR-Based Prior
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns A named list
#'
#' @noRd
transform_hyper_ar <- function(prior) {
  specific <- prior$specific
  n_coef <- specific$n_coef
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n_coef, 1L))
}
