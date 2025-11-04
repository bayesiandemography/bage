
## Helper functions for 'fit' method for 'bage_mod' objects

## HAS_TESTS
#' Record Draws and Point Estimates for Parameters Estimated in TMB
#'
#' @param mod Object of class 'bage_mod'
#' @param est Point estimates
#' @param prec Precision matrix
#' @param map Named list specifying parameters held fixed
#' @param max_jitter Maximum amount added to matrix
#' to enable Cholesky factorization
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
draw_vals_and_record <- function(mod, est, prec, map, max_jitter) {
  mod <- make_stored_draws(mod = mod,
                           est = est,
                           prec = prec,
                           map = map,
                           max_jitter)
  mod <- make_stored_point(mod = mod,
                           est = est)
  mod
}


## HAS_TESTS
#' Extract Point Estimates and Precision Matrix from TMB
#'
#' Note that TMB modifies 'f' in place - adding estimates
#' and precision/variance information to function.
#'
#' @param f Function created by 'makeADFun'
#' @param has_random_effects Whether model has random effects
#'
#' @returns Named list
#'
#' @noRd
extract_est_prec <- function(f, has_random_effects) {
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
  list(est = est,
       prec = prec)
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
#' @param max_jitter Maximum amount to add to allow
#' Cholesky factorization
#' @param start_oldpar Whether to start from old parameter values
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_default <- function(mod,
                        aggregate,
                        optimizer,
                        quiet,
                        max_jitter,
                        start_oldpar) {
  t_start <- Sys.time()
  if (is_not_testing_or_snapshot())
    cli::cli_progress_message("Building log-posterior function...") # nocov
  if (start_oldpar) {
    if (is_fitted(mod))
      oldpar <- mod$oldpar
    else
      cli::cli_abort("{.arg start_oldpar} is {.val {start_oldpar}} but model has not been fitted.")
  }
  mod <- unfit(mod)
  data <- make_fit_data(mod = mod, aggregate = aggregate)
  parameters <- if (start_oldpar) oldpar else make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  t_optim <- Sys.time()
  if (is_not_testing_or_snapshot())
    cli::cli_progress_message("Finding maximum...") # nocov
  optimizer_out <- optimize_adfun(f = f,
                                  quiet = quiet,
                                  optimizer = optimizer,
                                  data = data,
                                  random = random,
                                  map = map,
                                  is_test_nonconv = FALSE)
  t_report <- Sys.time()
  if (is_not_testing_or_snapshot())
    cli::cli_progress_message("Drawing values for hyper-parameters...") # nocov
  est_prec <- extract_est_prec(f = optimizer_out$f,
                               has_random_effects = has_random_effects)
  est <- est_prec$est
  prec <- est_prec$prec
  mod <- draw_vals_and_record(mod = mod,
                              est = est,
                              prec = prec,
                              map = map,
                              max_jitter = max_jitter)
  t_end <- Sys.time()
  times <- make_fit_times(t_start = t_start,
                          t_optim = t_optim,
                          t_report = t_report,
                          t_end = t_end)
  mod <- record_metadata(mod = mod,
                         est = est,
                         optimizer = optimizer_out$optimizer,
                         iter = optimizer_out$iter,
                         converged = optimizer_out$converged,
                         message = optimizer_out$message,
                         times = times)
  mod
}


## HAS_TESTS
#' Two-Step Method for Fitting a Model
#'
#' @param object A `bage_mod` object.
#' @param quiet Whether to suppress warning messages from nlminb
#' @param vars_inner Variables used
#' in inner model.
#' @param max_jitter Maximum amount to add to
#' precision matrix to facilitate Cholesky factorization
#' @param optimizer Which optimizer to use
#' @param quiet Whether to suppress warnings from 'optimizer'
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_inner_outer <- function(mod,
                            optimizer,
                            quiet,
                            vars_inner,
                            max_jitter,
                            start_oldpar) {
  if (start_oldpar)
    cli::cli_abort("{.arg start_oldpar} must be {.val {FALSE}} when using \"inner-outer\" method.")
  if (has_datamod(mod))
    cli::cli_abort("\"inner-outer\" method cannot be used with models that include a data model.")
  if (is.null(vars_inner))
    vars_inner <- make_vars_inner(mod)
  else
    check_vars_inner(vars_inner)
  use_term <- make_use_term(mod = mod,
                            vars_inner = vars_inner)
  mod_inner <- make_mod_inner(mod = mod,
                              use_term = use_term)
  aggregate <- can_aggregate(mod_inner)
  mod_inner <- fit_default(mod = mod_inner,
                           optimizer = optimizer,
                           quiet = quiet,
                           start_oldpar = start_oldpar,
                           max_jitter = max_jitter,
                           aggregate = aggregate)
  mod_outer <- make_mod_outer(mod = mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  aggregate <- can_aggregate(mod_inner)
  mod_outer <- fit_default(mod = mod_outer,
                           optimizer = optimizer,
                           quiet = quiet,
                           start_oldpar = start_oldpar,
                           max_jitter = max_jitter,
                           aggregate = aggregate)
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
                            max_jitter = max_jitter,
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
#' Create New 'f' to Resume Calculations with New Optimizer
#'
#' @param f_old Function created by 'makeADFun'
#' @param quiet Whether to show progress messages
#' @param data Named list of inputs for makeADFun
#' @param random Named list of terms to be treated as random effects
#' @param map Named list of terms to be treated as fixed
#' @param optimizer_old Name of previous optimizer used on 'f_old'
#' @param optimizer_new Name of optimizer about to be used on 'f_new'
#'
#' @return New version of 'f'
#'
#' @noRd
make_f_new <- function(f_old, quiet, data, random, map, optimizer_old, optimizer_new) {
  if (!quiet)
    cli::cli_alert_info("{.val {optimizer_old}} optimizer did not converge: continuing with {.val {optimizer_new}}.")
  has_random_effects <- !is.null(random)
  if (has_random_effects)
    sdreport <- TMB::sdreport(f_old, bias.correct = FALSE, getJointPrecision = FALSE)
  else
    sdreport <- TMB::sdreport(f_old, bias.correct = FALSE, getReportCovariance = FALSE)
  oldpar <- as.list(sdreport, what = "Est")
  TMB::MakeADFun(data = data,
                 parameters = oldpar,
                 map = map,
                 DLL = "bage",
                 random = random,
                 silent = TRUE)
}


## HAS_TESTS
#' Make 'data' Argument for 'makeADFun'
#'
#' @param mod Object of class 'bage_mod'
#' @param aggregate Logical. Whether to aggrgate input data.
#'
#' @returns A named list
#'
#' @noRd
make_fit_data <- function(mod, aggregate) {
  l <- make_outcome_offset_matrices(mod = mod,
                                    aggregate = aggregate)
  outcome <- l$outcome
  offset <- l$offset
  matrices_effect_outcome <- l$matrices_effect_outcome
  matrix_covariates <- l$matrix_covariates
  dimnames_terms <- mod$dimnames_terms
  terms_effect <- make_terms_effects(dimnames_terms)
  has_covariates <- has_covariates(mod)
  i_lik <- make_i_lik(mod)
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
  i_datamod <- make_fit_i_datamod(mod)
  datamod_consts <- make_fit_datamod_consts(mod)
  datamod_matrices <- make_fit_datamod_matrices(mod)
  list(i_lik = i_lik,
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
       mean_disp = mean_disp,
       matrix_covariates = matrix_covariates,
       i_datamod = i_datamod,
       datamod_consts = datamod_consts,
       datamod_matrices = datamod_matrices)
}


## HAS_TESTS
#' Make Vector Holding Constants for Data Model
#'
#' Return vector of length 0 if no data model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A double vector
#'
#' @noRd
make_fit_datamod_consts <- function(mod) {
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    ans <- make_datamod_consts(datamod)
  }
  else
    ans <- double()
  ans
}


## HAS_TESTS
#' Make List Holding Matrices for Data Model
#'
#' Return list of length 0 if no data model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list
#'
#' @noRd
make_fit_datamod_matrices <- function(mod) {
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    ans <- make_datamod_matrices(datamod)
  }
  else
    ans <- list()
  ans
}


## HAS_TESTS
#' Make Vector Holding Parameters for Data Model
#'
#' Return vector of length 0 if no data model,
#' or data model does not use parameters
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list
#'
#' @noRd
make_fit_datamod_param <- function(mod) {
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    ans <- make_datamod_param(datamod)
  }
  else
    ans <- double()
  ans
}


## HAS_TESTS
#' Make 'i_datamod', Index for Data Models
#'
#' Value is 0 if no data model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer scalar
#'
#' @noRd
make_fit_i_datamod <- function(mod) {
  has_datamod <- has_datamod(mod)
  if (has_datamod) {
    datamod <- mod$datamod
    ans <- make_i_lik_part(datamod)
  }
  else
    ans <- 0L
  ans
}
      

## HAS_TESTS
#' Make mapping used by MakeADFun
#'
#' Make 'map' argument to be passed to MakeADFun.
#' Return value is non-NULL if
#' (i) any priors are "bage_prior_known", or
#' (ii) 'mean_disp' is 0, or
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns NULL or a named list
#'
#' @noRd
make_fit_map <- function(mod) {
  eps_disp <- 1e-6
  priors <- mod$priors
  mean_disp <- mod$mean_disp
  ## determine whether any parameters fixed
  is_known <- vapply(priors, is_known, FALSE)
  is_effectfree_fixed <- any(is_known)
  is_disp_fixed <- abs(mean_disp) < eps_disp
  ## return NULL if nothing fixed
  if (!is_effectfree_fixed && !is_disp_fixed)
    return(NULL)
  ## otherwise construct named list
  ans <- list()
  if (is_effectfree_fixed)
    ans$effectfree <- make_map_effectfree_fixed(mod)
  if (is_disp_fixed)
    ans$log_disp <- factor(NA)
  ans
}


## HAS_TESTS
#' Make 'parameters' Argument for 'makeADFun'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list
#'
#' @noRd
make_fit_parameters <- function(mod) {
  effectfree <- make_effectfree(mod)
  hyper <- make_hyper(mod)
  hyperrandfree <- make_hyperrandfree(mod)
  log_disp <- c(disp = 0)
  coef_covariates <- make_coef_covariates(mod)
  datamod_param <- make_fit_datamod_param(mod)
  list(effectfree = effectfree,
       hyper = hyper,
       hyperrandfree = hyperrandfree,
       log_disp = log_disp,
       coef_covariates = coef_covariates,
       datamod_param = datamod_param)
}


## HAS_TESTS
#' Make 'random' argument to MakeADFun function
#'
#' Need to make sure there are at least
#' some fixed effects
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A character vector
#'
#' @noRd
make_fit_random <- function(mod) {
  priors <- mod$priors
  has_hyper <- any(make_lengths_hyper(mod) > 0L)
  has_hyperrandfree <- any(vapply(priors, has_hyperrandfree, FALSE))
  has_covariates <- has_covariates(mod)
  has_datamod_param <- has_datamod_param(mod)
  if (has_hyper)
    ans <- "effectfree"
  else
    ans <- NULL
  if (has_hyperrandfree)
    ans <- c(ans, "hyperrandfree")
  if (has_covariates)
    ans <- c(ans, "coef_covariates")
  if (has_datamod_param)
    ans <- c(ans, "datamod_param")
  ans
}


## HAS_TESTS
#' Calculate Times Required for Fitting Model
#'
#' @param t_start Time 'fit' called
#' @param t_optim Time optimizer started
#' @param t_report Time reporting started
#' @param t_end Time calculations completed
#'
#' @returns A named list
#'
#' @noRd
make_fit_times <- function(t_start, t_optim, t_report, t_end) {
  time_total <- as.numeric(difftime(t_end, t_start, units = "secs"))
  time_max <- as.numeric(difftime(t_report, t_optim, units = "secs"))
  time_draw <- as.numeric(difftime(t_end, t_report, units = "secs"))
  list(time_total = time_total,
       time_max = time_max,
       time_draw = time_draw)
}


## HAS_TESTS
#' Optimise function 'f', Using Specified Optimizer
#'
#' Note that 'f' is modified in place by TMB.
#'
#' @param f Function created by 'makeADFun'
#' @param quiet Whether to print progress information
#' @param optimizer Name of optimizer to use
#' @param data Named list of fixed inputs
#' @param random Named list of terms to be treated as random effects
#' @param map Named list of terms to be held fixed
#' @param is_test_nonconv Whether the function is being called as part of a
#' unit test
#'
#' @returns A named list with 'f' and information
#' on the optimization.
#'
#' @noRd
optimize_adfun <- function(f,
                           quiet,
                           optimizer,
                           data,
                           random,
                           map,
                           is_test_nonconv) {
  if (optimizer == "multi") {
    out <- optimize_nlminb(f = f, quiet = quiet)
    if (!out$converged || is_test_nonconv) {
      iter_old <- out$iter
      message_old <- out$message %||% "<no message>"
      f_new <- make_f_new(f_old = f,
                          data = data,
                          quiet = quiet,
                          random = random,
                          map = map,
                          optimizer_old = "nlminb",
                          optimizer_new = "BFGS")
      out <- optimize_bfgs(f = f_new, quiet = quiet)
      out$iter <- paste(iter_old, out$iter, sep = " + ")
      message_new <- out$message %||% "<no message>"
      out$message <- paste(message_old, message_new, sep = " + ")
      out$optimizer <- "nlminb + BFGS"
    }
  }
  else if (optimizer == "nlminb")
    out <- optimize_nlminb(f = f, quiet = quiet)
  else if (optimizer == "BFGS")
    out <- optimize_bfgs(f = f, quiet = quiet)
  else if (optimizer == "CG")
    out <- optimize_cg(f = f, quiet = quiet)
  else
    cli::cli_abort("Internal error: {.val {optimizer}} is not a valid value for {.arg optimizer}.")
  if (!out$converged) # nocov
    cli::cli_alert_warning("Optimizer did not converge.") # nocov
  out
}


## HAS_TESTS
#' Optimise function 'f', Using "BFGS" Option from 'optim'
#'
#' Note that 'f' is modified in place by TMB.
#'
#' @param f Function created by 'makeADFun'
#' @param quiet Whether to print progress information
#'
#' @returns A named list with 'f' and information
#' on the optimization.
#'
#' @noRd
optimize_bfgs <- function(f, quiet) {
  if (quiet) {
    suppressWarnings(
      out <- stats::optim(par = f$par,
                          fn = f$fn,
                          gr = f$gr,
                          method = "BFGS",
                          control = list(maxit = 300L,
                                         trace = 0L))
    )
  }
  else {
    cli::cli_alert("Output from {.fun optim}:")
    out <- stats::optim(par = f$par,
                        fn = f$fn,
                        gr = f$gr,
                        method = "BFGS",
                        control = list(trace = 1L,
                                       maxit = 300L,
                                       REPORT = 1L))
  }
  iter <- out$counts[["gradient"]]
  message <- out$message
  converged <- identical(out$convergence, 0L)
  optimizer <- "BFGS"
  list(f = f,
       iter = iter,
       message = message,
       converged = converged,
       optimizer = optimizer)
}


## HAS_TESTS
#' Optimise function 'f', Using "CG" Option from 'optim'
#'
#' Note that 'f' is modified in place by TMB.
#'
#' @param f Function created by 'makeADFun'
#' @param quiet Whether to print progress information
#'
#' @returns A named list with 'f' and information
#' on the optimization.
#'
#' @noRd
optimize_cg <- function(f, quiet) {
  if (quiet) {
    suppressWarnings(
      out <- stats::optim(par = f$par,
                          fn = f$fn,
                          gr = f$gr,
                          method = "CG",
                          control = list(maxit = 300L,
                                         trace = 0L))
    )
  }
  else {
    cli::cli_alert("Output from {.fun optim}:")
    out <- stats::optim(par = f$par,
                        fn = f$fn,
                        gr = f$gr,
                        method = "CG",
                        control = list(trace = 1L,
                                       maxit = 300L,
                                       REPORT = 1L))
  }
  iter <- out$counts[["gradient"]]
  message <- out$message
  converged <- identical(out$convergence, 0L)
  optimizer <- "CG"
  list(f = f,
       iter = iter,
       message = message,
       converged = converged,
       optimizer = optimizer)
}


## HAS_TESTS
#' Optimise function 'f', Using Function 'nlminb'
#'
#' Note that 'f' is modified in place by TMB.
#'
#' @param f Function created by 'makeADFun'
#' @param quiet Whether to print progress information
#'
#' @returns A named list with 'f' and information
#' on the optimization.
#'
#' @noRd
optimize_nlminb <- function(f, quiet) {
  if (quiet) {
    suppressWarnings(
      out <- stats::nlminb(start = f$par,
                           objective = f$fn,
                           gradient = f$gr,
                           control = list(iter.max = 300L,
                                          eval.max = 400L,
                                          trace = 0L))
    )
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
  converged <- identical(out$convergence, 0L)
  optimizer <- "nlminb"
  list(f = f,
       iter = iter,
       message = message,
       converged = converged,
       optimizer = optimizer)
}



#' Record Information about the Fitting Process
#'
#' @param mod Object of class 'bage_mod'
#' @param est Point estimates
#' @param optimizer Name of optimizer used for fitting
#' @param iter Number of iterations used by optimizer
#' @param converged Whether opimization converged
#' @param message Messages from optimizer
#' @param times Times used for calculations
#'
#' @returns Modified version of 'bage_mod'
#'
#' @noRd
record_metadata <- function(mod, est, optimizer, iter, converged, message, times) {
  mod$computations <- tibble::tibble(time_total = times$time_total,
                                     time_max = times$time_max,
                                     time_draw = times$time_draw,
                                     iter = iter,
                                     converged = converged,
                                     message = message)
  mod$optimizer <- optimizer
  mod$oldpar <- est
  mod
}


