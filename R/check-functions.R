
## HAS_TESTS
#' Check That If a Term Includes Time, Then the Along
#' Dimension for that Term is Time
#'
#' Applied to whole model
#'
#' Assumes that 'var_time' non-NULL
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_along_is_time <- function(mod) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  for (i_prior in seq_along(priors)) {
    prior <- priors[[i_prior]]
    uses_along <- uses_along(prior)
    if (!uses_along)
      next
    along <- prior$specific$along
    has_along <- !is.null(along)
    if (!has_along)
      next
    nm_prior <- nms_priors[[i_prior]]
    nm_prior_split <- strsplit(nm_prior, split = ":")[[1L]]
    has_time <- !is.null(var_time) && (var_time %in% nm_prior_split)
    if (!has_time)
      next
    along_is_time <- identical(along, var_time)
    if (!along_is_time) {
      corrected_prior <- prior
      corrected_prior$specific$along <- NULL
      str_cor <- sprintf("%s ~ %s",
                         nm_prior,
                         str_call_prior(corrected_prior))
      cli::cli_abort(c("Unable to forecast term {.val {nm_prior}}.",
                       i = paste("{.val {nm_prior}} includes {.val {var_time}},",
                                 "but uses {.val {along}} as the \"along\" dimension."),
                       i = "Change specification of prior to {.code {str_cor}}?"))
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that object inherits from class "bage_mod"
#'
#' @param x Object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_bage_mod <- function(x, nm_x) {
    if (!inherits(x, "bage_mod"))
        cli::cli_abort(c("{.arg {nm_x}} does not have class {.cls bage_mod}.",
                         i = "{.arg {nm_x}} has class {.cls {class(x)}}.",
                         i = paste("{.arg {nm_x}} should be created by a function",
                                   "such as {.fun bage::mod_pois}.")))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'con' is Only "by" When 'n_by' is Greater Than 1
#'
#' @param con Constraints
#' @param n_by Number of 'by' dimensions.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_con_n_by <- function(con, n_by, nm) {
  if ((con == "by") && (n_by == 1L))
    cli::cli_abort(c("{.arg con} is {.val {con}} but {.var {nm}} term is a main effect.",
                     i = paste("{.arg con} should only equal {.val {con}}",
                               "in a prior for an interaction.")))
  invisible(TRUE)
}


## HAS_TESTS
#' Check Formula Used When Creating Covariates
#'
#' @param formula One-sided formula describing covariates
#' @param mod Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_covariates_formula <- function(formula, mod) {
  formula_mod <- mod$formula
  data <- mod$data
  nm_offset_data <- get_nm_offset_data(mod)
  nm_offset_mod <- get_nm_offset_mod(mod)
  terms_formula_mod <- stats::terms(formula_mod)
  nms_data <- names(data)
  ## 'formula' is formula
  if (!inherits(formula, "formula"))
    cli::cli_abort(c("{.arg formula} is not a formula.",
                     i = "{.arg formula} has class {.class {class(formula)}}."))
  ## 'formula' does not include response
  terms_formula <- stats::terms(formula)
  has_response <- attr(terms_formula, "response")
  if (has_response)
    cli::cli_abort(c("{.arg formula} includes a response variable.",
                     i = "{.arg formula}: {.code {deparse1(formula)}}."))
  ## 'formula' does not use any variables already used by main model
  nms_vars_formula <- rownames(attr(terms_formula, "factors"))
  nm_response <- rownames(attr(terms_formula_mod, "factors"))[[1L]]
  nms_vars_mod <- rownames(attr(terms_formula_mod, "factors"))[-1L]
  if (nm_response %in% nms_vars_formula)
    cli::cli_abort(c("{.arg formula} includes response from {.arg mod}.",
                     i = "{.arg formula}: {deparse(formula)}.",
                     i = "response: {.val {nm_response}}."))
  if (any(nms_vars_mod %in% nms_vars_formula)) {
    in_mod <- intersect(nms_vars_formula, nms_vars_mod)
    n <- length(in_mod)
    cli::cli_abort(c("{.arg formula} includes {cli::qty(n)} variable{?s} from {.arg mod}.",
                     i = "{.arg formula}: {deparse(formula)}.",
                     i = "variable{?s} from {.arg mod}: {.val {in_mod}}."))
  }
  is_offset_specified <- !is.null(nm_offset_data)
  if (is_offset_specified && (nm_offset_data %in% nms_vars_formula))
    cli::cli_abort(c("{.arg formula} includes {nm_offset_mod} from {.arg mod}.",
                     i = "{.arg formula}: {deparse(formula)}.",
                     i = "{nm_offset_mod}: {.val {nm_offset_data}}."))
  ## all variables used in 'formula' are present in 'data'
  is_in_data <- nms_vars_formula %in% nms_data
  i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
  if (i_not_in_data > 0L) {
    nm_not_found <- nms_vars_formula[[i_not_in_data]]
    cli::cli_abort(c("variable {.var {nm_not_found}} from {.arg formula} not found in data.",
                     i = "{.arg formula}: {deparse(formula)}.",
                     i = "variable{?s} in data: {.val {nms_data}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'est' Object Returned by TMB has No NAs
#'
#' @param est Named list
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_est <- function(est) {
  is_na <- is.na(unlist(est, use.names = FALSE))
  if (any(is_na)) {
    index_term <- which(is_na)
    nm_term <- get_term_from_est(est = est, index_term = index_term)
    cli::cli_abort(c("Problem deriving posterior distribution.",
                     i = "Estimation of posterior mean for {.val {nm_term}} term{?s} failed.",
                     i = "You may need to change the specification of your model."))
  }
  invisible(TRUE)
}
    
    
## HAS_TESTS
#' Check a Logical Flag
#'
#' @param x TRUE or FALSE
#' @param nm_x Name for 'x' to use in error messages.
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_flag <- function(x, nm_x) {
    if (!identical(length(x), 1L))
        cli::cli_abort(c("{.arg {nm_x}} does not have length {.val {1}}.",
                         i = "{.arg {nm_x}} has length {.val {length(x)}}."))
    if (!is.logical(x))
        cli::cli_abort(c("{.arg {nm_x}} does not have class {.cls logical}.",
                         i = "{.arg {nm_x}} has class {.cls {class(x)}}"))
    if (is.na(x))
        cli::cli_abort("{.arg {nm_x}} is {.val {NA}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that formula for prior meets basic
#' formatting requirements
#'
#' @param A formula
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_format_prior_formula <- function(formula) {
    if (!inherits(formula, "formula")) {
        msg <- "{.arg formula} not a formula."
        if (inherits(formula, "bage_prior"))
            info  <- "{.arg formula} should have format {.code <term> ~ <prior>}."
        else
            info <- "{.arg formula} has class {.cls {class(formula)}}."
        msg <- c(msg, i = info)
        cli::cli_abort(msg)
    }
    n <- length(formula)
    if (n < 3L)
        cli::cli_abort(c("{.arg formula} has too few elements.",
                         i = "{.arg formula} should have format {.code <term> ~ <prior>}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'formula' has Intercept
#'
#' @param formula A formula.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_has_intercept <- function(formula) {
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (!has_intercept)
        cli::cli_abort(c("{.arg formula} does not include an intercept.",
                         i = "{.arg formula}: {.code {deparse1(formula)}}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'formula' has response
#'
#' @param formula A formula.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_has_response <- function(formula) {
    has_response <- attr(stats::terms(formula), "response")
    if (!has_response)
        cli::cli_abort(c("{.arg formula} does not include a response variable.",
                         i = "{.arg formula}: {.code {deparse1(formula)}}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that formula has a variable
#'
#' Check that 'formula' contains the variable 'name'
#' (as a main effect or interaction, or both)
#'
#' @param name A string
#' @param formula A formula
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_formula_has_variable <- function(name, formula) {
  vars <- all.vars(formula[-2L])
  if (!(name %in% vars))
    cli::cli_abort(c("{.arg formula} does not have variable {.val {name}}.",
                     i = "{.arg formula}: {deparse1(formula)}"))
  invisible(TRUE)
}


## HAS_TESTS
#' Check variables used in 'formula' occur in 'data'
#'
#' @param formula A formula.
#' @param data A data frame.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_formula_vnames_in_data <- function(formula, data) {
  nms_formula <- all.vars(formula)
  nms_data <- names(data)
  is_in_data <- nms_formula %in% nms_data
  i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
  if (i_not_in_data > 0L) {
    nm_var <- nms_formula[[i_not_in_data]]
    cli::cli_abort(c("Variable {.var {nm_var}} from {.arg formula} not found in {.arg data}.",
                     i = "{.arg formula}: {.code {deparse(formula)}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Given that 'condition_on' argument is "expected", check that
#' the model includes a dispersion term
#'
#' @param x A fitted 'bage_mod' object
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_has_disp_if_condition_on_expected <- function(x) {
    if (!has_disp(x)) {
        val_expected <- "expected"
        val_fitted <- "fitted"
        cli::cli_abort(c(paste("{.arg condition_on} is {.val {val_expected}} but model",
                               "has no dispersion term."),
                         i = "Use {.code condition_on = {.val {val_fitted}}} instead?"))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check That No Arguments Absorbed By Dots in Function
#'
#' @param dots Arguments absorbed
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_has_no_dots <- function(...) {
  dots <- list(...)
  n_dot <- length(dots)
  if (n_dot > 0L) {
    nms <- names(dots)
    if (is.null(nms)) {
      if (n_dot == 1L)
        cli::cli_abort("Invalid unnamed argument.")
      else
        cli::cli_abort("{n_dot} invalid unnamed arguments.")
    }
    else {
      i_nonblank <- match(TRUE, nzchar(nms))
      cli::cli_abort("{.arg {nms[i_nonblank]}} is not a valid argument.")
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check if Vector Has Infinite Values
#'
#' @param x Vector
#' @param nm_x Name for 'x' to use in error messages.
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_inf <- function(x, nm_x) {
  n_inf <- sum(is.infinite(x))
  if (n_inf > 0L)
    cli::cli_abort("{.arg {nm_x}} has infinite {cli::qty(n_inf)} value{?s}.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check that an Object is a Data Frame
#'
#' @param x An object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_dataframe <- function(x, nm_x) {
  if (!is.data.frame(x))
    cli::cli_abort(c("{.arg {nm_x}} is not a data frame.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a model has been fitted
#'
#' @param x Object of class 'bage_mod'
#' @param nm_x Name for 'x' to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_fitted <- function(x, nm_x) {
    if (!is_fitted(x))
        cli::cli_abort(c("{.arg {nm_x}} has not been fitted.",
                         i = "Call function {.fun bage::fit} on {.arg {nm_x}}?"))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that an Object is a Formula
#'
#' @param formula An R formula
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_formula <- function(formula) {
  if (!inherits(formula, "formula"))
    cli::cli_abort(c("{.arg formula} is not an R formula.",
                     i = "{.arg formula} has class {.cls {class(formula)}}."))
  invisible(TRUE)
} 


## HAS_TESTS
#' Check that an Object is a Matrix
#'
#' @param x An object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_matrix <- function(x, nm_x) {
  if (!is.matrix(x))
    cli::cli_abort(c("{.arg {nm_x}} is not a matrix.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that an Object has Class 'bage_ssvd'
#'
#' @param x An object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_ssvd <- function(x, nm_x) {
  if (!inherits(x, "bage_ssvd"))
    cli::cli_abort(c("{.arg {nm_x}} does not hold scaled SVD values.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}.",
                     i = "{.arg {nm_x}} should have class {.cls bage_ssvd}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that term has has least 'min' elements
#'
#' @param length_effect Number of elements
#' @param min Minimum number of elements
#' @param nm Name of term
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_length_effect_ge <- function(length_effect, min, nm, prior) {
    if (length_effect < min)
        cli::cli_abort(c(paste("{.var {str_call_prior(prior)}} prior cannot be",
                               "used for {.var {nm}} term."),
                         i = paste("{.var {str_call_prior(prior)}} prior can only be",
                                   "used with terms that have at least {min} element{?s}."),
                         i = "{.var {nm}} term has {length_effect} element{?s}."))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'min' and 'max' Arguments for AR Valid
#'
#' @param min Minimum value for damping coefficient(s)
#' @param max Maximum value for damping coefficient(s)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_min_max_ar <- function(min, max) {
  for (nm in c("min", "max")) {
    val <- get(nm)
    if (!is.numeric(val))
      cli::cli_abort(c("{.arg {nm}} is non-numeric.",
                       i = "{.arg {nm}} has class {.cls {class(val)}}."))
    if (length(val) != 1L)
      cli::cli_abort(c("{.arg {nm}} does not have length 1.",
                       i = "{.arg {nm}} has length {.val {length(val)}}."))
    if (is.na(val))
      cli::cli_abort("{.arg {nm}} is {.val {NA}}.")
    if (val < -1)
      cli::cli_abort(c("{.arg {nm}} is less than -1.",
                       i = "{.arg {nm}} should be between -1 and 1.",
                       i = "{.arg {nm}}: {.val {val}}"))
    if (val > 1)
      cli::cli_abort(c("{.arg {nm}} is greater than 1.",
                       i = "{.arg {nm}} should be between -1 and 1.",
                       i = "{.arg {nm}}: {.val {val}}"))
  }    
  if (max <= min)
    cli::cli_abort(c("{.arg max} is less than or equal to {.arg min}.",
                     i = "{.arg min}: {.val {min}}",
                     i = "{.arg max}: {.val {max}}"))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'mod_est' and 'mod_sim' meet minimum
#' requirements for 'report_sim'
#'
#' @param mod_est,mod_sim Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mod_est_sim_compatible <- function(mod_est, mod_sim) {
    ## same class
    if (!is_same_class(x = mod_est, y = mod_sim))
        cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different classes.",
                         i = "{.arg mod_est} has class {.cls {class(mod_est)}}.",
                         i = "{.arg mod_sim} has class {.cls {class(mod_sim)}}."))
    ## outcome variables are same
    nm_outcome_data_sim <- get_nm_outcome_data(mod_est)
    nm_outcome_data_est <- get_nm_outcome_data(mod_sim)
    if (!identical(nm_outcome_data_sim, nm_outcome_data_est))
        cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different outcome variables.",
                         i = "Outcome variable for {.arg mod_est}: {.val {nm_outcome_data_sim}}.",
                         i = "Outcome variable for {.arg mod_sim}: {.val {nm_outcome_data_est}}."))
    ## apart from outcome variable, data are the same
    data_sim <- mod_est$data
    data_est <- mod_sim$data
    nms_sim <- names(data_sim)
    nms_est <- names(data_est)
    if (!identical(sort(nms_sim), sort(nms_est)))
        cli::cli_abort(c(paste("Data for {.arg mod_est} and {.arg mod_sim} have",
                               "different variables."),
                         i = "Data for {.arg mod_est} has variables {.val {nms_sim}}.",
                         i = "Data for {.arg mod_sim} has variables {.val {nms_est}}."))
    for (nm in setdiff(nms_sim, nm_outcome_data_sim)) {
        var_sim <- data_sim[[nm]]
        var_est <- data_est[[nm]]
        is_same <- var_sim == var_est
        i_not_same <- match(FALSE, is_same, nomatch = 0L)
        if (i_not_same > 0L) {
            val_sim <- var_sim[[i_not_same]]
            val_est <- var_est[[i_not_same]]
            cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different data.",
                             i = paste("In row {i_not_same} of data for {.arg mod_est},",
                                       "variable {.var {nm}} has value {.val {val_sim}}."),
                             i = paste("In row {i_not_same} of data for {.arg mod_sim},",
                                       "variable {.var {nm}} has value {.val {val_est}}.")))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that Model Data Includes at Least One Valid Observation
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns TRUE, invisbly
#'
#' @noRd
check_mod_has_obs <- function(mod) {
  is_in_lik <- get_is_in_lik(mod)
  if (!any(is_in_lik)) {
    msg <- "No data for fitting model."
    if (length(is_in_lik) == 0L)
      cli::cli_abort(msg)
    is_in_lik_effects <- get_is_in_lik_effects(mod)
    is_in_lik_offset <- get_is_in_lik_offset(mod)
    is_in_lik_outcome <- get_is_in_lik_outcome(mod)
    n_na_effects <- sum(!is_in_lik_effects)
    if (n_na_effects > 0L)
      msg <- c(msg, i = "Number of rows where predictor is {.val {NA}}: {.val {n_na_effects}}.")
    nm_offset_data <- get_nm_offset_data(mod)
    has_offset <- !is.null(nm_offset_data)
    if (has_offset) {
      nm_offset_mod <- get_nm_offset_mod(mod)
      n_na_offset <- sum(!is_in_lik_offset)
      if (n_na_offset > 0L)
        msg <- c(msg, i = "Number of rows where {nm_offset_mod} is {.val {NA}}: {.val {n_na_offset}}.")
    }
    n_na_outcome <- sum(!is_in_lik_outcome)
    if (n_na_outcome > 0L)
      msg <- c(msg, i = "Number of rows where outcome is {.val {NA}}: {.val {n_na_outcome}}.")
    cli::cli_abort(msg)
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'mult_high_rate' Argument for Detecting Suspicious Rates Valid
#'
#' OK for 'mult_high_rate' to be NULL or Inf
#' (in which case test for suspicious rates
#' not done.)
#' 
#' @param mult_high_rate Non-negative
#'
#' @returns TRUE invisibly
#'
#' @noRd
check_mult_high_rate <- function(mult_high_rate) {
  if (is.null(mult_high_rate))
    return(invisible(TRUE))
  if (identical(mult_high_rate, Inf))
    return(invisible(TRUE))
  if (!is.numeric(mult_high_rate))
    cli::cli_abort(c("{.arg mult_high_rate} not NULL or numeric.",
                     i = paste("{.arg mult_high_rate} has class",
                               "{.cls {class(mult_high_rate)}}.")))
  if (length(mult_high_rate) != 1L)
    cli::cli_abort(c("{.arg mult_high_rate} has length {.val {length(mult_high_rate)}}.",
                     i = "Should have length {.val {1}}."))
  if (is.na(mult_high_rate))
    cli::cli_abort("{.arg mult_high_rate} is {.val {NA}}.")
  if (mult_high_rate <= 0)
    cli::cli_abort(c("{.arg mult_high_rate} non-positive.",
                     "{.arg mult_high_rate} is {.val {mult_high_rate}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Along Dimension of Interaction has at Least 'min' Elements
#'
#' @param n_along Number of elements
#' @param min Minimum number of elements
#' @param nm Name of term
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n_along_ge <- function(n_along, min, nm, prior) {
  if (n_along < min)
    cli::cli_abort(c(paste("{.var {str_call_prior(prior)}} prior cannot be",
                           "used for {.var {nm}} term."),
                     i = paste("{.var {str_call_prior(prior)}} prior can only be",
                               "used with interactions where the 'along' dimension has at least {min} element{?s}."),
                     i = "The 'along' dimension of {.var {nm}} has {n_along} element{?s}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check if Vector Has NaNs
#'
#' @param x Vector
#' @param nm_x Name for 'x' to use in error messages.
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_nan <- function(x, nm_x) {
  if (any(is.nan(x)))
    cli::cli_abort("{.arg {nm_x}} has {.val {NaN}}.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'new_seeds' is List of Numeric Scalars with Correct Names
#'
#' @param new_seeds A named list of numeric scalars.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_new_seeds <- function(new_seeds) {
  if (!is.null(new_seeds)) {
    nms_expected <- c("seed_components",
                      "seed_augment",
                      "seed_forecast_components",
                      "seed_forecast_augment")
    if (!is.list(new_seeds))
      cli::cli_abort(c("{.arg new_seeds} is not a list.",
                       i = "{.arg new_seeds} has class {.cls {class(new_seeds)}}."))
    if (!identical(length(new_seeds), length(nms_expected)))
      cli::cli_abort(c("{.arg new_seeds} does not have {length(nms_expected)} elements.",
                       i = "{.arg new_seeds} has {length(new_seeds)} element{?s}."))
    nms_seeds <- names(new_seeds)
    if (is.null(nms_seeds))
      cli::cli_abort("{.arg new_seeds} does not have names.")
    if (!setequal(nms_seeds, nms_expected))
      cli::cli_abort(c("{.arg new_seeds} does not have expected names.",
                       i = "Names supplied: {.val {nms_seeds}}.",
                       i = "Names expected: {.val {nms_expected}}."))
    is_numeric <- vapply(new_seeds, is.numeric, TRUE)
    i_not_numeric <- match(FALSE, is_numeric, nomatch = 0L)
    if (i_not_numeric > 0L)
      cli::cli_abort(c("{.arg new_seeds} has non-numeric element.",
                       i = paste("Element {.val {nms_seeds[[i_not_numeric]]}} has class",
                                 "{.cls {class(new_seeds[[i_not_numeric]])}}.")))
    is_length_1 <- lengths(new_seeds) == 1L
    i_not_length_1 <- match(FALSE, is_length_1, nomatch = 0L)
    if (i_not_length_1 > 0L)
      cli::cli_abort(c("{.arg new_seeds} has element not of length 1.",
                       i = paste("Element {.val {nms_seeds[[i_not_length_1]]}} has length",
                                 "{length(new_seeds[[i_not_length_1]])}.")))
  }
  invisible(TRUE)
}
 


## HAS_TESTS
#' Check that Object is Numeric, Length 1, Non-NA, Finite
#'
#' @param x An object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_number <- function(x, nm_x) {
  if (!is.numeric(x))
    cli::cli_abort(c("{.arg {nm_x}} is non-numeric.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  if (length(x) != 1L)
    cli::cli_abort(c("{.arg {nm_x}} has length {.val {length(x)}}.",
                     i = "Should have length {.val {1}}."))
  if (is.na(x))
    cli::cli_abort("{.arg {nm_x}} is {.val {NA}}.")
  if (any(is.infinite(x)))
    cli::cli_abort("{.arg {nm_x}} is non-finite.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Vector is Numeric, Non-NA, Finite, Non-Zero Length
#'
#' @param x A vector
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_numeric <- function(x, nm_x) {
  if (!is.numeric(x))
    cli::cli_abort(c("{.arg {nm_x}} is non-numeric.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  if (length(x) == 0L)
    cli::cli_abort("{.arg {nm_x}} has length {.val {0}}.")
  if (anyNA(x))
    cli::cli_abort("{.arg {nm_x}} has {.val {NA}}.")
  if (any(is.infinite(x)))
    cli::cli_abort("{.arg {nm_x}} has non-finite value.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check offset occurs in 'data'
#'
#' @param nm_offset_data The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset_mod The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_in_data <- function(nm_offset_data, nm_offset_mod, data) {
  is_formula <- startsWith(nm_offset_data, "~")
  if (is_formula) {
    ans <- tryCatch(eval_offset_formula(nm_offset_data = nm_offset_data, data = data),
                    error = function(e) e)
    if (inherits(ans, "error"))
      cli::cli_abort(c("Problem with formula used for {.arg {nm_offset_mod}}.",
                       i = "Formula: {.val {nm_offset_data}}.",
                       i = ans$message))
  }
  else {
    nms_data <- names(data)
    if (!(nm_offset_data %in% nms_data)) {
      cli::cli_abort(c("{.arg {nm_offset_mod}} not found in {.arg data}.",
                       i = "{.arg {nm_offset_mod}}: {.val {nm_offset_data}}."))
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Offset Has No Negative Values
#'
#' @param nm_offset_data The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset_mod The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_nonneg <- function(nm_offset_data, nm_offset_mod, data) {
  is_formula <- startsWith(nm_offset_data, "~")
  if (is_formula) {
    offset <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  }
  else {
    offset <- data[[nm_offset_data]]
  }
  n_neg <- sum(offset < 0, na.rm = TRUE)
  if (n_neg > 0L)
    cli::cli_abort(c("{.arg {nm_offset_mod}} has negative {cli::qty(n_neg)} value{?s}.",
                     i = "{nm_offset_mod}: {.val {nm_offset_data}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Raise Error if Model Object Created Using Old Version of 'bage'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd 
check_old_version <- function(x, nm_x) {
  check_bage_mod(x = x, nm_x = nm_x)
  is_norm <- inherits(x, "bage_mod_norm")
  has_covariates <- has_covariates(x)
  nms <- names(x)
  is_old_version <- (!("draws_hyperrandfree" %in% nms)
    || ("seed_stored_draws" %in% nms)
    || (is_norm && !("offset_mean" %in% nms)))
  if (is_old_version) {
    cli::cli_abort(c("{.arg {nm_x}} appears to have been created with an old version of {.pkg bage}.",
                     i = "Please recreate the object using the current version."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'original_scale' Argument to 'augment'
#'
#' Includes warning when argument ignored.
#'
#' @param original_scale Logical scalar
#' @param mod Object of class 'bage_mod'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_original_scale <- function(original_scale, mod) {
  check_flag(x = original_scale, nm_x = "original_scale")
  if (isTRUE(original_scale) && !inherits(mod, "bage_mod_norm"))
    cli::cli_warn(paste("{.fun components} ignores {.arg original_scale} if {.arg object} was",
                        "not created with {.fun mod_norm}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Prior has Age Dimension
#'
#' @param prior Object of class 'bage_prior'
#' @param nm Name of term
#' @param vname_age Name of age dimension, or NULL
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prior_age <- function(prior, nm, var_age) {
  str_nm_prior <- str_nm_prior(prior)
  msg1 <- "Problem with {.var {str_nm_prior}} prior for term {.var {nm}}."
  if (is.null(var_age))
    cli::cli_abort(c(msg1,
                     i = paste("Can't use {.var {str_nm_prior}} prior when",
                               "age variable not yet identified."),
                     i = "Use function {.fun set_var_age} to identify age variable?"))
  nm_split <- strsplit(nm, split = ":")[[1L]]
  if (!(var_age %in% nm_split))
    cli::cli_abort(c(msg1,
                     i = "{.var {str_nm_prior}} prior only used with terms involving age."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Prior has Time Dimension
#'
#' @param prior Object of class 'bage_prior'
#' @param nm Name of term
#' @param vname_time Name of time dimension, or NULL
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prior_time <- function(prior, nm, var_time) {
  str_nm_prior <- str_nm_prior(prior)
  msg1 <- "Problem with {.var {str_nm_prior}} prior for term {.var {nm}}."
  if (is.null(var_time))
    cli::cli_abort(c(msg1,
                     i = paste("Can't use {.var {str_nm_prior}} prior when",
                               "time variable not yet identified."),
                     i = "Use function {.fun set_var_time} to identify time variable?"))
  nm_split <- strsplit(nm, split = ":")[[1L]]
  if (!(var_time %in% nm_split))
    cli::cli_abort(c(msg1,
                     i = "{.var {str_nm_prior}} prior only used with terms involving time."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check Offset not Used in 'formula' Agument
#'
#' Applied only when offset is the name of a variable.
#'
#' @param nm_offset_data The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset_mod The name used to refer to the
#' offset in user-visible functions
#' @param formula Formula specifying model
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_not_in_formula <- function(nm_offset_data, nm_offset_mod, formula) {
  is_offset_formula <- startsWith(nm_offset_data, "~")
  nms_formula <- all.vars(formula)
  if (!is_offset_formula) {
    if (nm_offset_data %in% nms_formula) {
      cli::cli_abort(c("{.arg {nm_offset_mod}} included in {.arg formula}.",
                       i = "{.arg {nm_offset_mod}}: {.val {nm_offset_data}}.",
                       i = "{.arg formula}: {.val {deparse1(formula)}}."))
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable less than
#' or equal to offset variable
#'
#' @param formula A formula
#' @param nm_offset_data The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset_mod The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_le_offset <- function(formula,
                                 nm_offset_data,
                                 nm_offset_mod,
                                 data) {
  nm_response <- deparse1(formula[[2L]])
  response <- data[[nm_response]]
  is_offset_formula <- startsWith(nm_offset_data, "~")
  if (is_offset_formula)
    offset <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  else
    offset <- data[[nm_offset_data]]
  is_gt_offset <- !is.na(response) & !is.na(offset) & (response > offset)
  i_gt_offset <- match(TRUE, is_gt_offset, nomatch = 0L)
  if (i_gt_offset > 0L) {
    cli::cli_abort(c("Response greater than {.var {nm_offset_mod}}.",
                     i = "Response: {.var {nm_response}}.",
                     i = "{.var {nm_offset_mod}}: {.val {nm_offset_data}}.",
                     i = "Value for response: {.val {response[[i_gt_offset]]}}",
                     i = "Value for {.var {nm_offset_mod}}: {.val {offset[[i_gt_offset]]}}"))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable always zero when
#' offset variable is zero
#'
#' @param formula A formula
#' @param nm_offset_data The name of the variable being
#' used as an offset
#' @param nm_offset_mod The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_zero_if_offset_zero <- function(formula,
                                           nm_offset_data,
                                           nm_offset_mod,
                                           data) {
  nm_response <- deparse1(formula[[2L]])
  response <- data[[nm_response]]
  is_offset_formula <- startsWith(nm_offset_data, "~")
  if (is_offset_formula)
    offset <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  else
    offset <- data[[nm_offset_data]]
  response_pos <- response > 0
  offset_pos <- offset > 0
  is_pos_nonpos <- !is.na(response) & !is.na(offset) & response_pos & !offset_pos
  i_pos_nonpos <- match(TRUE, is_pos_nonpos, nomatch = 0L)
  if (i_pos_nonpos > 0L)
    cli::cli_abort(c("Response is non-zero but {.var {nm_offset_mod}} is zero.",
                     i = "Response: {.var {nm_response}}.",
                     i = "{.var {nm_offset_mod}}: {.var {nm_offset_data}}.",
                     i = "Value for {.var {nm_response}}: {.val {response[[i_pos_nonpos]]}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable has no
#' negative values
#'
#' @param formula A formula
#' @param data A data frame
#' @param nm_distn Name of the distribution (eg "pois")
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_response_nonneg <- function(formula, data, nm_distn) {
  nm_response <- deparse1(formula[[2L]])
  response <- data[[nm_response]]
  n_neg <- sum(response < 0L, na.rm = TRUE)
  if (n_neg > 0L) 
    cli::cli_abort(c(paste("Model uses {nm_distn} distribution but response variable",
                           "has negative {cli::qty(n_neg)}  value{?s}."),
                     i = "Response variable: {.var {nm_response}}."))
  invisible(TRUE)
}
        
    
## HAS_TESTS
#' Check a scale term
#'
#' Check that `x` is a positive or non-negative
#' finite scalar.
#'
#' @param x A positive or non-negative number.
#' @param nm_x Name for `x` to be
#' used in error messages.
#' @param zero_ok Whether 'x' can be zero.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_scale <- function(x, nm_x, zero_ok) {
    if (!is.numeric(x))
        cli::cli_abort(c("{.arg {nm_x}} is non-numeric.",
                         i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
    if (length(x) != 1L)
        cli::cli_abort(c("{.arg {nm_x}} does not have length {.val {1}}.",
                         i = "{.arg {nm_x}} has length {.val {length(x)}}."))
    if (is.na(x))
        cli::cli_abort("{.arg {nm_x}} is {.val {NA}}.")
    if (is.infinite(x))
        cli::cli_abort("{.arg {nm_x}} is infinite.")
    if (zero_ok) {
        if (x < 0)
            cli::cli_abort(c("{.arg {nm_x}} is negative.",
                             i = "{.arg {nm_x}} equals {.val {x}}."))
    }
    else {
        if (x <= 0)
            cli::cli_abort(c("{.arg {nm_x}} is non-positive.",
                             i = "{.arg {nm_x}} equals {.val {x}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check a String
#'
#' Check that `x` is a character vector
#' of length 1, not blank and not NA.
#'
#' @param x A string
#' @param nm_x Name for `x` to be
#' used in error messages.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_string <- function(x, nm_x) {
    if (!is.character(x))
        cli::cli_abort(c("{.arg {nm_x}} is non-character.",
                         i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
    if (length(x) != 1L)
        cli::cli_abort(c("{.arg {nm_x}} does not have length 1.",
                         i = "{.arg {nm_x}} has length {.val {length(x)}}."))
    if (is.na(x))
        cli::cli_abort("{.arg {nm_x}} is {.val {NA}}.")
    if (!nzchar(x))
        cli::cli_abort("{.arg {nm_x}} is blank.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check bage_ssvd Object has Sex/Gender Dimension
#'
#' @param x Object of class 'bage_ssvd'
#' @param nm_x Name used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_ssvd_has_sexgender <- function(x, nm_x) {
  if (!has_sexgender(x)) {
    cli::cli_abort("{.arg {nm_x}} does not have a sex/gender dimension.")
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Age-Sex Details of SVD Prior Align with Term
#'
#' @param prior Object of class 'bage_prior' that involves SVD
#' @param nm Name of term
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sex/gender variable or NULL
#' @param agesex Type of term. Return value from function
#' 'make_agesex'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_svd_agesex <- function(prior,
                             nm,
                             var_age,
                             agesex) {
  str_nm_prior <- str_nm_prior(prior)
  has_age <- !is.null(var_age)
  msg1 <- "Problem with {.var {str_nm_prior}} prior for term {.var {nm}}."
  ## check that 'var_age' has been identified
  if (!has_age)
    cli::cli_abort(c(msg1,
                     i = "Can't use {.var {str_nm_prior}} prior when age variable not yet identified.",
                     i = "Use function {.fun set_var_age} to identify age variable?"))
  ## check that 'agesex' is not "other"
  if (agesex == "other")
    cli::cli_abort(c(msg1,
                     i = "{.var {str_nm_prior}} prior should be used with terms involving age."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Variance or Precision Returned by TMB has No NAs
#'
#' @param x Variance or precision matrix
#' @param est object returned by TMB (a named list)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_var_prec <- function(x, est) {
  has_na <- is.na(Matrix::diag(x))
  if (any(has_na)) {
    index_term <- which(has_na)
    nm_term <- get_term_from_est(est = est, index_term = index_term)
    cli::cli_abort(c("Problem deriving posterior distribution.",
                     i = "Estimation of posterior variance for {.val {nm_term}} term{?s} failed.",
                     i = "You may need to change the specification of your model."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'vars_inner' Argument
#'
#' @param vars_inner
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_vars_inner <- function(vars_inner) {
  ## is character
  if (!is.character(vars_inner))
    cli::cli_abort(c("{.arg vars_inner} is not a character vector.",
                     i = "{.arg vars_inner} has class {.cls {class(vars_inner)}}."))
  ## not length 0
  if (identical(length(vars_inner), 0L))
    cli::cli_abort("{.arg vars_inner} has length {.val {0}}.")
  ## no NAs
  n_na <- sum(is.na(vars_inner))
  if (n_na > 0L)
    cli::cli_abort("{.arg vars_inner} has {cli::qty(n_na)} NA{?s}.")
  ## blanks
  n_blank <- sum(!nzchar(vars_inner))
  if (n_blank > 0L)
    cli::cli_abort("{.arg vars_inner} has {cli::qty(n_blank)} blank{?s}.")
  ## duplicated
  n_dup <- sum(duplicated(vars_inner))
  if (n_dup > 0L)
    cli::cli_abort("{.arg vars_inner} has {cli::qty(n_dup)} duplicate{?s}.")
  ## return
  invisible(TRUE)
}



## HAS_TESTS
#' Check that 'widths' consists of numbers between 0 and 1
#'
#' @param width s
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_widths <- function(widths) {
    if (!is.numeric(widths))
        cli::cli_abort(c("{.arg widths} is non-numeric",
                         i = "{.arg widths} has class {.cls {class(widths)}}."))
    if (length(widths) == 0L)
        cli::cli_abort("{.arg widths} has length {.val {0}}.")
    n_na <- sum(is.na(widths))
    if (n_na > 0L)
        cli::cli_abort("{.arg widths} has {cli::qty(n_na)} NA{?s}.")
    n_out <- sum((widths <= 0) | (widths > 1))
    if (n_out > 0L)
        cli::cli_abort(c("{.arg widths} has {cli::qty(n_out)} value{?s} not in interval (0, 1]",
                         i = "{.arg widths}: {.val {widths}}"))
    invisible(TRUE)
}


