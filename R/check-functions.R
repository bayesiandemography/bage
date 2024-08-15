
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
        cli::cli_abort(c("{.arg {nm_x}} does not have length 1",
                         i = "{.arg {nm_x}} has length {length(x)}."))
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
    has_response <- attr(stats::terms(formula), "response") > 0L
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
    factors <- attr(stats::terms(formula), "factors")
    varnames <- rownames(factors)[-1L]
    if (!(name %in% varnames))
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
    nms_formula <- rownames(attr(stats::terms(formula), "factors"))
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
#' Check that Along Dimension of Interaction has at Least 'min' Elements
#'
#' @param length_along Number of elements
#' @param min Minimum number of elements
#' @param nm Name of term
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_length_along_ge <- function(length_along, min, nm, prior) {
    if (length_along < min)
        cli::cli_abort(c(paste("{.var {str_call_prior(prior)}} prior cannot be",
                               "used for {.var {nm}} term."),
                         i = paste("{.var {str_call_prior(prior)}} prior can only be",
                                   "used with interactions where the 'along' dimension has at least {min} element{?s}."),
                         i = "The 'along' dimension of {.var {nm}} has {length_along} element{?s}."))
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
    nm_outcome_sim <- get_nm_outcome(mod_est)
    nm_outcome_est <- get_nm_outcome(mod_sim)
    if (!identical(nm_outcome_sim, nm_outcome_est))
        cli::cli_abort(c("{.arg mod_est} and {.arg mod_sim} have different outcome variables.",
                         i = "Outcome variable for {.arg mod_est}: {.val {nm_outcome_sim}}.",
                         i = "Outcome variable for {.arg mod_sim}: {.val {nm_outcome_est}}."))
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
    for (nm in setdiff(nms_sim, nm_outcome_sim)) {
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
#' Check 'n' argument
#'
#' @param n A whole number
#' @param nm_n Name for 'n' to be used in error messages
#' @param min,max Minimum and maximum values 'n' can take
#' @param null_ok Whether passing NULL (and skipping tests) is allowed
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n, nm_n, min, max, null_ok) {
    if (null_ok && is.null(n)) 
        return(invisible(TRUE))
    if (!is.numeric(n))
        cli::cli_abort(c("{.arg {nm_n}} is non-numeric.",
                         i = "{.arg {nm_n}} has class {.cls {class(n)}}."))
    if (length(n) != 1L)
        cli::cli_abort(c("{.arg {nm_n}} does not have length 1.",
                         i = "{.arg {nm_n}} has length {length(n)}."))
    if (is.na(n))
        cli::cli_abort("{.arg {nm_n}} is {.val {NA}}.")
    if (is.infinite(n))
        cli::cli_abort("{.arg {nm_n}} is {.val {Inf}}.")
    if (!isTRUE(all.equal(round(n), n)))
        cli::cli_abort(c("{.arg {nm_n}} is not an integer.",
                         i = "{.arg {nm_n}} is {.val {n}}."))
    if (n < min)
        cli::cli_abort(c("{.arg {nm_n}} is less than {min}.",
                         i = "{.arg {nm_n}} is {.val {n}}."))
    if (!is.null(max) && (n > max))
        cli::cli_abort(c("{.arg {nm_n}} is greater than {max}.",
                         i = "{.arg {nm_n}} is {.val {n}}."))
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
    cli::cli_abort("{.arg {nm_x}} has length 0.")
  if (anyNA(x))
    cli::cli_abort("{.arg {nm_x}} has {.val {NA}}.")
  if (any(is.infinite(x)))
    cli::cli_abort("{.arg {nm_x}} has non-finite value.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check offset occurs in 'data'
#'
#' @param vname_offset The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_in_data <- function(vname_offset, nm_offset, data) {
  is_formula <- startsWith(vname_offset, "~")
  if (is_formula) {
    ans <- tryCatch(eval_offset_formula(vname_offset = vname_offset, data = data),
                    error = function(e) e)
    if (inherits(ans, "error"))
      cli::cli_abort(c("Problem with formula used for {.arg {nm_offset}}.",
                       i = "Formula: {.val {vname_offset}}.",
                       i = ans$message))
  }
  else {
    nms_data <- names(data)
    if (!(vname_offset %in% nms_data)) {
      cli::cli_abort(c("{.arg {nm_offset}} not found in {.arg data}.",
                       i = "{.arg {nm_offset}}: {.val {vname_offset}}."))
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Offset Has No Negative Values
#'
#' @param vname_offset The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_nonneg <- function(vname_offset, nm_offset, data) {
  is_formula <- startsWith(vname_offset, "~")
  if (is_formula) {
    offset <- eval_offset_formula(vname_offset = vname_offset, data = data)
  }
  else {
    offset <- data[[vname_offset]]
  }
  n_neg <- sum(offset < 0, na.rm = TRUE)
  if (n_neg > 0L)
    cli::cli_abort(c("{.arg {nm_offset}} has negative {cli::qty(n_neg)} value{?s}.",
                     i = "{nm_offset}: {.val {vname_offset}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check Offset not Used in 'formula' Agument
#'
#' Applied only when offset is the name of a variable.
#'
#' @param vname_offset The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param formula Formula specifying model
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_offset_not_in_formula <- function(vname_offset, nm_offset, formula) {
  is_offset_formula <- startsWith(vname_offset, "~")
  nms_formula <- rownames(attr(stats::terms(formula), "factors"))
  if (!is_offset_formula) {
    if (vname_offset %in% nms_formula) {
      cli::cli_abort(c("{.arg {nm_offset}} included in {.arg formula}.",
                       i = "{.arg {nm_offset}}: {.val {vname_offset}}.",
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
#' @param vname_offset The name of the variable being
#' used as an offset, or a formula
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_le_offset <- function(formula,
                                 vname_offset,
                                 nm_offset,
                                 data) {
  nm_response <- deparse1(formula[[2L]])
  response <- data[[nm_response]]
  is_offset_formula <- startsWith(vname_offset, "~")
  if (is_offset_formula)
    offset <- eval_offset_formula(vname_offset = vname_offset, data = data)
  else
    offset <- data[[vname_offset]]
  is_gt_offset <- !is.na(response) & !is.na(offset) & (response > offset)
  i_gt_offset <- match(TRUE, is_gt_offset, nomatch = 0L)
  if (i_gt_offset > 0L) {
    cli::cli_abort(c("Response greater than {.var {nm_offset}}.",
                     i = "Response: {.var {nm_response}}.",
                     i = "{.var {nm_offset}}: {.val {vname_offset}}.",
                     i = "Value for response: {.val {response[[i_gt_offset]]}}",
                     i = "Value for {.var {nm_offset}}: {.val {offset[[i_gt_offset]]}}"))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that response variable always zero when
#' offset variable is zero
#'
#' @param formula A formula
#' @param vname_offset The name of the variable being
#' used as an offset
#' @param nm_offset The name used to refer to the
#' offset in user-visible functions
#' @param data A data frame
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_resp_zero_if_offset_zero <- function(formula,
                                           vname_offset,
                                           nm_offset,
                                           data) {
  nm_response <- deparse1(formula[[2L]])
  response <- data[[nm_response]]
  is_offset_formula <- startsWith(vname_offset, "~")
  if (is_offset_formula)
    offset <- eval_offset_formula(vname_offset = vname_offset, data = data)
  else
    offset <- data[[vname_offset]]
  response_pos <- response > 0
  offset_pos <- offset > 0
  is_pos_nonpos <- !is.na(response) & !is.na(offset) & response_pos & !offset_pos
  i_pos_nonpos <- match(TRUE, is_pos_nonpos, nomatch = 0L)
  if (i_pos_nonpos > 0L)
    cli::cli_abort(c("Response is non-zero but {.var {nm_offset}} is zero.",
                     i = "Response: {.var {nm_response}}.",
                     i = "{.var {nm_offset}}: {.var {vname_offset}}.",
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
        cli::cli_abort(c("{.arg {nm_x}} does not have length 1.",
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
#' Check that Term with SVD+Time Prior has Time Dimension
#'
#' @param prior Object of class 'bage_prior'
#' @param nm Name of term
#' @param vname_time Name of time dimension, or NULL
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_svd_time <- function(prior, nm, var_time) {
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
        cli::cli_abort("{.arg widths} has length 0.")
    n_na <- sum(is.na(widths))
    if (n_na > 0L)
        cli::cli_abort("{.arg widths} has {cli::qty(n_na)} NA{?s}.")
    n_out <- sum((widths <= 0) | (widths > 1))
    if (n_out > 0L)
        cli::cli_abort(c("{.arg widths} has {cli::qty(n_out)} value{?s} not in interval (0, 1]",
                         i = "{.arg widths}: {.val {widths}}"))
    invisible(TRUE)
}
