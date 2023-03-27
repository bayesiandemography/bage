
## HAS_TESTS
#' Specify a Poisson model
#'
#' Specify a model where the outcome is drawn from
#' a Poisson distribution.
#'
#' - `formula` specifies the outcome and predictors,
#' including interactions between predictors.
#' It follows standard R [formula][stats::formula()]
#' conventions, except that it cannot include
#' transformations (e.g. `sqrt(deaths)`).
#' - `data` holds the outcome, the predictors, and,
#' optionally, exposure.
#' - `exposure` is the name (bare or quoted) of the variable
#' in `data` used to measure exposure, or, if the
#' model does include exposure, a `1`.
#'
#' 
#'
#' If the model includes exposure, then the
#' the first level of the model is
#'
#' \deqn{y \sim \text{Poisson}(\mu w)}
#'
#' where \eqn{\mu} is the underlying rate, and
#' \eqn{w} is exposure. If the model does not
#' include exposure, then the first level is
#'
#' \deqn{y \sim \text{Poisson}(\mu)}
#'
#' TODO - Include error term once specification finalised.
#'
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing the outcome,
#' predictors, and, optionally, exposure.
#' @param exposure Name of the exposure variable,
#' or a `1`.
#'
#' @returns An object of class `bage_mod_pois`.
#'
#' @seealso
#' - [mod_binom()] and [mod_norm()] for specification
#' of binomial and normal models
#' - [set_prior()] to specify non-default priors
#' - [fit()] to fit a model
#'
#' @examples
#' ## model with exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn)
#'
#' ## model without exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = 1)
#' @export
mod_pois <- function(formula, data, exposure) {
    n_draw <- 1000L
    nm_distn <- "pois"
    nm_offset <- "exposure"
    ## check individual inputs
    checkmate::assert_formula(formula)
    check_formula_has_response(formula)
    check_formula_has_predictors(formula)
    checkmate::assert_data_frame(data)
    ## process 'exposure'
    exposure <- deparse1(substitute(exposure))
    exposure <- gsub("^\\\"|\\\"$", "", exposure)
    is_offset_specified <- !identical(exposure, "1")
    vname_offset <- if (is_offset_specified) exposure else NULL
    ## check consistency between inputs
    check_formula_vnames_in_data(formula = formula,
                                 data = data)
    check_response_nonneg(formula = formula,
                          data = data,
                          nm_distn = nm_distn)
    if (is_offset_specified) {
        check_offset_in_data(vname_offset = vname_offset,
                             nm_offset = nm_offset,
                             data = data)
        check_offset_nonneg(vname_offset = vname_offset,
                            nm_offset = nm_offset,
                            data = data)
        check_resp_zero_if_offset_zero(formula = formula,
                                       vname_offset = vname_offset,
                                       data = data)                                       
    }
    ## process inputs
    data <- tibble(data)
    outcome <- make_outcome_array(formula = formula,
                                  data = data)
    if (is_offset_specified)
        offset <- make_offset_array(formula = formula,
                                    vname_offset = vname_offset,
                                    data = data)
    else
        offset <- make_offset_ones_array(formula = formula,
                                         data = data)
    var_age <- infer_var_age(formula)
    var_sexgender <- infer_var_sexgender(formula)
    var_time <- infer_var_time(formula)
    if (is_offset_specified)
        scale_prior <- 1
    else
        scale_prior <- make_scale_outcome(outcome, log = TRUE)
    priors <- make_priors(formula = formula,
                          scale = scale_prior,
                          var_age = var_age,
                          var_time = var_time)
    matrices_par <- make_matrices_par_array(formula = formula,
                                            outcome = outcome)
    ## create object and return
    est <- NULL
    prec <- NULL
    ans <- list(formula = formula,
                data = data,
                outcome = outcome,
                offset = offset,
                vname_offset = vname_offset,
                scale_prior = scale_prior,
                priors = priors,
                var_age = var_age,
                var_sexgender = var_sexgender,
                var_time = var_time,
                matrices_par = matrices_par,
                est = est,
                prec = prec,
                n_draw = n_draw)
    class(ans) <- c("bage_mod_pois", "bage_mod")
    ans
}


## HAS_TESTS
#' Specify a binomial model
#'
#' Specify a model where the outcome is drawn from
#' a binomial distribution.
#'
#' - `formula` specifies the outcome and predictors,
#' including interactions between predictors.
#' It follows standard R [formula][stats::formula()]
#' conventions, except that it cannot include
#' transformations (e.g. `sqrt(deaths)`).
#' - `data` A data frame holding the outcome, the predictors,
#' and number of trials.
#' - `size` is the name (bare or quoted) of the variable
#' in `data` measuring the number of trials.
#'
#' The first level of the model is
#'
#' \deqn{y \sim \text{binom}(n, \pi)}
#'
#' where \eqn{\pi} is the sucess probability,
#' and \eqn{n} is the number of trials.
#'
#' TODO - Include error term once specification finalised.
#'
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing the outcome,
#' predictors, and, number of trials
#' @param size Name of the variable describing
#' the number of trials.
#'
#' @returns An object of class `bage_mod`.
#'
#' @seealso
#' - [mod_pois()] and [mod_norm()] for specification
#' of Poisson and normal models
#' - [set_prior()] to specify non-default priors
#' - [fit()] to fit a model
#'
#' @examples
#' mod <- mod_binom(oneperson ~ age:region + age:year,
#'                  data = households,
#'                  size = total)
#' @export
mod_binom <- function(formula, data, size) {
    n_draw <- 1000L
    scale_prior <- 1
    nm_distn <- "binom"
    nm_offset <- "size"
    ## check individual inputs
    checkmate::assert_formula(formula)
    check_formula_has_response(formula)
    check_formula_has_predictors(formula)
    checkmate::assert_data_frame(data)
    ## process 'size'
    size <- deparse1(substitute(size))
    size <- gsub("^\\\"|\\\"$", "", size)
    vname_offset <- size
    ## check consistency between inputs
    check_formula_vnames_in_data(formula = formula,
                                 data = data)
    check_response_nonneg(formula = formula,
                          data = data,
                          nm_distn = nm_distn)
    check_offset_in_data(vname_offset = vname_offset,
                         nm_offset = nm_offset,
                         data = data)
    check_offset_nonneg(vname_offset = vname_offset,
                        nm_offset = nm_offset,
                        data = data)
    check_resp_zero_if_offset_zero(formula = formula,
                                   vname_offset = vname_offset,
                                   data = data)
    check_resp_le_offset(formula = formula,
                         vname_offset = vname_offset,
                         data = data)
    ## process inputs
    data <- tibble(data)
    outcome <- make_outcome_array(formula = formula,
                                  data = data)
    offset <- make_offset_array(formula = formula,
                                vname_offset = vname_offset,
                                data = data)
    var_age <- infer_var_age(formula)
    var_sexgender <- infer_var_sexgender(formula)
    var_time <- infer_var_time(formula)
    priors <- make_priors(formula = formula,
                          scale = scale_prior,
                          var_age = var_age,
                          var_time = var_time)
    matrices_par <- make_matrices_par_array(formula = formula,
                                            outcome = outcome)
    ## create object and return
    est <- NULL
    prec <- NULL
    ans <- list(formula = formula,
                data = data,
                outcome = outcome,
                offset = offset,
                vname_offset = vname_offset,
                scale_prior = scale_prior,
                priors = priors,
                var_age = var_age,
                var_sexgender = var_sexgender,
                var_time = var_time,
                matrices_par = matrices_par,
                est = est,
                prec = prec,
                n_draw = n_draw)
    class(ans) <- c("bage_mod_binom", "bage_mod")
    ans
}


## HAS_TESTS
#' Specify a normal model
#'
#' Specify a model where the outcome is drawn from
#' a normal distribution.
#'
#' - `formula` specifies the outcome and predictors,
#' including interactions between predictors.
#' It follows standard R [formula][stats::formula()]
#' conventions, except that it cannot include
#' transformations (e.g. `sqrt(deaths)`).
#' - `data` holds the outcome, the predictors, and,
#' optionally, weights.
#' - `weights` is the name (bare or quoted) of the variable
#' in `data` used as weights, or, if the
#' model does include weights, a `1`.
#'
#' If the model includes weights, then the
#' the first level of the model is
#'
#' \deqn{y \sim \text{norm}(\mu, \sigma^2 / w)}
#'
#' where \eqn{\mu} is the underlying rate, and
#' \eqn{w} is weights. If the model does not
#' include weights, then the first level is
#'
#' \deqn{y \sim \text{norm}(\mu, \sigma^2)}
#'
#' TODO - Include error term once specification finalised.
#'
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing the outcome,
#' predictors, and, optionally, weights.
#' @param weights Name of the weights variable,
#' or a `1`.
#'
#' Internally, outcome scaled to have mean 0 and sd 1;
#' weights scaled to have mean 1.
#'
#' @returns An object of class `bage_mod`.
#'
#' @seealso
#' - [mod_pois()] and [mod_binom()] for specification
#' of Poisson and binomial models
#' - [set_prior()] to specify non-default priors
#' - [fit()] to fit a model
#'
#' @examples
#' mod <- mod_norm(value ~ diag:age + year,
#'                 data = expenditure,
#'                 weights = 1)
#' @export
mod_norm <- function(formula, data, weights) {
    n_draw <- 1000L
    nm_offset <- "weights"
    ## check individual inputs
    checkmate::assert_formula(formula)
    check_formula_has_response(formula)
    check_formula_has_predictors(formula)
    checkmate::assert_data_frame(data)
    ## process 'weights'
    weights <- deparse1(substitute(weights))
    weights <- gsub("^\\\"|\\\"$", "", weights)
    is_offset_specified <- !identical(weights, "1")
    vname_offset <- if (is_offset_specified) weights else NULL
    ## check consistency between inputs
    check_formula_vnames_in_data(formula = formula,
                                 data = data)
    if (is_offset_specified) {
        check_offset_in_data(vname_offset = vname_offset,
                             nm_offset = nm_offset,
                             data = data)
        check_offset_nonneg(vname_offset = vname_offset,
                            nm_offset = nm_offset,
                            data = data)
    }
    ## process inputs
    data <- tibble(data)
    outcome <- make_outcome_vec(formula = formula,
                                data = data)
    if (is_offset_specified)
        offset <- make_offset_vec(vname_offset = vname_offset,
                                  data = data)
    else
        offset <- make_offset_ones_vec(data)
    scale_prior <- make_scale_outcome(outcome, log = FALSE)
    var_age <- infer_var_age(formula)
    var_sexgender <- infer_var_sexgender(formula)
    var_time <- infer_var_time(formula)
    priors <- make_priors(formula = formula,
                          scale = scale_prior,
                          var_age = var_age,
                          var_time = var_time)
    matrices_par <- make_matrices_par_vec(formula = formula,
                                          data = data)
    ## create object and return
    est <- NULL
    prec <- NULL
    ans <- list(formula = formula,
                data = data,
                outcome = outcome,
                offset = offset,
                vname_offset = vname_offset,
                scale_prior = scale_prior,
                priors = priors,
                var_age = var_age,
                var_sexgender = var_sexgender,
                var_time = var_time,
                matrices_par = matrices_par,
                est = est,
                prec = prec,
                n_draw = n_draw)
    class(ans) <- c("bage_mod_norm", "bage_mod")
    ans
}




