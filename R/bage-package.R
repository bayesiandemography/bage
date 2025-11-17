
#' Package 'bage'
#'
#' Bayesian estimation and forecasting of age-specific rates.
#' Estimation uses [TMB](https://CRAN.R-project.org/package=TMB),
#' and is fast.
#'
#' @section Example workflow:
#'
#' 1. Specify model using [mod_pois()]
#' 2. Fit model using [fit()]
#' 3. Extract results using [augment()]
#' 4. Check model using [replicate_data()]
#'
#' @section Functions:
#'
#' **Specify model**
#'
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [set_prior()] Specify prior for main effect or interaction
#' - [priors] Overview of priors for main effects or interactions
#' - [set_disp()] Specify prior for dispersion/variance
#' - [set_covariates()] Add covariates to model
#' - [datamods] Overview of data models (measurement error models)
#' - [confidential] Overview of confidentialization models
#'
#' **Fit model**
#'
#' - [bage::fit()] Derive posterior distribution
#'
#' **Extract results**
#'
#' - [augment()] Original data, plus observation-level estimates
#' - [components()][bage::components.bage_mod] Hyper-parameters
#' - [dispersion()] Dispersion parameter (a type of hyper-parameter)
#' - [tidy()] One-line summary
#' - [set_n_draw()] Specify number of prior or posterior draws
#'
#' **Forecast**
#'
#' - [forecast()] Use model to obtain estimates for future periods
#'
#' **Check model**
#'
#' - [replicate_data()] Compare real and simulated data
#' - [report_sim()] Simulation study of model
#'
#' @section Data:
#'
#' - [datasets] Overview of datasets
#' - [svds] Overview of scaled SVDs
#'
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rvec n_draw
#' @importFrom tibble tibble
#' @useDynLib bage, .registration = TRUE
## usethis namespace: end
NULL
