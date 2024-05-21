
#' Package 'bage'
#'
#' Bayesian estimation and forecasting of age-specific rates.
#' Estimation uses [TMB](https://CRAN.R-project.org/package=TMB),
#' and is fast.
#'
#' @section Functions:
#'
#' **Specify model**
#'
#' - [mod_pois()] Specify a Poisson model
#' - [mod_binom()] Specify a binomial model
#' - [mod_norm()] Specify a normal model
#' - [set_prior()] Specify prior for main effect or interaction
#' - [priors] List of available priors for main effects or interactions
#' - [set_disp()] Specify prior for dispersion/variance
#' - [set_var_age()] Identify age variable in data
#' - [set_var_sexgender()] Identify sex or gender variable in data
#' - [set_var_time()] Identify time variable in data
#'
#' **Fit model**
#'
#' - [fit()] Derive posterior distribution
#' - [is_fitted()] See if model has been fitted
#'
#' **Extract results**
#'
#' - [augment()] Add cell-level estimates to data
#' - [components()] Hyper-parameters
#' - [tidy()] One-line summary
#' - [set_n_draw()] Specify number of posterior draws
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
#' **Data**
#'
#' - [deaths] Deaths in Iceland
#' - [divorces] Divorces in New Zealand
#' - [expenditure] Health expenditure in the Netherlands
#' - [us_acc_deaths] Accidental deaths in the USA
#' - [HMD] SVD of mortality rates from Human Mortality Database
#' - [LFP] SVD of Labour Force Participation rates from OECD
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @useDynLib bage, .registration = TRUE
## usethis namespace: end
NULL
