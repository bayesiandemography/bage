
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
#' - [components()][bage::components.bage_mod] Hyper-parameters
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
#' **SVD-based modelling of age profiles**
#'
#' - [components()][bage::components.bage_ssvd] Matrices and offsets from scaled SVD
#' - [generate()][bage::generate.bage_ssvd] Random profiles
###' - [HFD] Scaled SVD of fertility rates from Human Fertility Database
#' - [HMD] Scaled SVD of mortality rates from Human Mortality Database
#' - [LFP] Scaled SVD of labor force participation rates from OECD
#'
#' **Data**
#'
#' - [isl_deaths] Deaths in Iceland
#' - [kor_births] Births in South Korea
#' - [nld_expenditure] Health expenditure in the Netherlands
#' - [nzl_divorces] Divorces in New Zealand
#' - [nzl_households] One-person households in New Zealand
#' - [nzl_injuries] Fatal injuries in New Zealand
#' - [swe_infant] Infant mortality in Sweden
#' - [usa_deaths] Accidental deaths in the USA
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @importFrom rvec n_draw
#' @useDynLib bage, .registration = TRUE
## usethis namespace: end
NULL
