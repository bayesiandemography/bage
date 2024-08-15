
#' Priors for Intercept, Main Effects, Interactions
#'
#' @description 
#' The models created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] always include an intercept,
#' and typically include main effects
#' and interactions formed from variables in input data.
#' Most models, for instance
#' include an age effect, and many include
#' an interaction between age and sex/gender,
#' or age and time.
#'
#' The intercept, main effects, and interactions
#' all have prior models that capture the expected
#' behavior of the term. Current choices of prior
#' models are summarised here.
#'
#' @details
#'
#' | **Prior** |   **Description**        | **Uses**                     | **Forecast** |
#' |-----------|--------------------------|------------------------------|------|
#' | [N()]     | Elements drawn from normal distribution | Term with no natural order | Yes |
#' | [NFix()]  | As for `N()`, but standard deviation fixed | Term with few elements | Yes |
#' | [RW()]    | Random walk | Smoothing | Yes |
#' | [RW2()]   | Second-order random walk | Like `RW()`, but smoother | Yes |
#' | [RW_Seas()] | Random walk, with seasonal effect | Terms involving time | Yes |
#' | [RW2_Seas()] | Second-order random walk, with seasonal effect | Term involving time | Yes |
#' | [AR()]    | Auto-regressive prior of order *k* | Mean reversion | Yes |
#' | [AR1()]   | Auto-regressive prior of order 1 Special case of `AR()` | Mean reversion | Yes |
#' | [Known()] | Values  treated as known | Simulations,  prior knowledge | No |
#' | [Lin()]   | Linear trend, with independent normal | Parsimonious model for time | Yes |
#' | [Lin_AR()] | Linear trend, with autoregressive errors | Term involving time | Yes |
#' | [Lin_AR1()] | Linear trend, with AR1 errors | Terms involving time | Yes |
#' | [Sp()]    | P-Spline (penalised spline) | Smoothing, eg over age | No |
#' | [SVD()]   | Age or age-sex profile based on SVD of database | Age or age-sex | No |
#' | [SVD_AR()] | `SVD()`, but coefficients follow `AR()` | Age or age-sex and time | Yes |
#' | [SVD_AR1()] | `SVD()`, but coefficients follow `AR1()` | Age or age-sex and time | Yes |
#' | [SVD_RW()] | `SVD()`, but coefficients follow `RW()` | Age or age-sex and time | Yes |
#' | [SVD_RW2()] | `SVD()`, but coefficients follow `RW2()` | Age or age-sex and time | Yes |
#'
#' @name priors
NULL
