
#' Priors for Intercept, Main Effects, Interactions
#'
#' @description 
#' The models created with [mod_pois()], [mod_binom()],
#' and [mod_norm()] include terms such as age effects
#' and region-time interactions. Each of these terms
#' requires a prior distribution. Current options
#' for these priors are summarised in the table below.
#'
#' @details
#'
#' | **Prior** |   **Description**        | **Uses**                     | **Forecast** | **Along/By** |
#' |-----------|--------------------------|------------------------------|------|-----|
#' | [N()]     | Elements drawn from normal distribution | Term with no natural order | Yes | No |
#' | [NFix()]  | `N()` with standard deviation fixed | Term with few elements | Yes | No |
#' | [Known()] | Values treated as known | Simulations, prior knowledge | No | No |
#' | [RW()]    | Random walk | Smoothing | Yes | Yes |
#' | [RW2()]   | Second-order random walk | Like `RW()`, but with trends | Yes | Yes |
#' | [DRW()]    | Damped random walk | Smoothing, forecasting | Yes | Yes |
#' | [DRW2()]   | Damped second-order random walk | Like `DRW()`, but with trends | Yes | Yes |
#' | [RW2_Infant()]   | `RW2()` with infant indicator | Mortality age profiles | No | Yes |
#' | [RW_Seas()] | `RW()`, with seasonal effect | Terms involving time | Yes | Yes |
#' | [RW2_Seas()] | `RW2()`, with seasonal effect | Term involving time | Yes | Yes |
#' | [AR()]    | Auto-regressive prior of order *k* | Mean reversion, forecasting | Yes | Yes |
#' | [AR1()]   | Special case of `AR()` | Mean reversion, forecasting | Yes | Yes |
#' | [Lin()]   | Linear trend, with independent errors | Parsimonious model for time | Yes | Yes |
#' | [Lin_AR()] | Linear trend, with AR errors | Term involving time, forecasting | Yes | Yes |
#' | [Lin_AR1()] | Linear trend, with AR1 errors | Terms involving time, forecasting | Yes | Yes |
#' | [Sp()]    | P-Spline (penalised spline) | Smoothing, eg over age | No | Yes |
#' | [SVD()]   | Age-sex profile based on SVD | Age or age-sex | No | No |
#' | [SVD_AR()] | `SVD()`, but coefficients follow `AR()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_AR1()] | `SVD()`, but coefficients follow `AR1()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_Lin()] | `SVD()`, but coefficients follow `Lin()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_RW()] | `SVD()`, but coefficients follow `RW()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_RW2()] | `SVD()`, but coefficients follow `RW2()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_DRW()] | `SVD()`, but coefficients follow `DRW()` | Age or age-sex and time | Yes | Yes |
#' | [SVD_DRW2()] | `SVD()`, but coefficients follow `DRW2()` | Age or age-sex and time | Yes | Yes |
#'
#' @section 'Along' and 'by' dimensions:
#'
#' Priors for interaction terms often consist of a time-series-style model
#' along one dimension, with a separate series for each
#' combination of the remaining dimensions. For instance,
#' a prior for an age-sex-time interaction might
#' consist of a separate random walk along time
#' for each combination of age-group and sex.
#' In \pkg{bage} the dimension with the time-series-type model is
#' referred to as the 'along' dimension, and the remaining dimensions
#' are referred to as the 'by' dimensions.
#' 
#' @section Default prior:
#'
#' If no prior is specified for a term, then \pkg{bage} assigns the term a default prior using
#' the following algorithm:
#'
#' - if the term has one or two elements, use [NFix()];
#' - otherwise, if the term involves time, use [RW()], with time as the 'along' dimension;
#' - otherwise, if the term involves age, use [RW()], with age as the 'along' dimension;
#' - otherwise, use [N()].
#'
#' @section Forecasting:
#'
#' A model can only be used for forecasting if
#'
#' - the model includes a time dimension, and
#' - the prior for the time dimension supports forecasting.
#'
#' If necessary, the time dimension can be identified
#' using [set_var_time()]. The table above lists the
#' priors that support forecasting.
#' 
#' @name priors
NULL
