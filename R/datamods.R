
#' Data Models
#'
#' @description 
#' The models for rates, probabilities, or means
#' created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] can be extended by adding data models,
#' also referred to as measurement error models.
#'
#' @details
#'
#' | **Function**        | **Assumptions about measurement error**         | **Poisson** | **Binomial** | **Normal** |
#' |---------------------|------------------------------------|----------|-----------|----------|
#' | [set_datamod_miscount()] | Reported outcome has undercount and overcount | Yes | No | No |
#' | [set_datamod_undercount()] | Reported outcome has undercount | Yes | Yes | No |
#' | [set_datamod_overcount()] | Reported outcome has overcount | Yes | No | No |
#' | [set_datamod_noise()] | Reported outcome unbiased, but with positive and negative measurement errors | Yes* | No | Yes |
#' | [set_datamod_exposure()] | Reported exposure unbiased, but with positive and negative measurement errors | Yes* | No | No |
#'
#' *Models with no dispersion term for rates.
#' 
#' @name datamods
NULL
