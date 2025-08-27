
#' Data Models
#'
#' @description 
#' The models for rates, probabilities, or means
#' created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] can be extended by adding data models,
#' also referred to as measurement error models.
#' Data models can only be use with the outcome variable,
#' with one exception: in Poisson models where dispersion
#' is set to 0 (via [set_disp()]), a data model
#' can be used with exposure.
#'
#' @details
#'
#' | **Function**        | **Assumptions about measurement error**         | **pois** | **binom** | **norm** |
#' |---------------------|------------------------------------|----------|-----------|----------|
#' | [set_datamod_exposure()] | Exposure has positive and negative errors | Yes*     | No       | No       |
#' | [set_datamod_miscount()] | Outcome has positive and negative errors | Yes      | No       | No       |
#' | [set_datamod_noise()] | Outcome has positive and negative errors | No      | No       | Yes       |
#' | [set_datamod_overcount()] | Outcome has positive errors | Yes      | No       | No       |
#' | [set_datamod_undercount()] | Outcome has negative errors | Yes      | No       | No       |
#'
#' *Only models with no dispersion.
#' 
#' @name datamods
NULL
