
#' Data Models
#'
#' @description 
#' The models for rates, probabilities, or means
#' created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] can be extended by adding data models,
#' also referred to as measurement error models.
#' Data models can be applied to the outcome variable,
#' the exposure variable (in Poisson models), or the
#' size variable (in binomial models).
#'
#' @details
#'
#' **Data models for outcome variable**
#' 
#' | **Function**        | **Assumptions about data**         | **pois** | **binom** | **norm** |
#' |---------------------|------------------------------------|----------|-----------|----------|
#' | [set_datamod_outcome_rr3()] | Outcome randomly rounded to base 3 | Yes      | Yes       | No       |
#'
#' **Data models for exposure or size variable**
#'
#' *None implemented yet*
#' 
#' @name datamods
NULL
