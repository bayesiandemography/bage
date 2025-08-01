
#' Confidentialization
#'
#' @description 
#' The models for rates, probabilities, or means
#' created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] can be extended by adding
#' descriptions of confidentalization procedures applied
#' to the outcome variable. 
#'
#' @details
#'
#' **Data models for outcome variable**
#' 
#' | **Function**        | **Confidentialization procedure**         | **pois** | **binom** | **norm** |
#' |---------------------|------------------------------------|----------|-----------|----------|
#' | [set_confidential_rr3()] | Outcome randomly rounded to multiple of 3 | Yes      | Yes       | No       |
#'
#' @name confidential
NULL
