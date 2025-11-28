
## Internal functions to extract parts of model object,
## or of 'components' or 'augment'

## HAS_TESTS
#' Get Fitted Values for 'coef' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_coef <- function(components, term) {
  is_term <- components$term == term
  is_hyper <- components$component == "hyper"
  is_coef <- startsWith(components$level, "coef")
  is_get <- is_term & is_hyper & is_coef
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}, component",
                         "{.val hyper}, and level starting with {.val coef}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'effect' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_effect <- function(components, term) {
  is_term <- components$term == term
  is_effect <- components$component == "effect"
  is_get <- is_term & is_effect
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}",
                         "and component {.val effect}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'sd' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_sd <- function(components, term) {
  is_term <- components$term == term
  is_hyper <- components$component == "hyper"
  is_sd <- components$level == "sd"
  is_get <- is_term & is_hyper & is_sd
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}},",
                         "component {.val hyper}, and level {.val sd}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'sd_seas' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_sd_seas <- function(components, term) {
  is_term <- components$term == term
  is_hyper <- components$component == "hyper"
  is_sd_seas <- components$level == "sd_seas"
  is_get <- is_term & is_hyper & is_sd_seas
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}},",
                         "component {.val hyper}, and level {.val sd_seas}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'season' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_season <- function(components, term) {
  is_term <- components$term == term
  is_season <- components$component == "season"
  is_get <- is_term & is_season
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}",
                         "and component {.val season}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'trend' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_slope <- function(components, term) {
  is_term <- components$term == term
  is_hyper <- components$component == "hyper"
  is_slope <- startsWith(components$level, "slope")
  is_get <- is_term & is_hyper & is_slope
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}, component",
                         "{.val hyper}, and level starting with {.val slope}."))
  components$.fitted[is_get]
}

## HAS_TESTS
#' Get Fitted Values for 'trend' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_svd <- function(components, term) {
  is_term <- components$term == term
  is_svd <- components$component == "svd"
  is_get <- is_term & is_svd
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}",
                         "and component {.val svd}."))
  components$.fitted[is_get]
}


## HAS_TESTS
#' Get Fitted Values for 'trend' from 'components'
#'
#' @param components A tibble with estimates for hyper-parameters
#' @param term Name of term
#'
#' @returns An rvec
#'
#' @noRd
get_from_comp_trend <- function(components, term) {
  is_term <- components$term == term
  is_trend <- components$component == "trend"
  is_get <- is_term & is_trend
  if (!any(is_get))
    cli::cli_abort(paste("Internal error: No values with term {.val {term}}",
                         "and component {.val trend}."))
  components$.fitted[is_get]
}


  
