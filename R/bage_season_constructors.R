
## User-visible constructor ---------------------------------------------------

## NO_TESTS
#' Seasonal effect
#'
#' Add a seasonal effect to a main effect for time.
#'
#' TODO - description
#'
#' @param n Number of seasons.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#'
#' @returns An object of class `bage_season`.
#'
#' @examples
#' Season(n = 4)
#' Season(n = 12, s = 0.1)
#' @export
Season <- function(n, s = 1) {
    scale <- check_and_tidy_scale(s, x_arg = "s") 
    new_bage_season(n = n, scale = scale)
}



## Internal constructor -------------------------------------------------------

## Assume that inputs are all correct.
## (Checking is done by user-visible functions.)

## NO_TESTS
new_bage_season <- function(n, scale) {
    ans <- list(n = n,
                scale = scale)
    class(ans) <- "bage_season"
    ans
}

