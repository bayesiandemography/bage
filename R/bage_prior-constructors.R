
## User-visible constructors --------------------------------------------------

## 'bage_prior_ar' only ever created via 'set_prior()'

## HAS_TESTS
#' Autoregressive Prior
#'
#' Autoregressive prior with order `k`.
#'
#' @section Mathematical description:
#'
#' The model is
#'
#' \deqn{x_i = \phi_1 x_{i-1} + \cdots + \phi_k x_{i-k} + \epsilon_i}
#' \deqn{\epsilon_i \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{x_i} has
#' marginal variance \eqn{\sigma^2}. The value of
#' \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' The \eqn{\phi_j} are contrained values between -1 and 1.
#' 
#' @param n The order of the model.
#' Default is `2`.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Default is `1`.
#'
#' @returns An object of class `bage_prior_ar`.
#'
#' @seealso [N()], [RW()], [RW2()], [Known()].
#' The values for `min` and `max` are based on the
#' defaults for function `forecast::ets()`.
#' [AR1()]
#'
#' @references TMB documentation for
#' [ARk](http://kaskr.github.io/adcomp/classdensity_1_1ARk__t.html#details)
#'
#' @examples
#' AR(n = 3)
#' AR(n = 3, s = 2.4)
#' @export
AR <- function(n = 2, s = 1) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, null_ok = FALSE)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  n <- as.integer(n)
  scale <- as.double(s)
  new_bage_prior_ar(n = n,
                    min = -1,
                    max = 1,
                    scale = scale,
                    nm = "AR")
}


## 'bage_prior_ar1' only ever created via 'set_prior()'

## HAS_TESTS
#' AR1 prior
#'
#' Autoregressive prior of order 1
#'
#' @section Mathematical description:
#'
#' The model is
#' \deqn{x_0 \sim \text{N}(0, \sigma^2)}
#' \deqn{x_i = \phi x_{i-1} + \sqrt{1 - \phi^2}\epsilon_i}
#' \deqn{\epsilon \sim \text{N}(0, \sigma^2)}
#'
#' \eqn{\sigma} is drawn from a half-normal distribition
#' with scale set by the `s` argument.
#'
#' Correlation parameter \eqn{\phi} is constrained
#' to lie in the interval `(a, b)`,
#' where \eqn{a} = `min` and \eqn{b} = `max`.
#' The prior distribution is for \eqn{\phi}
#' is
#' \deqn{\phi = (b - a) \phi' - a}
#' where
#' \deqn{\phi' \sim \text{beta}(2, 2)}.
#'
#' @param min,max Minimum and maximum values
#' for autocorrelation parameter (\eqn{\phi}).
#' Defaults are `0.8` and `0.98`.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Default is `1`.
#'
#' @returns An object of class `bage_prior_ar`.
#'
#' @seealso [AR()],
#' [N()], [RW()], [RW2()], [Known()].
#' The values for `min` and `max` are based on the
#' defaults for function `forecast::ets()`.
#'
#' @references TMB documentation of
#' [AR1](http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.html#details)
#'
#' @examples
#' AR1()
#' AR1(min = 0, max = 1, s = 2.4)
#' @export
AR1 <- function(min = 0.8, max = 0.98, s = 1) {
  check_min_max_ar(min = min, max = max)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  scale <- as.double(s)
  min <- as.double(min)
  max <- as.double(max)
  new_bage_prior_ar(n = 1L,
                    min = min,
                    max = max,
                    scale = scale,
                    nm = "AR1")
}


## HAS_TESTS
#' Compose a Prior for a Time Main Effect or Interaction
#'
#' Create a composite prior for a time main effect,
#' or for an interaction involving time.
#' The composite prior always contains a trend,
#' and may contain a cyclical effect,
#' a seasonal effect, and an error.
#'
#'
#' | Term       | Main effects                     | Interactions      |
#' |------------|----------------------------------|-------------------|
#' | `trend`    | [Lin()], [RW()], [RW2()], [Sp()] | [ELin()]          |
#' | `seasonal` | [Seas()]                         | [ESeas()]         |
#' | `cyclical` | [AR()], [AR1()]                  | [EAR()], [EAR1()] |
#' | `error`    | [N()]                            | [N()]             |
#' 
#'
#' @param trend Prior describing the long-run behavior
#' of the series. See below for choices. Required.
#' @param cyclical Prior describing departures from the
#' long-run trend that do not have a fixed
#' period. See below for choices. Optional.
#' @param seasonal Prior describing departures from the
#' long-run trend that have a fixed period.
#' See below for choices. Optional.
#' @param error Prior for additional idiosyncratic variation.
#' See below for choices. Optional
#'
#' @returns An object of class `"bage_prior_compose"`
#'
#' @examples
#' compose_time(
#'   trend = Lin(),
#'   cyclical = AR()
#' )
#'
#' compose_time(
#'   trend = RW2(),
#'   error = N()
#' )
#'
#' compose_time(
#'   trend = ELin(),
#'   cyclical = EAR(),
#'   season = ESeas(n = 4)
#' )
#' @export
compose_time <- function(trend, cyclical = NULL, seasonal = NULL, error = NULL) {
  if (!inherits(trend, "bage_prior"))
    cli::cli_abort("{.arg trend} has class {.cls {class(trend)}}.")
  if (!use_for_compose_trend(trend))
    cli::cli_abort("{.var {str_call_prior(trend)}} prior cannot be used for {.arg trend}.")
  priors <- list(trend = trend)
  for (nm in c("cyclical", "seasonal", "error")) {
    val <- get(nm)
    if (!is.null(val)) {
      if (!inherits(val, "bage_prior"))
        cli::cli_abort("{.arg {nm}} has class {.cls {class(val)}}.")
      use_for_compose <- get(paste0("use_for_compose_", nm))
      if (!use_for_compose(val))
        cli::cli_abort("{.var {str_call_prior(val)}} prior cannot be used for {.arg {nm}}.")
      check_main_effect_interaction(x1 = trend,
                                    x2 = val,
                                    nm1 = "trend",
                                    nm2 = nm)
      val <- list(val)
      names(val) <- nm
      priors <- c(priors, val)
    }
  }
  if (length(priors) == 1L)
    cli::cli_abort(c("Not enough priors specified.",
                     i = "No values supplied for {.arg cyclical}, {.arg seasonal}, or {.arg error}."))
  along <- make_compose_along(priors)
  new_bage_prior_compose(priors = priors,
                         along = along,
                         nm = "compose_time")
}
  

## HAS_TESTS
#' Exchangeable Autoregressive Prior
#'
#' Prior for an interaction,
#' where an autoregression model
#' of order `n` is applied
#' to the "along" variable, within each
#' combination of values of the "by" variable.
#' The damping coefficients are shared across
#' different combinations of the "by" variables.
#' The time series within each combination of the
#' 'by' variables are, however, treated as exchangeable.
#' 
#' @section 'Along' and 'by' variables:
#'
#' Multivariate priors for interactions distinguish
#' between 'along' and 'by' variables. The 'along'
#' variable is typically time, or, in interactions
#' not involving time, is typically age. The 'by'
#' variables are everything else. In an interaction
#' between region, sex, and time, for instance,
#' the by variables are likely to be region and sex.
#'
#' If no `along` argument is supplied, then:
#'
#' - if the interaction contains a time variable, then
#'   it is assumed to be the 'along' variable;
#' - otherwise, if the interaction contains an
#'   age variable, then it is assumed to be
#'   the 'along' variable;
#' - otherwise, an error is raised.
#' 
#' @section Mathematical description:
#'
#' The model is
#'
#' \deqn{x_{u,v} = \phi_1 x_{u,i-1} + \cdots + \phi_k x_{u,i-k} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{x_{u,v}} has
#' marginal variance \eqn{\sigma^2}. The value of
#' \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' The \eqn{\phi_j} are constrained to the interval
#' between -1 and 1.
#' 
#' @param n The order of the model.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#' @param along Name of one of the dimensions
#' in the interaction. Optional, provided
#' the data contain a time or age dimension.
#'
#' @returns An object of class `bage_prior_ear`.
#'
#' @seealso [N()], [RW()], [RW2()], [Known()].
#' The values for `min` and `max` are based on the
#' defaults for function `forecast::ets()`.
#' [AR1()]
#'
#' @references TMB documentation for
#' [ARk](http://kaskr.github.io/adcomp/classdensity_1_1ARk__t.html#details)
#'
#' @examples
#' EAR(n = 3)
#' EAR(n = 3, s = 2.4)
#' EAR(along = "cohort")
#' @export
EAR <- function(n = 2, s = 1, along = NULL) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, null_ok = FALSE)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  n <- as.integer(n)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  new_bage_prior_ear(n = n,
                     min = -1,
                     max = 1,
                     scale = scale,
                     along = along,
                     nm = "EAR")
}


## HAS_TESTS
#' Exchangeable AR1 Prior
#'
#' Autogressive prior for an interaction,
#' where an AR model order 1 is applied
#' to the "along" variable, within each
#' combination of values of the "by" variable.
#' The damping coefficient is shared across
#' different combinations of the "by" variables.
#' The series within each combination of the
#' 'by' variables are, however, treated as
#' exchangeable.
#'
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Mathematical description:
#'
#' The model is
#'
#' \deqn{x_{u,v} = \phi x_{u,i-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{x_{u,v}} has
#' marginal variance \eqn{\sigma^2}. The value of
#' \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' Correlation parameter \eqn{\phi} is constrained
#' to lie in the interval `(a, b)`,
#' where \eqn{a} = `min` and \eqn{b} = `max`.
#' The prior distribution is for \eqn{\phi} is
#' \deqn{\phi = (b - a) \phi' - a}
#' where
#' \deqn{\phi' \sim \text{beta}(2, 2)}.
#'
#' @inheritParams EAR
#' @param min,max Minimum and maximum values
#' for autocorrelation parameter (\eqn{\phi}).
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `bage_prior_ear`.
#'
#' @seealso [N()], [RW()], [RW2()], [Known()].
#' The values for `min` and `max` are based on the
#' defaults for function `forecast::ets()`.
#' [EAR()]
#'
#' @references TMB documentation for
#' [ARk](http://kaskr.github.io/adcomp/classdensity_1_1ARk__t.html#details)
#'
#' @examples
#' EAR1()
#' EAR1(min = 0, max = 1, s = 2.4)
#' EAR1(along = "cohort")
#' @export
EAR1 <- function(min = 0.8, max = 0.98, s = 1, along = NULL) {
  check_min_max_ar(min = min, max = max)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  min <- as.double(min)
  max <- as.double(max)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  new_bage_prior_ear(n = 1L,
                     min = min,
                     max = max,
                     scale = scale,
                     along = along,
                     nm = "EAR1")
}

## 'bage_prior_elin' only ever created via 'set_prior()'

## HAS_TESTS
#' Exchangeable Linear Prior
#'
#' Prior for an interaction where, within each combination
#' of the 'by' variables, the elements of the 'along'
#' variable follow (approximately) a straight line.
#' The slopes of the line vary across
#' different combinations of 'by' variables'.
#'
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Statistical model:
#' 
#' The model is
#' 
#' \deqn{x_{uv} \sim \text{N}(\text{slope}_u q_v, \sigma^2)}
#'
#' where
#' - \eqn{u} is the index for a combination of 'by' variables,
#' - \eqn{v} is the index of the 'along' variable,
#' - \eqn{q_j} is a rescaled version of \eqn{j}, with mean 0,
#' minimum -1, and maximum 1: \eqn{q_j = - (J+1)/(J-1) + 2j/(J-1)}.
#'
#' The slopes are drawn from a common distribution with mean
#' \eqn{\text{slope}}.
#'
#' \deqn{\text{slope}_u \sim \text{N}(\text{slope}, \omega^2)}
#'
#' Larger absolute values for \eqn{\text{slope}} imply
#' steeper lines. The absolute value of \eqn{slope}
#' is governed by parameter `s`:
#'
#' \deqn{\text{slope} ~ \text{N}(0, \text{s}^2)}.
#'
#' Larger values for \eqn{\omega} imply more variability
#' in slopes across different combinations of the 'by'
#' variables. The size of \eqn{\omega} is governed by
#' parameter `ms`
#'
#' \deqn{\omega \sim \text{N}^+(0, \text{ms}^2)}
#' 
#' Larger values for \eqn{\sigma} imply more variability
#' around each line. The size of \eqn{\sigma} is
#' governed by parameter `s`:
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^} denotes a half-normal distribution,
#' which has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' @inheritParams EAR
#' @param sd A postive number. Default is 1.
#' @param ms A positive number. Default is 1.
#'
#' @returns An object of class `bage_prior_elin`.
#'
#' @seealso
#' - [N()] etc
#' - [set_var_time()] to specify the time variable
#' - [set_var_age()] to specify the age variable
#'
#' @examples
#' ELin()
#' ELin(s = 0.5, sd = 2, ms = 0.1, along = "cohort")
#' @export
ELin <- function(s = 1, sd = 1, ms = 1, along = NULL) {
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  check_scale(sd, x_arg = "sd", zero_ok = FALSE)
  check_scale(ms, x_arg = "ms", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  scale <- as.double(s)
  sd_slope <- as.double(sd)
  mscale <- as.double(ms)
  new_bage_prior_elin(scale = scale,
                      sd_slope = sd_slope,
                      mscale = mscale,
                      along = along)
}


## 'bage_prior_rw' can be created during initial call to mod_* function TODO - IMPLEMENT THIS

## HAS_TESTS
#' Exchangeable Random Walk Prior
#'
#' Prior for an interaction,
#' where a (first order) random walk model
#' is applied to the "along" variable, within each
#' combination of values of the "by" variable.
#' Standard deviations are shared across
#' different combinations of the "by" variables.
#' The series within each combination of the
#' 'by' variables are, however, treated as exchangeable.
#' 
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Mathematical description:
#'
#' The model is
#'
#' \deqn{x_{u,v} = x_{u,i-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \sigma^2)}
#'
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' @inheritParams EAR
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#'
#' @returns An object of class `bage_prior_erw`.
#'
#' @seealso [N()], [RW()], [RW2()], [Known()].
#'
#' @examples
#' ERW()
#' ERW(s = 0.5)
#' @export
ERW <- function(s = 1, along = NULL) {
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  new_bage_prior_erw(scale = scale,
                     along = along)
}


## 'bage_prior_eseas' only ever created via 'set_prior()'

## HAS_TESTS
#' Exchangeable Seasonal Prior
#'
#' Prior for a seasonal effect, within an interaction.
#' Each combination of the 'by' variables,
#' has a separate seasonal effect.
#' The `along` variable is almost always
#' time.
#'
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Statistical model:
#' 
#' The model is
#' 
#' \deqn{x_{uv} \sim \text{N}(x_{u,v-n}, \sigma^2)}
#'
#' where
#' - \eqn{u} is the index for a combination of 'by' variables,
#' - \eqn{v} is the index of the 'along' variable,
#' - \eqn{n} is the number of seasons
#'
#' Larger values for \eqn{\sigma} imply more variability
#' in seasonal effects over time. The size of \eqn{\sigma} is
#' governed by parameter `s`:
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^} denotes a half-normal distribution,
#' which has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' @inheritParams EAR
#' @param n Number of seasons.
#'
#' @returns An object of class `bage_prior_eseas`.
#'
#' @seealso
#' - [Seas()] etc
#' - [set_var_time()] to specify the time variable
#' - [set_var_age()] to specify the age variable
#'
#' @examples
#' ESeas(n = 4)
#' ESeas(n = 12, s = 0.5, along = "cohort")
#' @export
ESeas <- function(n, s = 1, along = NULL) {
  check_n(n = n, nm_n = "n", min = 2L, max = NULL, null_ok = FALSE)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  n <- as.integer(n)
  scale <- as.double(s)
  new_bage_prior_eseas(n = n,
                       scale = scale,
                       along = along)
}


## 'bage_prior_known' only ever created via 'set_prior()'

## HAS_TESTS
#' Treat an Intercept, Main Effect, or Interaction as Known
#'
#' Treat the intercept, a main effect, or an interaction
#' in an model as fixed and known.
#'
#' @param values A numeric vector
#'
#' @returns An object of class `bage_prior_known`.
#'
#' @seealso `Known` is usually called within [set_prior()].
#'
#' @examples
#' Known(-2.3)
#' Known(c(0.1, 2, -0.11))
#'
#' @export
Known <- function(values) {
  check_numeric(x = values, nm_x = values)
  values <- as.double(values)
  new_bage_prior_known(values = values)
}


## 'bage_prior_lin' only ever created via 'set_prior()'

## HAS_TESTS
#' Linear Prior
#'
#' Prior for a main effect where the elements
#' follow (approximately) a straight line.
#'
#' The model is
#'
#' \deqn{x_j \sim \text{N}(\text{slope} q_j, \sigma^2)}
#'
#' where \eqn{q_j} is a rescaled version of \eqn{j}, with mean 0,
#' minimum -1, and maximum 1: \eqn{q_j = - (J+1)/(J-1) + 2j/(J-1)}.
#'
#' The slope term is drawn from a normal distribution,
#'
#' \deqn{\text{slope} ~ \text{N}(0, \text{sd}^2)}.
#' 
#' The standard deviation for the normal distribution,
#' `sd`, defaults to 1, but can be set to other values.
#' Lower values for `sd` lead to smaller values for
#' the slope term.
#'
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^} denotes a half-normal distribution,
#' which has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' @param s A positive number. Default is 1.
#' @param sd A postive number. Default is 1.
#'
#' @returns An object of class `bage_prior_lin`.
#'
#' @seealso
#' - [N()] etc
#'
#' @examples
#' Lin()
#' Lin(s = 0.5, sd = 2)
#' @export
Lin <- function(s = 1, sd = 1) {
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  check_scale(sd, x_arg = "sd", zero_ok = FALSE)
  scale <- as.double(s)
  sd_slope <- as.double(sd)
  new_bage_prior_lin(scale = scale,
                     sd_slope = sd_slope)
}


## 'bage_prior_norm' can be created during initial call to mod_* function

## HAS_TESTS
#' Normal prior
#'
#' Prior in which units are drawn independently from a normal
#' distribution. The default prior for most terms.
#'
#' The normal distribution has mean \eqn{0} and standard
#' deviation \eqn{\sigma}.
#' 
#' \deqn{x \sim \text{N}(0, \sigma^2)}
#'
#' Standard deviation \eqn{\sigma} is drawn from a half-normal
#' distribution,
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution defaults
#' to 1, but can be set to other values. Lower values
#' lead to more tightly concentrated
#' estimates for `x`, and higher values lead to less tightly
#' concentrated estimates.
#'
#' @param s A positive, finite number.
#'
#' @returns An object of class `bage_prior_norm`.
#'
#' @seealso `N()` is usually called within [set_prior()].
#' Other priors are [RW()], [RW2()].
#'
#' @examples
#' N()
#' N(s = 0.5)
#' @export
N <- function(s = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE) 
    scale <- as.double(s)
    new_bage_prior_norm(scale = scale)
}


## 'bage_prior_normfixed' priors can be created during intial call to mod_* function

#' Normal prior with fixed standard deviation
#'
#' Normal prior where, in contrast to [N()], the
#' standard deviation is treated as fixed and known.
#'
#' The distribution has mean equal to `0` and
#' standard deviation `sd`, where `sd` is supplied
#' by the user.
#'
#' \deqn{x \sim \text{N}(0, \text{sd}^2)}
#'
#' `NFix()` is the default prior for the intercept.
#'
#' @param sd Standard deviation.
#' A positive, finite number.
#' Default is `1`.
#'
#' @returns An object of class `bage_prior_normfixed`.
#' `NFix()` is usually called within [set_prior()].
#' Other priors are [N()], [RW()], [RW2()].
#'
#' @examples
#' NFix()
#' NFix(sd = 10) ## prior used for intercept
#' @export
NFix <- function(sd = 1) {
    check_scale(sd, x_arg = "sd", zero_ok = FALSE) 
    sd <- as.double(sd)
    new_bage_prior_normfixed(sd = sd)
}

## 'bage_prior_rw' can be created during initial call to mod_* function

## HAS_TESTS
#' Random walk prior
#'
#' Prior in which units follow a (first order)
#' random walk. Increments between neighbouring
#' `x`s are assumed to be normally distibuted,
#'
#' \deqn{x_1 \sim \text{N}(0, 1)}
#' \deqn{x_i - x_{i-1} \sim \text{N}(0, \sigma^2), i = 2, \cdots, n}
#'
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' @param s A positive, finite number. Default is 1.
#'
#' @returns An object of class `bage_prior_rw`.
#'
#' @seealso
#' - [RW2()] for a random walk with drift
#' (a second-order random walk).
#' - `RW()` is usually called as part of
#' a call to [set_prior()]
#'
#' @examples
#' RW()
#' RW(s = 0.5)
#' @export
RW <- function(s = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    new_bage_prior_rw(scale = scale)
}


## 'bage_prior_rw2' only ever created by call to 'set_prior' function

## HAS_TESTS
#' Random walk with drift prior
#'
#' Prior in which units follow a random walk with drift.
#' Second-order differences are normally distributed,
#' 
#' \deqn{x_1 \sim \text{N}(0, 1)}
#' \deqn{x_2 \sim \text{N}(x_1, sd^2)}
#' \deqn{(x_i - x_{i-1}) - (x_{i-1} - x_{i-2}) \sim \text{N}(0, \sigma^2),
#'       i = 3, \cdots, n}
#'
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#' 
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' Parameter `sd` governs the expected size of
#' increments between neighbouring units.
#' It defaults to 1, but can be set to other values.
#'
#' @param s A positive, finite number. Default is 1.
#' @param sd A positive, finite number. Default is 1.
#'
#' @returns An object of class `bage_prior_rw`
#' or `bage_prior_rw2`.
#'
#' @seealso
#' - [RW()] for a first-order random walk.
#' - `RW2()` is usually called as part of
#' a call to [set_prior()]
#'
#' @examples
#' RW2()
#' RW2(s = 0.2)
#' @export
RW2 <- function(s = 1, sd = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    check_scale(sd, x_arg = "sd", zero_ok = FALSE)
    scale <- as.double(s)
    sd_slope <- as.double(sd)
    new_bage_prior_rw2(scale = scale,
                       sd_slope = sd_slope)
}



## HAS_TESTS
#' Prior for Seasonal Effect
#'
#' Prior for seasonal effect, where the size of
#' the effects evolve over time. Each season follows its
#' own random walk.
#'
#' @section Mathematical description:
#'
#' \deqn{x_j \sim \text{N}(0, 1), j = 1, \cdots, \text{n}}
#' \deqn{x_j - x_{j-n} \sim \text{N}(x_{j - n}, \sigma^2), j = n+1, \cdots, J}
#'
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series.
#'
#' @param n The number of seasons.
#' @param s A positive, finite number. Default is 1.
#'
#' @returns An object of class `"bage_prior_seas"`.
#'
#' @seealso
#' - [RW()] for an ordinary random walk.
#'
#' @examples
#' Seas(n = 4)
#' Seas(n = 12, s = 0.5)
#' @export
Seas <- function(n, s = 1) {
  check_n(n = n, nm_n = "n", min = 2L, max = NULL, null_ok = FALSE)
  check_scale(s, x_arg = "s", zero_ok = FALSE)
  n <- as.integer(n)
  scale <- as.double(s)
  new_bage_prior_seas(n = n,
                      scale = scale)
}


## 'bage_prior_spline' only ever created by call to 'set_prior' function

## HAS_TESTS
#' P-spline prior
#'
#' Specify a P-spline (penalised spline) prior.
#' A P-spline is flexible, but
#' favours profiles that are relatively smooth.
#'
#' @section Mathematical details:
#'
#' The model for the effect, on the log scale, is
#'
#' \deqn{\beta = X \gamma}
#'
#' where
#' - \eqn{\beta} is a main effect,
#' - \eqn{X} is a matrix holding the basis functions
#' for the spline, with `n` columns, and
#' - \eqn{\alpha} is a vector of coefficients,
#' with `n` elements.
#'
#' The elements of \eqn{\gamma} are assumed to follow
#' a second order random walk (see [RW2()]).
#'
#' @inheritParams RW2
#' @param n Number of spline vectors.
#' By default is `NULL`, in which case the number of
#' vectors is set to `max(ceiling(0.7 * k), 4)`
#' where `k` is the number
#' of elements in the term being modelled.
#'
#' @returns An object of class `"bage_prior_spline"`.
#'
#' @seealso
#' - [RW2()] etc
#'
#' @examples
#' Sp()
#' Sp(n = 10)
#' @export
Sp <- function(n = NULL, s = 1, sd = 1) {
    check_n(n,
            nm_n = "n",
            min = 4L,
            max = NULL,
            null_ok = TRUE)
    if (!is.null(n))
        n <- as.integer(n)
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    check_scale(sd, x_arg = "sd", zero_ok = FALSE)
    scale <- as.double(s)
    sd_slope <- as.double(sd)
    new_bage_prior_spline(n = n,
                          scale = scale,
                          sd_slope = sd_slope)
}


## 'bage_prior_svd' only ever created by call to 'set_prior' function

## HAS_TESTS
#' SVD prior
#'
#' Specify a scaled SVD (singular value decomposition)
#' prior for an age variable, or a combination
#' of age and sex/gender variables.
#'
#' @param scaled_svd An object created by [scaled_svd()]
#' holding scaled values from a SVD of
#' age-specific rates, probabilities, or means.
#' @param n Number of components from scaled SVD
#' to use in modelling. Must be between 1 and 10.
#' Default is 10.
#' @param indep Whether, in an interaction,
#' age profiles for different
#' sexes/genders are modelled independently.
#' See description in Details. Default is `TRUE`.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @export
SVD <- function(scaled_svd, n = 5, indep = TRUE) {
    nm_scaled_svd <- deparse1(substitute(scaled_svd))
    if (!inherits(scaled_svd, "bage_scaled_svd"))
        cli::cli_abort(c("{.arg scaled_svd} does not hold scaled SVD values.",
                         i = "{.arg scaled_svd} has class {.cls {class(scaled_svd)}}.",
                         i = "{.arg scaled_svd} should have class {.cls bage_scaled_svd}."))
    check_n(n,
            nm_n = "n",
            min = 1L,
            max = 10L,
            null_ok = FALSE)
    n <- as.integer(n)
    check_flag(indep)
    new_bage_prior_svd(scaled_svd = scaled_svd,
                       nm_scaled_svd = nm_scaled_svd,
                       n = n,
                       indep = indep)
}


## Internal constructors ------------------------------------------------------

## Assume that inputs are all correct.
## (Checking is done by user-visible functions.)

## 'i_prior' is the index number for the prior. 
## It is *very* important that this be 
## consistent with value used by function
## 'logpost' in src/bage.cpp
##
## 'const' is a vector of doubles holding constants
## used in calculation of log-posterior.
## We cannot pass a zero-length vector to TMB,
## so when there are no constants, we use single 0.
##
## 'specific' is a general list of objects
## contained in this prior


## HAS_TESTS
new_bage_prior_ar <- function(n, scale, min, max, nm) {
  shape1 <- 2.0
  shape2 <- 2.0
  ans <- list(i_prior = 5L,
              const = c(shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale),
              specific = list(n = n,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              scale = scale,
                              nm = nm))
  class(ans) <- c("bage_prior_ar", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_compose <- function(priors, along, nm) {
  ans <- list(i_prior = 1000L,
              const = 0L, ## not used
              specific = list(priors = priors,
                              along = along,
                              nm = nm))
  class(ans) <- c("bage_prior_compose", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_ear <- function(n, scale, min, max, nm, along) {
  shape1 <- 2.0
  shape2 <- 2.0
  ans <- list(i_prior = 12L,
              const = c(shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale),
              specific = list(n = n,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              scale = scale,
                              along = along,
                              nm = nm))
  class(ans) <- c("bage_prior_ear", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_elin <- function(scale, sd_slope, mscale, along) {
    ans <- list(i_prior = 9L,
                const = c(scale = scale,
                          sd_slope = sd_slope,
                          mscale = mscale),
                specific = list(scale = scale,
                                sd_slope = sd_slope,
                                mscale = mscale,
                                along = along))
    class(ans) <- c("bage_prior_elin", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_erw <- function(scale, along) {
    ans <- list(i_prior = 13L,
                const = c(scale = scale),
                specific = list(scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_erw", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_eseas <- function(n, scale, along) {
    ans <- list(i_prior = 11L,
                const = c(scale = scale,
                          rep(c("<unused>" = 0), times = n - 1L)),
                specific = list(n = n,
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_eseas", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_known <- function(values) {
    ans <- list(i_prior = 0L,
                const = 0, ## not used
                specific = list(values = values))
    class(ans) <- c("bage_prior_known", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_lin <- function(scale, sd_slope) {
    ans <- list(i_prior = 8L,
                const = c(scale = scale,
                          sd_slope = sd_slope),
                specific = list(scale = scale,
                                sd_slope = sd_slope))
    class(ans) <- c("bage_prior_lin", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_norm <- function(scale) {
    ans <- list(i_prior = 1L,
                const = c(scale = scale),
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_normfixed <- function(sd) {
    ans <- list(i_prior = 2L,
                const = c(sd = sd),
                specific = list(sd = sd))
    class(ans) <- c("bage_prior_normfixed", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw <- function(scale) {
    ans <- list(i_prior = 3L,
                const = c(scale = scale),
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2 <- function(scale, sd_slope) {
    ans <- list(i_prior = 4L,
                const = c(scale = scale,
                          sd_slope = sd_slope),
                specific = list(scale = scale,
                                sd_slope = sd_slope))
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_seas <- function(n, scale) {
    ans <- list(i_prior = 10L,
                const = c(scale = scale),
                specific = list(n = n,
                                scale = scale))
    class(ans) <- c("bage_prior_seas", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_spline <- function(n, scale, sd_slope) {
    ans <- list(i_prior = 6L,
                const = c(scale = scale,
                          sd_slope = sd_slope),
                specific = list(n = n,
                                scale = scale,
                                sd_slope = sd_slope))
    class(ans) <- c("bage_prior_spline", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_svd <- function(scaled_svd, nm_scaled_svd, n, indep) {
    ans <- list(i_prior = 7L,
                const = 0, ## not used
                specific = list(scaled_svd = scaled_svd,
                                nm_scaled_svd = nm_scaled_svd,
                                n = n,
                                indep = indep))
    class(ans) <- c("bage_prior_svd", "bage_prior")
    ans
}
