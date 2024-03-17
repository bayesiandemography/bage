
## User-visible constructors --------------------------------------------------

## 'bage_prior_ar' only ever created via 'set_prior()'

## HAS_TESTS
#' Autoregressive Prior
#'
#' Autoregressive prior with order `k`. Used to model main effects:
#' typically time main effects.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of the main effect, then
#'
#' \deqn{\beta_j = \phi_1 \beta_{j-1} + \cdots + \phi_k \beta_{j-k} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{\beta_j} has
#' marginal variance \eqn{\sigma^2}. The value of
#' \eqn{\tau} has prior
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' The \eqn{\phi_1, \cdots, \phi_k} are restricted to values
#' in the interval \eqn{(-1,1)} that yield stationary series.
#' 
#' @param n The order of the model.
#' Default is `2`.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\tau}).
#' Default is `1`.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR1()] Special case of `AR()`, though with
#'   more options for damping.
#' - [EAR()] Exchangeable version of `AR()`,
#'   used with interactions.
#' - [priors] Overview of priors implemented in `bage`.
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
#' AR1 Prior
#'
#' Autoregressive prior of order 1
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of the main effect, then

#' \deqn{\beta_0 \sim \text{N}(0, \sigma^2)}
#' \deqn{\beta_j = \phi \beta_{j-1} + \sqrt{1 - \phi^2}\epsilon_j}
#' \deqn{\epsilon \sim \text{N}(0, \tau^2)}
#'
#' \eqn{\tau} is drawn from a half-normal distribition
#' with scale set by the `s` argument.
#'
#' Correlation parameter \eqn{\phi} is constrained
#' to lie in the interval `(\text{min}, \text{max})`.
#' The prior distribution for \eqn{\phi} is
#' 
#' \deqn{\phi = (\text{max} - \text{min}) \phi' - \text{min}}
#' 
#' where
#' 
#' \deqn{\phi' \sim \text{beta}(2, 2)}.
#'
#' @param min,max Minimum and maximum values
#' for autocorrelation parameter (\eqn{\phi}).
#' Defaults are `0.8` and `0.98`.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\tau}).
#' Default is `1`.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR()] General case of `AR()`.
#' - [EAR1()] Exchangeable version of `AR1()`,
#'   used with interactions.
#' - [priors] Overview of priors implemented in `bage`.
#' - The values for `min` and `max` are based on the
#'   defaults for function `forecast::ets()`.
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
#' An exchangeable autoregressive prior
#' is used with interactions. An autoregressive
#' model of order \eqn{k} is applied to the 'along' 
#' variable, within each combination of values of the 'by' variables.
#' The damping coefficients are shared across
#' different combinations of the 'by' variables.
#' The series within each combination of the
#' 'by' variables are treated as exchangeable.
#' 
#' @section 'Along' and 'by' variables:
#'
#' Some priors for interactions distinguish
#' between 'along' and 'by' variables. The 'along'
#' variable is typically time, or, in interactions
#' not involving time, is typically age. The 'by'
#' variables are everything else. In an interaction
#' between region, sex, and time, for instance,
#' the 'along' variable is likely to be time,
#' and the 'by' variables are likely to be region and sex.
#'
#' If no `along` argument is supplied, a default value
#' is chosen as follows:
#'
#' - if the interaction contains a time variable, then
#'   the 'along' variable is assumed to be time;
#' - otherwise, if the interaction contains an
#'   age variable, then the 'along' variable
#'   is assumed to be age;
#' - otherwise, an error is raised.
#' 
#' @section Mathematical details:
#'
#' If \eqn{\beta_{u,v}} is the \eqn{v}th element of the interaction
#' within the \eqn{u}th combination of the
#' 'by' variables, then
#'
#' \deqn{x_{u,v} = \phi_1 x_{u,v-1} + \cdots + \phi_k x_{u,v-k} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{x_{u,v}} has
#' marginal variance \eqn{\tau^2}. The value of
#' \eqn{\tau} has prior
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' The \eqn{\phi_1, \cdots, \phi_k} are restricted to values within
#' the interval \eqn{(-1, 1)} that yield stationary series.
#' 
#' @param n The order of the model.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\tau}).
#' Defaults to 1.
#' @param along Name of one of the dimensions
#' in the interaction. Optional, provided
#' the data contain a time or age dimension.
#'
#' @returns An object of class `"bage_prior_ear"`.
#'
#' @seealso
#' - [EAR1()] Special case of `EAR()`, though with
#'   more options for damping.
#' - [AR()] Autoregressive prior for main effects.
#' - [priors] Overview of priors implemented in `bage`.
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
#' An exchangeable AR1 prior
#' is used with interactions. An autoregressive
#' model of order \eqn{k} is applied to the 'along' 
#' variable, within each combination of values of the 'by' variables.
#' The damping coefficients are shared across
#' different combinations of the 'by' variables.
#' The series within each combination of the
#' 'by' variables are treated as exchangeable.
#' 
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Mathematical description:
#'
#' If \eqn{\beta_{u,v}} is the \eqn{v}th element of the interaction
#' within the \eqn{u}th combination of the
#' 'by' variables, then
#'
#' \deqn{x_{u,v} = \phi x_{u,v-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2)}
#'
#' where \eqn{\omega} is chosen so that each \eqn{x_{u,v}} has
#' marginal variance \eqn{\tau^2}. The value of
#' \eqn{\tau} has prior
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' Correlation parameter \eqn{\phi} is constrained
#' to lie in the interval `(\text{min}, \text{max})`.
#' The prior distribution for \eqn{\phi} is
#' 
#' \deqn{\phi = (\text{max} - \text{min}) \phi' - \text{min}}
#'
#' where
#' 
#' \deqn{\phi' \sim \text{beta}(2, 2)}.
#'
#' @inheritParams EAR
#' @param min,max Minimum and maximum values
#' for autocorrelation parameter (\eqn{\phi}).
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_ear"`.
#'
#' @seealso
#' - [EAR()] More general exchangeable AR model.
#' - [AR1()] AR1 prior for main effects.
#' - [priors] Overview of priors implemented in `bage`.
#' - The values for `min` and `max` are based on the
#'   defaults for function `forecast::ets()`.
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
#' variable follow a straight line, with idiosyncratic
#' errors around that line.
#' Each combination of the 'by' variables has a different
#' line, with a different slope. The slopes are drawn
#' from a common distribution.
#'
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Mathematical details:
#' 
#' If \eqn{\beta_{u,v}} is the \eqn{v}th element of the interaction
#' within the \eqn{u}th combination of the
#' 'by' variables, then
#' 
#' \deqn{\beta_{u,v} \sim \text{N}(\eta_u q_v, \tau^2)}
#'
#' where
#'
#' \deqn{q_v = - (V+1)/(V-1) + 2v/(V-1).}
#'
#' The slopes \eqn{\eta_u} are drawn from a common distribution,
#'
#' \deqn{\eta_u \sim \text{N}(\eta, \omega^2).}
#'
#' The absolute value of mean \eqn{\eta}
#' is governed by parameter `s`:
#'
#' \deqn{\eta \sim \text{N}(0, \text{s}^2).}
#'
#' Larger values for \eqn{\omega} imply more variability
#' in slopes across different combinations of the 'by'
#' variables. The size of \eqn{\omega} is governed by
#' parameter `ms`,
#'
#' \deqn{\omega \sim \text{N}^+(0, (\text{ms})^2)}
#' 
#' Larger values for \eqn{\tau} imply more variability
#' around each line. The size of \eqn{\tau} is
#' governed by parameter `s`:
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^+} denotes a half-normal distribution,
#' which has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' @inheritParams EAR
#' @param sd A postive number. Default is 1.
#' @param ms A positive number. Default is 1.
#'
#' @returns An object of class `"bage_prior_elin"`.
#'
#' @seealso
#' - [Lin()] Linear prior for main effects.
#' - [priors] Overview of priors implemented in `bage`.
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
#' is applied to the 'along' variable, within each
#' combination of values of the 'by' variables.
#' Standard deviations are shared across
#' different combinations of the 'by' variables.
#' The series within each combination of the
#' 'by' variables are treated as exchangeable.
#' 
#' @inheritSection EAR 'Along' and 'by' variables
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_{u,v}} is the \eqn{v}th element of the interaction
#' within the \eqn{u}th combination of the
#' 'by' variables, then
#'
#' \deqn{x_{u,v} = x_{u,v-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2)}
#'
#' Standard deviation \eqn{\tau} is drawn from a
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
#' for `s` lead to smoother series.
#'
#' @inheritParams EAR
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#'
#' @returns An object of class `bage_prior_erw`.
#'
#' @seealso
#' - [RW()] Random walk prior for main effects.
#' - [priors] Overview of priors implemented in `bage`.
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
#' @section Mathematical details:
#' 
#' If \eqn{\beta_{u,v}^{\text{seas}}} is the \eqn{v}th
#' element of the seasonal effect within
#' within the \eqn{u}th combination of the
#' 'by' variables, then
#' 
#' \deqn{\beta_{u,v}^{\text{seas}} \sim \text{N}(\beta_{u,v-n}^{\text{seas}}, \tau^2),}
#'
#' where \eqn{n} is the number of seasons.
#'
#' Larger values for \eqn{\tau} imply more variability
#' in seasonal effects over time. The size of \eqn{\tau} is
#' governed by parameter `s`:
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^+} denotes a half-normal distribution,
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
#' - [Seas()] Seasonal effect for main effects.
#' - Seasonal effects are always
#'   used within 'composite' priors, created using
#'   [compose_time()].
#' - [priors] Overview of priors implemented in `bage`.
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
#' @seealso
#' - [NFix()] Prior where level unknown, but variability known.
#' - [priors] Overview of priors implemented in `bage`.
#'
#'
#' @examples
#' Known(-2.3)
#' Known(c(0.1, 2, -0.11))
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
#' follow a straight line, with idiosyncratic
#' errors around this line.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of the main effect, then
#'
#' \deqn{\beta_j \sim \text{N}(\eta q_j, \tau^2)}
#'
#' where
#'
#' \deqn{q_j = - (J+1)/(J-1) + 2j/(J-1).}
#'
#' The slope term is drawn from a normal distribution,
#'
#' \deqn{\eta ~ \text{N}(0, (\text{sd})^2)}.
#' 
#' The standard deviation for the normal distribution,
#' `sd`, defaults to 1, but can be set to other values.
#' Lower values for `sd` lead to smaller values for
#' the slope term.
#'
#' Standard deviation \eqn{\tau} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' (\eqn{\text{N}^+} denotes a half-normal distribution,
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
#' - [ELin()] Exchangeable version of `Lin()`,
#'   used with interactions.
#' - [priors] Overview of priors implemented in `bage`.
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
#' Normal Prior
#'
#' Prior in which units are drawn independently from a normal
#' distribution. The default prior for most terms.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of a main effect
#' or interaction, then
#'
#' \deqn{\beta_j \sim \text{N}(0, \tau^2).}
#'
#' Standard deviation \eqn{\tau} is drawn from a half-normal
#' distribution,
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution defaults
#' to 1, but can be set to other values. Lower values
#' lead to more tightly concentrated
#' estimates for `\beta_j`, and higher values lead to less tightly
#' concentrated estimates.
#'
#' @param s A positive, finite number.
#'
#' @returns An object of class `bage_prior_norm`.
#'
#' @seealso 
#' - [NFix()] Version of `N()` where the standard deviation
#'   term is supplied rather than estimated from the data.
#' - [priors] Overview of priors implemented in `bage`.
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

#' Normal Prior with Fixed Standard Deviation
#'
#' Normal prior where, in contrast to [N()], the
#' standard deviation is treated as fixed and known.
#'
#' `NFix()` is the default prior for the intercept.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of a main effect
#' or interaction, then
#'
#' \deqn{\beta_j \sim \text{N}(0, \text{sd}^2).}
#'
#' @param sd Standard deviation.
#' A positive, finite number.
#' Default is `1`.
#'
#' @returns An object of class `"bage_prior_normfixed"`.
#'
#' @seealso 
#' - [N()] Version of `NFix()` where the standard deviation
#'   term is estimated from the data.
#' - [priors] Overview of priors implemented in `bage`.
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
#' elements are assumed to be normally distibuted.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of a main effect, then
#'
#' \deqn{\beta_j - x_{j-1} \sim \text{N}(0, \tau^2).}
#'
#' Standard deviation \eqn{\tau} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
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
#' @returns An object of class `"bage_prior_rw"`.
#'
#' @seealso
#' - [RW2()] for a random walk with drift
#'   (a second-order random walk).
#' - [priors] Overview of priors implemented in `bage`.
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
#' Random Walk with Drift Prior
#'
#' Prior in which units follow a random walk with drift,
#' also known as a second-order random walk.
#' Second-order differences are normally distributed.
#'
#' @section Mathematical details:
#'
#' If \eqn{\beta_j} is the \eqn{j}th element of a main effect, then
#' 
#' \deqn{(\beta_j - \beta_{j-1}) - (\beta_{j-1} - x_{j-2}) \sim \text{N}(0, \tau^2)},
#'
#' Standard deviation \eqn{\tau} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `s` lead to smoother series .
#'
#' Parameter `sd` governs the expected size of
#' increments between neighbouring units.
#' It defaults to 1, but can be set to other values.
#'
#' @param s A positive, finite number. Default is 1.
#' @param sd A positive, finite number. Default is 1.
#'
#' @returns An object of class `"bage_prior_rw2"`.
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
#' @section Mathematical details:
#'
#' If \eqn{\beta_j^{\text{seas}}} is the \eqn{j}th element of a
#' seasonal effect, then
#'
#' \deqn{\beta_j^{\text{seas}} - \beta_{j-n}^{\text{seas}} \sim \text{N}(x_{j - n}, \tau^2).}
#'
#' Standard deviation \eqn{\tau} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2)}
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
#' - [priors] Overview of priors implemented in `bage`.
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
#' SVD Prior for Age Main Effect
#'
#' Specify a Singular Value Decomposition (SVD) prior
#' for an age main effect.
#'
#' An SVD prior assumes that the age profile for the quantity
#' being modelled looks like an age profile drawn at random
#' from an external demographic database. For instance,
#' ```
#' set_prior(mod, age ~ SVD(HMD))
#' ```
#' specifies that the age profile looks like it was drawn from
#' [Human Mortality Database](https://www.mortality.org).
#'
#' A [Singular Value Decomposition][bage::svd()] is closely related
#' to a Principal Components analysis, and is a way of
#' extracting patterns from large volumes of data.
#'
#' @section Mathematical details:
#' 
#' With an SVD prior,
#' 
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g}}
#'
#' where
#' 
#' - \eqn{\pmb{\beta}} is an age effect with \eqn{J} elements;
#' - \eqn{\pmb{X}} is a known \eqn{J \times K} matrix, where \eqn{J}
#'   is the number of age groups and \eqn{K} is the number of
#'   vectors from the SVD to use (with \eqn{K} typically much
#'   less than than \eqn{J});
#' - \eqn{\pmb{\alpha}} is a vector with \eqn{K} elements; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements.
#'
#' The elements of \eqn{\pmb{\alpha}} have prior
#' 
#' \deqn{\alpha_k \sim \text{N}(0, 1), \quad k = 1, \cdots, K.}
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by preforming an SVD and then standardizing.
#' For details see TODO - REFERENCE TO VIGNETTE.
#'
#' @section Scaled SVDs currently implemented in bage:
#'
#' - \code{\link{HMD}} Values from the
#' [Human Mortality Database](https://www.mortality.org).
#' - TODO - add others
#'
#' @param ssvd Object of class `"bage_ssvd"`
#' holding results from a scaled SVD.
#' See below for current options.
#' @param n Number of vectors from scaled SVD
#' to use in modelling. Must be between 1 and 10.
#' Default is 5.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @seealso
#' - [set_prior()] Specify the prior for a model
#' - [SVDS()] SVD prior for interaction between
#'   age and sex/gender.
#' - [ESVD()] Exchangeable SVD prior for interaction involving
#'   age and other dimensions, but not sex/gender.
#' - [ESVDS()] Exchangeable SVD prior for interaction involving
#'   age, sex/gender, and other dimension(s).
#'
#' @examples
#' SVD(HMD)
#' SVD(HMD, n = 3)
#' @export
SVD <- function(ssvd, n = 5) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_n(n,
          nm_n = "n",
          min = 1L,
          max = 10L,
          null_ok = FALSE)
  n <- as.integer(n)
  new_bage_prior_svd(ssvd = ssvd,
                     nm_ssvd = nm_ssvd,
                     n = n,
                     joint = NULL)
}


## HAS_TESTS
#' SVD Prior for Interaction Between Age and Sex/Gender
#'
#' Specify a Singular Value Decomposition (SVD) prior
#' for an interaction between age and sex/gender.
#' The age-sex profile is assumed to look like it
#' was drawn at random
#' from an external demographic database. For instance,
#' ```
#' set_prior(mod, age:sex ~ SVDS(HMD))
#' ```
#' specifies that age-sex profile looks like it was drawn from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' @section Joint or independent SVDs:
#'
#' To possible ways of extracting patterns
#' from age-sex-specific data are
#'
#' 1. carry out separate SVDs on separate datasets for
#'    each sex/gender; or
#' 1. carry out a single SVD on dataset that has separate
#'    entries for each sex/gender.
#'
#' Option 1 is more flexible. Option 2 is
#' more robust to sampling or measurement errors.
#' Option 1 is obtained by setting the `joint`
#' argument  to `FALSE`. Option 2
#' is obtained by setting the `joint` argument to `TRUE`.
#' The default is `FALSE`.
#'
#' @section Mathematical details:
#' 
#' Let \eqn{A} denote the number of age groups, and \eqn{S} the
#' number of sexes/genders. When `joint` is `FALSE`,
#' 
#' \deqn{\pmb{\beta}_s = \pmb{F}_s \pmb{\alpha}_s + \pmb{g}_s, \quad s = 1, \cdots S}
#'
#' where \eqn{\pmb{\beta}_s}, \eqn{\pmb{\alpha}_s},
#' and \eqn{\pmb{g}_s} each have \eqn{A}
#' elements, and \eqn{\pmb{F}} has \eqn{A} rows.
#'
#' When `joint` is `TRUE`,
#'
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g}}
#'
#' where
#' 
#' where \eqn{\pmb{\beta}}, \eqn{\pmb{\alpha}},
#' and \eqn{\pmb{g}} each have \eqn{A \times S}
#' elements, and \eqn{\pmb{F}_s} has \eqn{A \times S} rows.
#'
#' \eqn{\pmb{F}_s}, \eqn{\pmb{g}_s}, \eqn{\pmb{F}}, and \eqn{\pmb{g}},
#' are all estimated externally, using estimates from a
#' demographic database, and are treated as fixed and known.
#'
#' The elements of \eqn{\pmb{\alpha}_s} and \eqn{\pmb{\alpha}} have prior
#' 
#' \deqn{\alpha_k \sim \text{N}(0, 1)}
#' 
#' For a description of the construction of \eqn{\pmb{F}_s},
#' \eqn{\pmb{F}}, \eqn{\pmb{g}_s}, and \eqn{\pmb{g}},
#' see TODO - REFERENCE TO VIGNETTE.
#'
#' @inheritSection SVD Scaled SVDs currently implemented in bage
#'
#' @inheritParams SVD
#' @param joint Whether to use combined or
#' separate SVDs. Default is `FALSE`.
#' See below for details.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @seealso
#' - [set_prior()] Specify the prior for a model.
#' - [SVD()] SVD prior for age main effect.
#' - [ESVD()] Exchangeable SVD prior for interaction involving
#'   age and other dimensions, but not sex/gender.
#' - [ESVDS()] Exchangeable SVD prior for interaction involving
#'   age, sex/gender, and other dimension(s).
#'
#' @examples
#' SVDS(HMD)
#' SVDS(HMD, joint = TRUE)
#' @export
SVDS <- function(ssvd, n = 5, joint = FALSE) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_n(n,
          nm_n = "n",
          min = 1L,
          max = 10L,
          null_ok = FALSE)
  n <- as.integer(n)
  check_flag(joint)
  new_bage_prior_svd(ssvd = ssvd,
                     nm_ssvd = nm_ssvd,
                     n = n,
                     joint = joint)
}


## HAS_TESTS
#' Exchangeable SVD Prior for Interactions Involving Age
#'
#' Specify a Singular Value Decomposition (SVD) prior
#' for interactions involving age but not sex/gender.
#' Age profiles are "exchangeable" in that
#' profiles for all combinations of the non-age variables
#' are drawn from the same distribution.
#' The age profiles are assumed to look like
#' they were drawn at random
#' from an external demographic database. For instance,
#' ```
#' set_prior(mod, age ~ ESVD(HMD))
#' ```
#' specifies that the age profiles should look like they
#' were drawn from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' @section Mathematical details:
#' 
#' With an ESVD prior,
#' 
#' \deqn{\pmb{\beta}_u = \pmb{F} \pmb{\alpha}_u + \pmb{g}}
#'
#' where
#' 
#' - \eqn{\pmb{\beta}}_u is the age profile associated with
#'   combination \eqn{u} of the non-age variables;
#' - \eqn{\pmb{X}} is a known \eqn{V \times K} matrix, where
#'   \eqn{V} is the number of age groups, and \eqn{K} is much
#'   less than \eqn{V};
#' - \eqn{\pmb{\alpha}}_u is a vector with \eqn{V} elements; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{V} elements.
#'
#' The elements of \eqn{\pmb{\alpha}}_u have prior
#' 
#' \deqn{\alpha_{uk} \sim \text{N}(0, 1), \quad i = 1, \cdots, K}
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by preforming an SVD and then extracting means and variances.
#' For details see TODO - REFERENCE TO VIGNETTE.
#'
#' @inheritSection SVD Scaled SVDs currently implemented in bage
#'
#' @inheritParams SVD
#'
#' @returns An object of class `"bage_prior_esvd"`.
#'
#' @seealso
#' - [set_prior()] Specify the prior for a model
#' - [SVD()] SVD prior for age main effect.
#' - [SVDS()] SVD prior for interaction between
#'   age and sex/gender.
#' - [ESVDS()] Exchangeable SVD prior for interaction involving
#'   age, sex/gender, and other dimension(s).
#' - XXX Create a new scaled SVD.
#'
#' @examples
#' ESVD(HMD)
#' ESVD(HMD, n = 3)
#' @export
ESVD <- function(ssvd, n = 5) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_n(n,
          nm_n = "n",
          min = 1L,
          max = 10L,
          null_ok = FALSE)
  n <- as.integer(n)
  new_bage_prior_esvd(ssvd = ssvd,
                      nm_ssvd = nm_ssvd,
                      n = n,
                      joint = NULL)
}


## HAS_TESTS
#' Exchangeable SVD Prior for Interactions Involving Age
#' and Sex/Gender
#'
#' Specify a Singular Value Decomposition (SVD) prior
#' for an interaction involving age, sex/gender,
#' and at least one other dimension.
#' Age-sex profiles are "exchangeable", in that
#' profiles for all combinations of the 
#' non-age, non-sex variables are drawn
#' from the same distribution.
#' The age-sex profiles are assumed to look like they
#' were drawn at random
#' from an external demographic database. For instance,
#' ```
#' set_prior(mod, age:sex ~ ESVDS(HMD))
#' ```
#' specifies that age-sex profiles should look
#' like they was drawn from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' @inheritSection SVDS Joint or independent SVDs
#'
#' @section Mathematical details:
#' 
#' Let \eqn{V} denote the number of age groups, \eqn{S} the
#' number of sexes/genders, and \eqn{U} the number of combinations
#' of non-age, non-sex variables.
#' When `joint` is `FALSE`,
#' 
#' \deqn{\pmb{\beta}_{us} = \pmb{F}_s \pmb{\alpha}_{us} + \pmb{g}_s,
#'   \quad, u = 1, \cdots, U, s = 1, \cdots S}
#'
#' where \eqn{\pmb{\beta}_{us}}, \eqn{\pmb{\alpha}_{us}},
#' and \eqn{\pmb{g}_{us}} each have \eqn{V}
#' elements, and \eqn{\pmb{F}} has \eqn{V} rows.
#'
#' When `joint` is `TRUE`,
#'
#' \deqn{\pmb{\beta}_u = \pmb{F} \pmb{\alpha}_u + \pmb{g}}
#'
#' where
#' 
#' where \eqn{\pmb{\beta}_u}, \eqn{\pmb{\alpha}_u},
#' and \eqn{\pmb{g}} each have \eqn{V \times S}
#' elements, and \eqn{\pmb{F}} has \eqn{V \times S} rows.
#'
#' \eqn{\pmb{F}_s}, \eqn{\pmb{g}_s}, \eqn{\pmb{F}}, and \eqn{\pmb{g}},
#' are all estimated externally, using estimates from a
#' demographic database, and are treated as fixed and known.
#'
#'
#' The elements of \eqn{\pmb{\alpha}_{us}} and \eqn{\pmb{\alpha}_u} have prior
#' 
#' \deqn{\alpha_{uk} \sim \text{N}(0, 1)}
#' 
#' For a description of the construction of \eqn{\pmb{F}_s},
#' \eqn{\pmb{F}}, \eqn{\pmb{g}_s}, and \eqn{\pmb{g}},
#' see TODO - REFERENCE TO VIGNETTE.
#'
#' @inheritSection SVD Scaled SVDs currently implemented in bage
#'
#' @inheritParams SVD
#' @param joint Whether to use combined or
#' separate SVDs. Default is `FALSE`.
#' See below for details.
#'
#' @returns An object of class `"bage_prior_esvd"`.
#'
#' @seealso
#' - [set_prior()] Specify the prior for a model
#' - [SVD()] SVD prior for age main effect.
#' - [SVDS()] SVD prior for interaction between
#'   age and sex/gender.
#' - [ESVD()] Exchangeable SVD prior for interaction involving
#'   age and other dimensions, but not sex/gender.
#'
#' @examples
#' ESVDS(HMD)
#' ESVDS(HMD, joint = TRUE)
#' @export
ESVDS <- function(ssvd, n = 5, joint = FALSE) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_n(n,
          nm_n = "n",
          min = 1L,
          max = 10L,
          null_ok = FALSE)
  n <- as.integer(n)
  check_flag(joint)
  new_bage_prior_esvd(ssvd = ssvd,
                      nm_ssvd = nm_ssvd,
                      n = n,
                      joint = joint)
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
## Doesn't use 'along', because can potentially have two
## 'along' dimensions (age and sex/gender)
new_bage_prior_esvd <- function(ssvd, nm_ssvd, n, joint) {
    ans <- list(i_prior = 14L,
                const = 0, ## not used
                specific = list(ssvd = ssvd,
                                nm_ssvd = nm_ssvd,
                                n = n,
                                joint = joint))
    class(ans) <- c("bage_prior_esvd", "bage_prior")
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
new_bage_prior_svd <- function(ssvd, nm_ssvd, n, joint) {
    ans <- list(i_prior = 7L,
                const = 0, ## not used
                specific = list(ssvd = ssvd,
                                nm_ssvd = nm_ssvd,
                                n = n,
                                joint = joint))
    class(ans) <- c("bage_prior_svd", "bage_prior")
    ans
}
