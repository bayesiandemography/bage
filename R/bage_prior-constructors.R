
## User-visible constructors --------------------------------------------------

## HAS_TESTS
#' Autoregressive Prior
#'
#' Prior for main effects or interaction.
#' An autoreggressive process
#' with order `k`. Typically used with time.
#'
#' If `AR()` is used with an interaction,
#' then separate AR series are constructed along
#' the "along" variable within
#' each combination of the
#' "by" variables. 
#' 
#' Argument `s` controls the size of innovations. Smaller values
#' for `s` tend to give smoother series.
#'
#' @section Mathematical details:
#'
#' When `AR()` is used with a main effect,
#'
#' \deqn{\beta_j = \phi_1 \beta_{j-1} + \cdots + \phi_n \beta_{j-n} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \phi_1 \beta_{u,v-1} + \cdots + \phi_n \beta_{u,v-n} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction; and
#' - \eqn{n} is \text{n_coef}.
#'
#' Internally, `AR()` derives a value for \eqn{\omega} that
#' gives every element of \eqn{\beta} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#'
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#'
#' where `s` is provided by the user.
#' 
#' The autocorrelation coefficients \eqn{\phi_1, \cdots, \phi_n}
#' are restricted to values between -1 and 1 that jointly
#' lead to a stationary model. The quantity
#' \eqn{r = \sqrt{\phi_1^2 + \cdots + \phi_n^2}} has the
#' boundary-avoiding prior
#'
#' \deqn{r \sim \text{Beta}(2, 2).}
#' 
#' @param n_coef Thehe number of lagged terms in the
#' model, ie the order of the model. Default is `2`.
#' @param s Scale for the prior for the innovations.
#' Default is `1`.
#' @param along Name of the variable to be used
#' as the "along" variable. Only used with
#' interactions.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR1()] Special case of `AR()`
#' - [Lin_AR()] AR process combined with straight line
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' 
#' @references
#' - `AR()` is based on the TMB function
#' [ARk](http://kaskr.github.io/adcomp/classdensity_1_1ARk__t.html#details)
#'
#' @examples
#' AR(n_coef = 3)
#' AR(n_coef = 3, s = 2.4)
#' AR(along = "cohort")
#' @export
AR <- function(n_coef = 2, s = 1, along = NULL) {
  check_n(n = n_coef,
          nm_n = "n_coef",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  check_scale(s,
              nm_x = "s",
              zero_ok = FALSE)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  new_bage_prior_ar(n_coef = n_coef,
                    min = -1,
                    max = 1,
                    scale = scale,
                    along = along,
                    nm = "AR")
}


## HAS_TESTS
#' AR1 Prior
#'
#' Prior for main effect or interaction. 
#' An autoregressive process with order 1.
#' Typically used with time.
#'
#' If `AR1()` is used with an interaction,
#' then separate AR1 series constructed along
#' the "along" variable within
#' each combination of the
#' "by" variables. 
#' 
#' Argument `s` controls the size of innovations. Smaller values
#' for `s` tend to give smoother series.
#'
#' @section Mathematical details:
#'
#' When `AR1()` is used with a main effect,
#'
#' \deqn{\beta_j = \phi \beta_{j-1} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \phi \beta_{u,v-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction; and
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction.
#'
#' Internally, `AR1()` derives a value for \eqn{\omega} that
#' gives every element of \eqn{\beta} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#' 
#' Coefficient \eqn{\phi} is constrained
#' to lie between `min` and `max`.
#' Its prior distribution is
#' 
#' \deqn{\phi = (\text{max} - \text{min}) \phi' - \text{min}}
#' 
#' where
#' 
#' \deqn{\phi' \sim \text{Beta}(2, 2).}
#' 
#' @inheritParams AR
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR()] Generalisation of `AR1()`
#' - [Lin_AR1()] AR1 process combined with straight line
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' 
#' @references
#' - `AR1()` is based on the TMB function
#' [AR1](http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.html#details)
#' - The defaults for `min` and `max` are based on the
#'   defaults for `forecast::ets()`.
#'
#' @examples
#' AR1()
#' AR1(min = 0, max = 1, s = 2.4)
#' AR1(along = "cohort")
#' @export
AR1 <- function(min = 0.8, max = 0.98, s = 1, along = NULL) {
  check_min_max_ar(min = min, max = max)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  min <- as.double(min)
  max <- as.double(max)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  new_bage_prior_ar(n_coef = 1L,
                    min = min,
                    max = max,
                    scale = scale,
                    along = along,
                    nm = "AR1")
}


## HAS_TESTS
#' Known Prior
#'
#' Treat an intercept, a main effect, or an interaction
#' as fixed and known.
#'
#' @param values A numeric vector
#'
#' @returns An object of class `"bage_prior_known"`.
#'
#' @seealso
#' - [NFix()] Prior where level unknown, but variability known.
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
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


## HAS_TESTS
#' Linear Prior
#'
#' Prior for main effect or interaction.
#' A straight line or lines, combined with independent
#' normal errors. Typically used with time.
#'
#' If `Lin()` is used with an interaction,
#' then separate lines are constructed along 
#' the "along" variable, within each combination
#' of the "by" variables.
#' 
#' Argument `s` controls the size of the errors.
#' 
#' @section Mathematical details:
#'
#' When `Lin()` is used with a main effect,
#'
#' \deqn{\beta_j = \eta q_j + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \tau^2),}
#' 
#' and when it is used with an interaction,
#' 
#' \deqn{\beta_{u,v} \sim \eta_u q_v + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{u} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction;
#' - \deqn{q = - (J+1)/(J-1) + 2j/(J-1);} and
#' - \deqn{q_v = - (V+1)/(V-1) + 2v/(V-1)}.
#' 
#' The slopes have priors
#' \deqn{\eta \sim \text{N}(0, \text{sd}^2)}
#' and
#' \deqn{\eta_u \sim \text{N}(0, \text{sd}^2).}
#' Larger values for `sd` permit steeper slopes.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#'
#' @inheritParams AR
#' @param s Scale for the prior for the errors.
#' Default is `1`.
#' @param sd Standard deviation in prior for slope
#' of line. Default is 1.
#'
#' @returns An object of class `"bage_prior_lin"`.
#'
#' @seealso
#' - [Lin_AR()] Linear with AR errors
#' - [Lin_AR1()] Linear with AR1 errors
#' - [RW2()] Random walk with drift
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' Lin()
#' Lin(s = 0.5, sd = 2)
#' Lin(along = "cohort")
#' @export
Lin <- function(s = 1, sd = 1, along = NULL) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = FALSE)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  scale <- as.double(s)
  sd_slope <- as.double(sd)
  new_bage_prior_lin(scale = scale,
                     sd_slope = sd_slope,
                     along = along)
}


## HAS_TESTS
#' Linear Prior with Autoregressive Errors
#'
#' Prior for main effect or interaction.
#' A straight line or lines, combined with
#' autoregressive errors. Typically used with time.
#'
#' If `Lin_AR()` is used with an interaction,
#' then, within each combination of the "by" variables,
#' the "along" variable is modelled as a straight line
#' with autoregressive errors.
#'
#' Argument `s` controls the size of the errors.
#' 
#' @section Mathematical details:
#'
#' When `Lin_AR()` is used with a main effect,
#'
#' \deqn{\beta_j = \eta q_j + \epsilon_j}
#' \deqn{\epsilon_j = \phi_1 \epsilon_{j-1} + \cdots + \phi_n \epsilon_{j-n} + \varepsilon_j,}
#' \deqn{\varepsilon_j \sim \text{N}(0, \omega^2).}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \eta_j q_{u,v} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} = \phi_1 \epsilon_{u,v-1} + \cdots + \phi_n \epsilon_{u,v-n} + \varepsilon_{u,v},}
#' \deqn{\varepsilon_{u,v} \sim \text{N}(0, \omega^2).}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{u} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction;
#' - \eqn{n} is \text{n_coef};
#' - \eqn{q = - (J+1)/(J-1) + 2j/(J-1);} and
#' - \eqn{q_v = - (V+1)/(V-1) + 2v/(V-1)}.
#'
#' The slopes have priors
#' \deqn{\eta \sim \text{N}(0, \text{sd}^2)}
#' and
#' \deqn{\eta_u \sim \text{N}(0, \text{sd}^2).}
#' Larger values for `sd` permit steeper slopes.
#'
#' Internally, `Lin_AR()` derives a value for \eqn{\omega} that
#' gives \eqn{\epsilon_j} or \eqn{\epsilon_{u,v}} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where a value for `s` is provided by the user.
#'
#' The \eqn{\phi_1, \cdots, \phi_k} are restricted to values
#' between -1 and 1 that jointly lead to a stationary model. The quantity
#' \eqn{r = \sqrt{\phi_1^2 + \cdots + \phi_k^2}} is given a
#' boundary-avoiding prior
#'
#' \deqn{r \sim \text{Beta}(2, 2).}
#' 
#' Slope \eqn{\eta} is drawn from a normal distribution,
#' \deqn{\eta \sim \text{N}(0, (\text{sd})^2).}
#' 
#' @inheritParams AR
#' @param n_coef The number of lagged terms in the
#' model, ie the order of the model. Default is `2`.
#' @param s Scale for the innovations in the
#' AR process. Default is `1`.
#' @param sd Standard deviation in the prior for
#' the slope of the line. Larger values imply
#' steeper slopes. Default is 1.
#'
#' @returns An object of class `"bage_prior_linar"`.
#'
#' @seealso
#' - [Lin_AR1()] Special case of `Lin_AR()`
#' - [AR()] AR process with no line
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' Lin_AR()
#' Lin_AR(n_coef = 3, s = 0.5, sd = 2)
#' @export
Lin_AR <- function(n_coef = 2, s = 1, sd = 1, along = NULL) {
  check_n(n = n_coef,
          nm_n = "n_coef",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  check_scale(s,
              nm_x = "s",
              zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  sd_slope <- as.double(sd)
  new_bage_prior_linar(n_coef = n_coef,
                       scale = scale,
                       sd_slope = sd_slope,
                       min = -1,
                       max = 1,
                       along = along,
                       nm = "Lin_AR")
}


## HAS_TESTS
#' Linear Prior with AR1 Errors
#'
#' Prior for main effect or interaction.
#' Combines a straight line or lines with
#' AR1 errors. Typically used with time.
#'
#' If `Lin_AR1()` is used with an interaction,
#' then, within each combination of the "by" variables,
#' the "along" variable is modelled as a straight line
#' with autoregressive errors.
#'
#' Argument `s` controls the size of the errors.
#'
#' @section Mathematical details:
#'
#' When `Lin_AR1()` is being used with a main effect,
#'
#' \deqn{\beta_j = \eta q_j + \epsilon_j}
#' \deqn{\epsilon_j = \phi \epsilon_{j-1} + \varepsilon_j,}
#' \deqn{\varepsilon_j \sim \text{N}(0, \omega^2).}
#'
#' and when it is being used with an interaction,
#'
#' \deqn{\beta_{u,v} = \eta_j q_{u,v} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} = \phi + \varepsilon_{u,v},}
#' \deqn{\varepsilon_{u,v} \sim \text{N}(0, \omega^2).}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{u} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction;
#' - \eqn{q = - (J+1)/(J-1) + 2j/(J-1);} and
#' - \eqn{q_v = - (V+1)/(V-1) + 2v/(V-1)}.
#'
#' The slopes have priors
#' \deqn{\eta \sim \text{N}(0, \text{sd}^2)}
#' and
#' \deqn{\eta_u \sim \text{N}(0, \text{sd}^2).}
#' Larger values for `sd` permit steeper slopes.
#'
#' Internally, `Lin_AR1()` derives a value for \eqn{\omega} that
#' gives \eqn{\epsilon_j} or \eqn{\epsilon_{u,v}} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where a value for `s` is provided by the user.
#'
#' Coefficient \eqn{\phi} is constrained
#' to lie between `min` and `max`.
#' Its prior distribution is
#' \deqn{\phi = (\text{max} - \text{min}) \phi' - \text{min}}
#' where
#' \deqn{\phi' \sim \text{Beta}(2, 2).}
#' 
#' @inheritParams Lin_AR
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_linar"`.
#'
#' @seealso
#' - [Lin_AR()] Generalisation of `Lin_AR1()`
#' - [AR1()] AR1 process with no line
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @references
#' - The defaults for `min` and `max` are based on the
#'   defaults for `forecast::ets()`.
#'
#' @examples
#' Lin_AR1()
#' Lin_AR1(min = 0, s = 0.5, sd = 2)
#' @export
Lin_AR1 <- function(min = 0.8, max = 0.98, s = 1, sd = 1, along = NULL) {
  check_min_max_ar(min = min, max = max)
  check_scale(s,
              nm_x = "s",
              zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = FALSE)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  scale <- as.double(s)
  sd_slope <- as.double(sd)
  min <- as.double(min)
  max <- as.double(max)
  new_bage_prior_linar(n_coef = 1L,
                       scale = scale,
                       sd_slope = sd_slope,
                       min = min,
                       max = max,
                       along = along,
                       nm = "Lin_AR1")
}


## HAS_TESTS
#' Normal Prior
#'
#' Prior for a main effect or interaction.
#' Values are drawn independently from
#' a common normal distribution.
#' Typically used with variables other than
#' age or time, such as region or ethncity.
#'
#' Argument `s` controls the size of errors. Smaller values
#' for `s` tend to give more tightly clustered estimates
#' for the elements of the main effect or interaction.
#'
#' @section Mathematical details:
#'
#' \deqn{\beta_j \sim \text{N}(0, \tau^2)}
#'
#' where \eqn{\beta} is the main effect or interaction.
#' 
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#'
#' @param s Scale for the standard deviation.
#' Default is `1`.
#' 
#' @returns An object of class `"bage_prior_norm"`.
#'
#' @seealso 
#' - [NFix()] Similar to `N()` but standard deviation
#'   parameter is supplied rather than estimated from data
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' N()
#' N(s = 0.5)
#' @export
N <- function(s = 1) {
    check_scale(s, nm_x = "s", zero_ok = FALSE) 
    scale <- as.double(s)
    new_bage_prior_norm(scale = scale)
}


## HAS_TESTS
#' Normal Prior with Fixed Variance
#'
#' Normal prior where, in contrast to [N()], the
#' variance is treated as fixed and known.
#'
#' `NFix()` is the default prior for the intercept.
#'
#' @section Mathematical details:
#'
#' \deqn{\beta_j \sim \text{N}(0, \tau^2)}
#'
#' where \eqn{\beta} is the main effect or interaction,
#' and a value for `sd` is supplied by the user.
#'
#' @param sd Standard deviation. Default is `1`.
#'
#' @returns An object of class `"bage_prior_normfixed"`.
#'
#' @seealso 
#' - [N()] Similar to `NFix()`, but standard deviation
#'   parameter is estimated from the data rather
#'   than being fixed in advance
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' 
#' @examples
#' NFix()
#' NFix(sd = 10)
#' @export
NFix <- function(sd = 1) {
    check_scale(sd, nm_x = "sd", zero_ok = FALSE) 
    sd <- as.double(sd)
    new_bage_prior_normfixed(sd = sd)
}


## HAS_TESTS
#' Random Walk Prior
#'
#' Prior for a main effect or interaction.
#' A random walk. Typically used with time.
#'
#' If `RW()` is used with an interaction,
#' then separate random walks are used for
#' the "along" variable within
#' each combination of the
#' "by" variables.
#' 
#' Argument `s` controls the size of innovations.
#' Smaller values for `s` tend to give smoother series.
#' 
#' @section Mathematical details:
#'
#' When `RW()` is used with a main effect,
#'
#' \deqn{\beta_j = \beta_{j-1} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \tau^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \beta_{u,v-1} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction; and
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction.
#'
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#'
#' @inheritParams AR
#'
#' @returns An object of class `"bage_prior_rw"`.
#'
#' @seealso
#' - [RW2()] Random walk with drift
#' - [AR()] Autoressive with order k
#' - [AR1()] Autoressive with order 1
#' - [Sp()] Smoothing via splines
#' - [SVD()] Smoothing of age via singular value decomposition
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' RW()
#' RW(s = 0.5)
#' RW(along = "cohort")
#' @export
RW <- function(s = 1, along = NULL) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  new_bage_prior_rw(scale = scale,
                    along = along)
}


## HAS_TESTS
#' Random Walk Prior, with Seasonal Effect
#'
#' Prior for a main effect or interaction,
#' typically involving time. Combines
#' a random walk with a seasonal effect.
#'
#' If `RW_Seas()` is used with an interaction,
#' then separate series are used for
#' the "along" variable within
#' each combination of the
#' "by" variables.
#'
#' Argument `s` controls the size of innovations in the random walk.
#' Smaller values for `s` tend to give smoother series.
#'
#' Argument `n_seas` controls the number of `seasons`. 
#' When using quarterly data, for instance,
#' `n_seas` should be `4`, and when using
#' monthly data, `n_seas` should be `12`.
#'
#' By default, the magnitude of seasonal effects
#' can change over time. However, setting `s_seas`
#' to `0` produces seasonal effects that are "fixed",
#' ie that are the same every year.
#'
#' @section Mathematical details:
#'
#' When `RW_Seas()` is used with a main effect,
#'
#' \deqn{\beta_j = \alpha_j + \lambda_j}
#' \deqn{\alpha_j \sim \text{N}(\alpha_{j-1}, \tau^2)}
#' \deqn{\lambda_j \sim \text{N}(\lambda_{j-n}, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \alpha_{u,v} + \lambda_{u,v}}
#' \deqn{\alpha_{u,v} \sim \text{N}(\alpha_{u,v-1}, \tau^2),}
#' \deqn{\lambda_{u,v} \sim \text{N}(\lambda_{u,v-n}, \omega^2)}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{\alpha_j} or \eqn{\alpha_{u,v}} is an element of the random walk;
#' - \eqn{\lambda_j} or \eqn{\lambda_{u,v}} is an element of the seasonal effect;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction; and
#' - \eqn{n} is `n_seas`.
#'
#' Parameter \eqn{\omega} has a half-normal prior
#' \deqn{\omega \sim \text{N}^+(0, \text{s\_seas}^2),}
#' where `s_seas` is provided by the user. If
#' `s_seas` is set to 0, then \eqn{\omega} is 0,
#' and the seasonal effects are fixed over time.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#' 
#' @inheritParams AR
#' 
#' @param n_seas Number of seasons
#' @param s Scale for prior for innovations in
#' the random walk. Default is `1`.
#' @param s_seas Scale for prior for innovations
#' in the seasonal effect. Default is `1`.
#' Can be `0`.
#'
#' @returns Object of class `"bage_prior_rwseasvary"`
#' or `"bage_prior_rwseasfix"`.
#'
#' @seealso
#' - [RW()] Random walk without seasonal effect
#' - [RW2_Seas()] Random walk with drift, with seasonal effect
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' RW_Seas(n_seas = 4)             ## seasonal effects evolve
#' RW_Seas(n_seas = 4, s_seas = 0) ## seasonal effects fixed
#' @export
RW_Seas <- function(n_seas, s = 1, s_seas = 1, along = NULL) {
  check_n(n = n_seas,
          nm_n = "n_seas",
          min = 2L,
          max = NULL,
          null_ok = FALSE)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(s_seas, nm_x = "s_seas", zero_ok = TRUE)
  n_seas <- as.integer(n_seas)
  scale <- as.double(s)
  scale_seas = as.double(s_seas)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  if (scale_seas > 0)
    new_bage_prior_rwseasvary(n_seas = n_seas,
                              scale = scale,
                              scale_seas = scale_seas,
                              along = along)
  else
    new_bage_prior_rwseasfix(n_seas = n_seas,
                             scale = scale,
                             along = along)
}


## HAS_TESTS
#' Random Walk with Drift Prior
#'
#' Prior for a main effect or interaction.
#' A random walk with drift, also referred
#' to as a second-order random walk.
#' Typically used with age, or used with time
#' when there are sustained upward
#' or downward trends.
#'
#' If `RW2()` is used with an interaction,
#' then separate random walks are used for
#' the "along" variable within
#' each combination of the
#' "by" variables.
#' 
#' Argument `s` controls the size of errors. Smaller values
#' for `s` tend to give smoother series.
#'
#' @section Mathematical details:
#'
#' When `RW()` is used with a main effect,
#'
#' \deqn{\beta_j = 2 \beta_{j-1} - \beta_{j-2} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \tau^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = 2\beta_{u,v-1} - \beta_{u,v-2} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction; and
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction.
#'
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#'
#' @inheritParams AR
#'
#' @returns An object of class `"bage_prior_rw2"`.
#'
#' @seealso
#' - [RW()] Random walk
#' - [AR()] Autoressive with order k
#' - [AR1()] Autoressive with order 1
#' - [Sp()] Smoothing via splines
#' - [SVD()] Smoothing of age via singular value decomposition
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' RW2()
#' RW2(s = 0.5)
#' @export
RW2 <- function(s = 1, along = NULL) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  scale <- as.double(s)
  new_bage_prior_rw2(scale = scale,
                     along = along)
}


## HAS_TESTS
#' Random Walk with Drift Prior, with Seasonal Effect
#'
#' Prior for a main effect or interaction,
#' typically involving time. Combines
#' a random walk with drift (ie a second-order random walk)
#' and a seasonal effect.
#'
#' If `RW2_Seas()` is used with an interaction,
#' then separate series are used for
#' the "along" variable within
#' each combination of the
#' "by" variables.
#'
#' Argument `s` controls the size of innovations in the random walk.
#' Smaller values for `s` tend to give smoother series.
#'
#' Argument `n_seas` controls the number of `seasons`. 
#' When using quarterly data, for instance,
#' `n_seas` should be `4`, and when using
#' monthly data, `n_seas` should be `12`.
#'
#' By default, the magnitude of seasonal effects
#' can change over time. However, setting `s_seas`
#' to `0` produces seasonal effects that are "fixed",
#' ie that are the same every year.
#'
#' @section Mathematical details:
#'
#' When `RW2_Seas()` is used with a main effect,
#'
#' \deqn{\beta_j = \alpha_j + \lambda_j}
#' \deqn{\alpha_j \sim \text{N}(2 \alpha_{j-1} - \alpha_{j-2}, \tau^2)}
#' \deqn{\lambda_j \sim \text{N}(\lambda_{j-n}, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \alpha_{u,v} + \lambda_{u,v}}
#' \deqn{\alpha_{u,v} \sim \text{N}(2 \alpha_{u,v-1} - \alpha_{u,v-2}, \tau^2),}
#' \deqn{\lambda_{u,v} \sim \text{N}(\lambda_{u,v-n}, \omega^2)}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{\alpha_j} or \eqn{\alpha_{u,v}} is an element of the random walk;
#' - \eqn{\lambda_j} or \eqn{\lambda_{u,v}} is an element of the seasonal effect;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the "along" variable of the interaction;
#' - \eqn{u} denotes position within the "by" variable(s) of the interaction; and
#' - \eqn{n} is `n_seas`.
#'
#' Parameter \eqn{\omega} has a half-normal prior
#' \deqn{\omega \sim \text{N}^+(0, \text{s\_seas}^2),}
#' where `s_seas` is provided by the user. If
#' `s_seas` is set to 0, then \eqn{\omega} is 0,
#' and the seasonal effects are fixed over time.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
#' where `s` is provided by the user.
#' 
#' @inheritParams AR
#' 
#' @param n_seas Number of seasons
#' @param s Scale for prior for innovations in
#' the random walk. Default is `1`.
#' @param s_seas Scale for prior for innovations
#' in the seasonal effect. Default is `1`.
#' Can be `0`.
#'
#' @returns Object of class `"bage_prior_rw2seasvary"`
#' or `"bage_prior_rw2seasfix"`.
#'
#' @seealso
#' - [RW2()] Random walk with drift, without seasonal effect
#' - [RW_Seas()] Random walk, with seasonal effect
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @examples
#' RW2_Seas(n_seas = 4)             ## seasonal effects evolve
#' RW2_Seas(n_seas = 4, s_seas = 0) ## seasonal effects fixed
#' @export
RW2_Seas <- function(n_seas, s = 1, s_seas = 1, along = NULL) {
  check_n(n = n_seas,
          nm_n = "n_seas",
          min = 2L,
          max = NULL,
          null_ok = FALSE)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(s_seas, nm_x = "s_seas", zero_ok = TRUE)
  n_seas <- as.integer(n_seas)
  scale <- as.double(s)
  scale_seas = as.double(s_seas)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  if (scale_seas > 0)
    new_bage_prior_rw2seasvary(n_seas = n_seas,
                               scale = scale,
                               scale_seas = scale_seas,
                               along = along)
  else
    new_bage_prior_rw2seasfix(n_seas = n_seas,
                              scale = scale,
                              along = along)
}


## HAS_TESTS
#' P-Spline Prior
#'
#' A p-spline (penalised spine) prior for main
#' effects or interactions. Typically used with age,
#' but can be used with any variable where outcomes are
#' expected to vary smoothly from one element to the next.
#'
#' If `Sp()` is used with an interaction,
#' then separate splines are used for
#' the "along" variable within
#' each combination of the
#' "by" variables.
#'
#' @section Mathematical details:
#'
#' When `Sp()` is used with a main effect,
#'
#' \deqn{\pmb{\beta} = \pmb{X} \pmb{\alpha}}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\pmb{\beta}_u = \pmb{X} \pmb{\alpha}_u}
#'
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction, with \eqn{J} elements;
#' - \eqn{\pmb{beta}_u} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the  \eqn{u}th combination of the "by" variables;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{V} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{X} is a \eqn{J \times n} or \eqn{V \times n} matrix of
#'   spline basis functions; and
#' - \eqn{n} is `n_comp`.
#'
#' The elements of \eqn{\pmb{\alpha}} or \eqn{\pmb{alpha}_u} are assumed
#' to follow a [random walk with drift][RW2()].
#'
#' @inheritParams AR
#' @param n_comp Number of spline basis functions (components)
#' to use.
#'
#' @returns An object of class `"bage_prior_spline"`.
#'
#' @seealso
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via random walk with drift
#' - [SVD()] Smoothing of age via singlular value decomposition
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - **bage** uses function [splines::bs()] to construct
#'   spline basis functions
#'
#' @references
#' - Eilers, P.H.C. and Marx B. (1996).
#'   "Flexible smoothing with B-splines and penalties".
#'   Statistical Science. 11 (2): 89–121.
#'
#' @examples
#' Sp()
#' Sp(n_comp = 10)
#' @export
Sp <- function(n_comp = NULL, s = 1, along = NULL) {
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 4L,
          max = NULL,
          null_ok = TRUE)
  if (!is.null(n_comp))
    n_comp <- as.integer(n_comp)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  new_bage_prior_spline(n_comp = n_comp,
                        scale = scale,
                        along = along)
}


## HAS_TESTS
#' SVD Prior for Age
#'
#' A Singular Value Decomposition (SVD) prior
#' for a main effect or interaction involving age.
#'
#' A `SVD()` prior assumes that the age profile for the quantity
#' being modelled looks like it was drawn at random
#' from an external demographic database. For instance,
#' the prior obtained via
#' ```
#' SVD(HMD)
#' ```
#' assumes that age profiles look like
#' they were obtained from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' If `SVD()` is used with an interaction,
#' then separate profiles are constructed along
#' within each combination of the non-age variables.
#'
#' Age-sex or age-gender profiles should be
#' modelled using [SVDS()] rather than `SVD()`.
#'
#' @section Mathematical details:
#' 
#' When `SVD()` is used with a main effect,
#' 
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\pmb{\beta}_u = \pmb{F} \pmb{\alpha}_u + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is a main effect or interaction involving age;
#' - \eqn{\pmb{\beta}_u} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the  \eqn{u}th combination of the non-age variables;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{V} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{n} is `n_comp`;
#' - \eqn{\pmb{F}} is a known matrix with dimension \eqn{J \times n}
#'   if \eqn{\pmb{\beta}} is a main effect, and dimension \eqn{V \times n}
#'   if \eqn{\pmb{\beta}} is an interaction; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements if
#'   if \eqn{\pmb{\beta}} is a main effect and \eqn{V}
#'   elements if \eqn{\pmb{\beta}} is an interaction.
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by performing an SVD and standardizing.
#'
#' The elements of \eqn{\pmb{\alpha}} have prior
#' \deqn{\alpha_k \sim \text{N}(0, 1), \quad k = 1, \cdots, K.}
#' 
#' @section Scaled SVDs of demographic databases in bage:
#'
#' - \code{\link{HMD}} Mortality rates from the
#' [Human Mortality Database](https://www.mortality.org).
#' - \code{\link{LFP}} Labour forcce participation
#' rates from the [OECD](https://data-explorer.oecd.org).
#'
#' @param ssvd Object of class `"bage_ssvd"`
#' holding a scaled SVD. See below for scaled SVDs
#' of databases currently available in **bage**.
#' @param n_comp Number of components from scaled SVD
#' to use in modelling. The default is half
#' the number of components of `ssvd`.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @seealso
#' - [SVDS()] SVD prior for age-sex or age-gender profile
#' - [SVDS_RW()] SVD prior for age with time-varying coeffients
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via random walk with drift
#' - [Sp()] Smoothing via splines
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the vignette
#'   [here](https://bayesiandemography.github.io/bage/articles/vig4_svd.html).
#'
#' @examples
#' SVD(HMD)
#' SVD(HMD, n_comp = 3)
#' @export
SVD <- function(ssvd, n_comp = NULL) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp, nm_n_comp = "n_comp", ssvd = ssvd)
  new_bage_prior_svd(ssvd = ssvd,
                     nm_ssvd = nm_ssvd,
                     n_comp = n_comp,
                     joint = NULL)
}


## HAS_TESTS
#' SVD Prior for Age-Sex or Age-Gender Interaction
#'
#' A Singular Value Decomposition (SVD) prior
#' for an interaction involving age-sex
#' profiles or age-gender profiles.
#'
#' A `SVDS()` prior assumes that the age-sex profile
#' or age-gender profile for the quantity being
#' modelled looks like it was drawn at random
#' from an external demographic database. For instance,
#' the prior obtained via
#' ```
#' SVDS(HMD)
#' ```
#' assumes that age-sex profiles look like
#' they were obtained from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' If `SVDS()` is used with an interaction
#' between age, sex/gender, and one or more
#' other variables, then separate age-sex or
#' age-gender profiles are constructed within
#' each combination of the other variables.
#'
#' Age profiles with no sex or gender dimension
#' should be modelled using [SVD()] rather than
#' `SVDS()`.
#'
#' @section Joint or independent SVDs:
#'
#' Two possible ways of extracting patterns
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
#'
#' **Case 1: Interaction involving age and sex or gender, but no other variables**
#'
#' When `SVDS()` is used with `joint = FALSE`,
#' 
#' \deqn{\pmb{\beta}_{s} = \pmb{F}_s \pmb{\alpha}_{s} + \pmb{g}_s,}
#'
#' and when `SVDS()` is used with `joint = TRUE`,
#'
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving age and sex/gender;
#' - \eqn{\pmb{\beta}_{s}} is a subvector of \eqn{\pmb{\beta}},
#'   holding values for sex/gender \eqn{s};
#' - \eqn{J} is the number of elements in \eqn{\pmb{\beta}};
#' - \eqn{S} is the number of sexes/genders;
#' - \eqn{n} is `n_comp`;
#' - \eqn{\pmb{F}_s} is a known \eqn{(J/S) \times n} matrix, specific
#'   to sex/gender \eqn{s};
#' - \eqn{\pmb{g}_s} is a known vector with \eqn{J/S} elements,
#'   specific to sex/gender \eqn{s};
#' - \eqn{\pmb{F}} is a known \eqn{J \times n} matrix, with values
#'   for all sexes/genders; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements, with values
#'   for all sexes/genders.
#'
#' The elements of \eqn{\pmb{\alpha}_s} and \eqn{\pmb{\alpha}} have prior
#' \deqn{\alpha_k \sim \text{N}(0, 1).}
#'
#' 
#' 
#' **Case 2: Interaction involving age and sex/gender, and one or more other variables**
#'
#' When `SVDS()` is used with `joint = FALSE`,
#' 
#' \deqn{\pmb{\beta}_{u,s} = \pmb{F}_s \pmb{\alpha}_{u,s} + \pmb{g}_s,}
#'
#' and when `SVDS()` is used with `joint = TRUE`,
#'
#' \deqn{\pmb{\beta}_u = \pmb{F} \pmb{\alpha}_u + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving sex/gender;
#' - \eqn{\pmb{\beta}_{u,s}} is a subvector of \eqn{\pmb{\beta}},
#'   holding values for sex/gender \eqn{s} for the \eqn{u}th
#'   combination of the other variables;
#' - \eqn{V} is the number of elements in \eqn{\pmb{\beta}_u};
#' - \eqn{S} is the number of sexes/genders;
#' - \eqn{n} is `n_comp`;
#' - \eqn{\pmb{F}_s} is a known \eqn{(V/S) \times n} matrix, specific
#'   to sex/gender \eqn{s};
#' - \eqn{\pmb{g}_s} is a known vector with \eqn{V/S} elements,
#'   specific to sex/gender \eqn{s};
#' - \eqn{\pmb{F}} is a known \eqn{V \times n} matrix, with values
#'   for all sexes/genders; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{V} elements, with values
#'   for all sexes/genders.
#'
#' The elements of \eqn{\pmb{\alpha}_{u,s}} and \eqn{\pmb{\alpha}_u} have prior
#' \deqn{\alpha_k \sim \text{N}(0, 1).}
#' 
#'
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @inheritParams SVD
#' @param joint Whether to use combined or
#' separate SVDs. Default is `FALSE`.
#' See below for details.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @seealso
#' - [SVD()] SVD prior for age, with no sex or gender
#' - [SVDS_RW()] SVD prior for age and sex/gender with time-varying coeffients
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via random walk with drift
#' - [Sp()] Smoothing via splines
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the vignette
#'   [here](https://bayesiandemography.github.io/bage/articles/vig4_svd.html).
#'
#' @examples
#' SVDS(HMD)
#' SVDS(HMD, joint = TRUE)
#' @export
SVDS <- function(ssvd, n_comp = NULL, joint = FALSE) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  if (!has_sexgender(ssvd)) {
    cli::cli_abort(c("{.arg ssvd} does not have a sex/gender dimension.",
                     i = "Try prior {.val SVD()} instead?",
                     i = "For a list of priors, see {.topic bage::priors}."))
  }
  n_comp <- n_comp_svd(n_comp = n_comp, nm_n_comp = "n_comp", ssvd = ssvd)
  check_flag(x = joint, nm_x = "joint")
  new_bage_prior_svd(ssvd = ssvd,
                     nm_ssvd = nm_ssvd,
                     n_comp = n_comp,
                     joint = joint)
}



## NO_TESTS
#' SVD Priors for Age, With Time-Varying Coefficients
#'
#' Singular Value Decomposition (SVD) priors
#' for interactions involving age and time, and possibly
#' other variables, where the coefficients evolve over time.
#'
#' `SVD_AR()`, `SVD_AR1()`, `SVD_RW()`, and `SVD_RW2()`
#' priors assume that, in any given period,
#' the age profiles for the quantity
#' being modelled looks like they were drawn at random
#' from an external demographic database. For instance,
#' the `SVD_AR()` prior obtained via
#' ```
#' SVD_AR(HMD)
#' ```
#' assumes that profiles look like
#' they were obtained from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' Interactions that involve sex or gender
#' as well age and time should be
#' modelled using [SVDS_AR()], [SVDS_AR1()], [SVDS_RW()],
#' or [SVDS_RW2()] priors, rather than `SVD_AR()`,
#' `SVDS_AR1()`, `SVDS_RW()`, or `SVDS_RW2()` priors.
#'
#' @section Mathematical details:
#' 
#' When the interaction being modelled only involves
#' age and time,
#' 
#' \deqn{\pmb{\beta}_t = \pmb{F} \pmb{\alpha}_t + \pmb{g},}
#'
#' and when it involives other variables besides age and time,
#'
#' \deqn{\pmb{\beta}_{u,t} = \pmb{F} \pmb{\alpha}_{u,t} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving age and time;
#' - \eqn{\pmb{\beta}_t} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the period \eqn{t};
#' - \eqn{\pmb{\beta}_{u,t}} is a subvector of \eqn{\pmb{\beta}_t} holding
#'   values for the  \eqn{u}th combination of the "by" variables;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{V} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{n} is `n_coef`;
#' - \eqn{\pmb{F}} is a known matrix with dimension \eqn{J \times n}
#'   if \eqn{\pmb{\beta}} is a main effect and dimension \eqn{V \times n}
#'   if \eqn{\pmb{\beta}} is an interaction; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements if
#'   if \eqn{\pmb{\beta}} is a main effect and \eqn{V}
#'   elements if \eqn{\pmb{\beta}} is an interaction.
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by performing an SVD and standardizing.
#'
#' With `SVD_AR()`, the prior for the elements of \eqn{\pmb{\alpha}_t} or \eqn{\pmb{\alpha}_{u,t}} is
#'
#' \deqn{\alpha_{kt} = \phi_1 \alpha_{k,t-1} + \cdots + \phi_n \beta_{k,t-n} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi_1 \alpha_{k,u,t-1} + \cdots + \phi_n \beta_{k,u,t-n} + \epsilon_{k,u,t}};
#'
#' with `SVD_AR1()`, it is
#'
#' \deqn{\alpha_{kt} = \phi \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi \alpha_{k,u,t-1} + \epsilon_{k,u,t}};
#'
#' with `SVD_RW()`, it is
#'
#' \deqn{\alpha_{kt} = \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \alpha_{k,u,t-1} + \epsilon_{k,u,t}};
#' 
#' and with `SVD_RW2()`, it is
#'
#' \deqn{\alpha_{kt} = 2 \alpha_{k,t-1} - \alpha_{k,t-2} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = 2 \alpha_{k,u,t-1} - \alpha_{k,u,t-2} + \epsilon_{k,u,t}}.
#'
#' For more on the \eqn{\phi} and \eqn{\epsilon}, see [AR()], [AR1()],
#' [RW()], and [RW2()].
#' 
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @inheritParams SVD
#' @param n_comp Number of components from scaled SVD
#' to use in modelling. The default is half
#' the number of components of `ssvd`.
#' @param n_coef Number of AR coefficients in `SVD_RW()`.
#' @param s Scale for standard deviations terms.
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient in `SVD_AR()`.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_svd_ar"`,
#' `"bage_prior_svd_rw"`, or `"bage_prior_svd_rw2"`.
#'
#' @seealso
#' - [SVDS()] SVD prior for non-time-varying age-sex or age-gender profile
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via random walk with drift
#' - [Sp()] Smoothing via splines
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the vignette
#'   [here](https://bayesiandemography.github.io/bage/articles/vig4_svd.html).
#'
#' @examples
#' SVD_AR1(HMD)
#' SVD_RW(HMD, n_comp = 3)
#' @export
SVD_AR <- function(ssvd, n_comp = NULL, n_coef = 2, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_n(n = n_coef,
          nm_n = "n_coef",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  check_scale(x = s,
              nm_x = "s",
              zero_ok = FALSE)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  new_bage_prior_svd_ar(ssvd = ssvd,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        n_coef = n_coef,
                        scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_AR1 <- function(ssvd, n_comp = NULL, min = 0.8, max = 0.98, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_min_max_ar(min = min, max = max)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  min <- as.double(min)
  max <- as.double(max)
  scale <- as.double(s)
  new_bage_prior_svd_ar1(ssvd = ssvd,
                         nm_ssvd = nm_ssvd,
                         n_comp = n_comp,
                         min = min,
                         max = max,
                         scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_RW <- function(ssvd, n_comp = NULL, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  new_bage_prior_svd_rw(ssvd = ssvd,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_RW2 <- function(ssvd, n_comp = NULL, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  new_bage_prior_svd_rw2(ssvd = ssvd,
                         nm_ssvd = nm_ssvd,
                         n_comp = n_comp,
                         scale = scale)
}






## NO_TESTS
#' SVD Priors for Age-Sex or Age-Gender Interactions,
#' With Time-Varying Coefficients
#'
#' Singular Value Decomposition (SVD) priors
#' for interactions involving age, sex or gender,
#' time, and possibly other variables.
#'
#' `SVD_AR()`, `SVD_AR1()`, `SVD_RW()`, and `SVD_RW2()`
#' priors assume that, in any given period,
#' the age-sex profile or age-gender
#' profiles for the quantity being modelled look
#' they were drawn at random
#' from an external demographic database. For instance,
#' the prior obtained via
#' ```
#' SVDSAR(HMD)
#' ```
#' favours age-sex profiles that look like
#' they were obtained from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' AgeInteractions that involve sex or gender
#' as well age and time should be
#' modelled using [SVDS_AR()], [SVDS_AR1()], [SVDS_RW()],
#' or [SVDS_RW2()] priors, rather than `SVD_AR()`,
#' `SVDS_AR1()`, `SVDS_RW()`, or `SVDS_RW2()` priors.
#'
#' @section Mathematical details:
#' 
#' When the interaction being modelled only involves
#' age and time,
#' 
#' \deqn{\pmb{\beta}_t = \pmb{F} \pmb{\alpha}_t + \pmb{g},}
#'
#' and when it involives other variables besides age and time,
#'
#' \deqn{\pmb{\beta}_{u,t} = \pmb{F} \pmb{\alpha}_{u,t} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving age and time;
#' - \eqn{\pmb{\beta}_t} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the period \eqn{t};
#' - \eqn{\pmb{\beta}_{u,t}} is a subvector of \eqn{\pmb{\beta}_t} holding
#'   values for the  \eqn{u}th combination of the "by" variables;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{V} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{n} is `n_coef`;
#' - \eqn{\pmb{F}} is a known matrix with dimension \eqn{J \times n}
#'   if \eqn{\pmb{\beta}} is a main effect and dimension \eqn{V \times n}
#'   if \eqn{\pmb{\beta}} is an interaction; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements if
#'   if \eqn{\pmb{\beta}} is a main effect and \eqn{V}
#'   elements if \eqn{\pmb{\beta}} is an interaction.
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by performing an SVD and standardizing.
#'
#' With `SVD_AR()`, the prior for the elements of \eqn{\pmb{\alpha}_t} or \eqn{\pmb{\alpha}_{u,t}} is
#'
#' \deqn{\alpha_{kt} = \phi_1 \alpha_{k,t-1} + \cdots + \phi_n \beta_{k,t-n} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi_1 \alpha_{k,u,t-1} + \cdots + \phi_n \beta_{k,u,t-n} + \epsilon_{k,u,t}};
#'
#' with `SVD_AR1()`, it is
#'
#' \deqn{\alpha_{kt} = \phi \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi \alpha_{k,u,t-1} + \epsilon_{k,u,t}};
#'
#' with `SVD_RW()`, it is
#'
#' \deqn{\alpha_{kt} = \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \alpha_{k,u,t-1} + \epsilon_{k,u,t}};
#' 
#' and with `SVD_RW2()`, it is
#'
#' \deqn{\alpha_{kt} = 2 \alpha_{k,t-1} - \alpha_{k,t-2} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = 2 \alpha_{k,u,t-1} - \alpha_{k,u,t-2} + \epsilon_{k,u,t}}.
#'
#' For more on the \eqn{\phi} and \eqn{\epsilon}, see [AR()], [AR1()],
#' [RW()], and [RW2()].
#' 
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @inheritParams SVD
#' @param n_comp Number of components from scaled SVD
#' to use in modelling. The default is half
#' the number of components of `ssvd`.
#' @param n_coef Number of AR coefficients in `SVD_RW()`.
#' @param s Scale for standard deviations terms.
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient in `SVD_AR()`.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_svd_ar"`,
#' `"bage_prior_svd_rw"`, or `"bage_prior_svd_rw2"`.
#'
#' @seealso
#' - [SVDS()] SVD prior for non-time-varying age-sex or age-gender profile
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via random walk with drift
#' - [Sp()] Smoothing via splines
#' - [priors] Overview of priors implemented in **bage**
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the vignette
#'   [here](https://bayesiandemography.github.io/bage/articles/vig4_svd.html).
#'
#' @examples
#' SVD_AR1(HMD)
#' SVD_RW(HMD, n_comp = 3)
#' @export
SVD_AR <- function(ssvd, n_comp = NULL, n_coef = 2, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_n(n = n_coef,
          nm_n = "n_coef",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  check_scale(x = s,
              nm_x = "s",
              zero_ok = FALSE)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  new_bage_prior_svd_ar(ssvd = ssvd,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        n_coef = n_coef,
                        scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_AR1 <- function(ssvd, n_comp = NULL, min = 0.8, max = 0.98, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_min_max_ar(min = min, max = max)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  min <- as.double(min)
  max <- as.double(max)
  scale <- as.double(s)
  new_bage_prior_svd_ar1(ssvd = ssvd,
                         nm_ssvd = nm_ssvd,
                         n_comp = n_comp,
                         min = min,
                         max = max,
                         scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_RW <- function(ssvd, n_comp = NULL, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  new_bage_prior_svd_rw(ssvd = ssvd,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        scale = scale)
}

#' @rdname SVD_AR
#' @export
SVD_RW2 <- function(ssvd, n_comp = NULL, s = 1) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  scale <- as.double(s)
  new_bage_prior_svd_rw2(ssvd = ssvd,
                         nm_ssvd = nm_ssvd,
                         n_comp = n_comp,
                         scale = scale)
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
new_bage_prior_ar <- function(n_coef, scale, min, max, nm, along) {
  shape1 <- 2.0
  shape2 <- 2.0
  ans <- list(i_prior = 1L,
              const = c(shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale),
              specific = list(n_coef = n_coef,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              scale = scale,
                              along = along,
                              nm = nm))
  class(ans) <- c("bage_prior_ar", "bage_prior")
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
new_bage_prior_lin <- function(scale, sd_slope, along) {
    ans <- list(i_prior = 2L,
                const = c(scale = scale,
                          sd_slope = sd_slope),
                specific = list(scale = scale,
                                sd_slope = sd_slope,
                                along = along))
    class(ans) <- c("bage_prior_lin", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_linar <- function(n_coef, scale, sd_slope, min, max, along, nm) {
  shape1 <- 2.0
  shape2 <- 2.0
  ans <- list(i_prior = 3L,
              const = c(scale = scale,
                        sd_slope = sd_slope,
                        shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max),
              specific = list(n_coef = n_coef,
                              scale = scale,
                              sd_slope = sd_slope,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              along = along,
                              nm = nm))
  class(ans) <- c("bage_prior_linar", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_norm <- function(scale) {
    ans <- list(i_prior = 4L,
                const = c(scale = scale),
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_normfixed <- function(sd) {
    ans <- list(i_prior = 5L,
                const = c(sd = sd),
                specific = list(sd = sd))
    class(ans) <- c("bage_prior_normfixed", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw <- function(scale, along) {
    ans <- list(i_prior = 6L,
                const = c(scale = scale),
                specific = list(scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rwseasfix <- function(n_seas, scale, along) {
    ans <- list(i_prior = 10L,
                const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                          scale = scale),
                specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rwseasfix", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rwseasvary <- function(n_seas, scale_seas, scale, along) {
    ans <- list(i_prior = 11L,
                const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                          scale_seas = scale_seas,  
                          scale = scale),
                specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                                scale_seas = scale_seas,
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rwseasvary", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2 <- function(scale, along) {
    ans <- list(i_prior = 7L,
                const = c(scale = scale),
                specific = list(scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2seasfix <- function(n_seas, scale, along) {
    ans <- list(i_prior = 12L,
                const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                          scale = scale),
                specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rw2seasfix", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2seasvary <- function(n_seas, scale_seas, scale, along) {
    ans <- list(i_prior = 13L,
                const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                          scale_seas = scale_seas,  
                          scale = scale),
                specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                                scale_seas = scale_seas,
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_rw2seasvary", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_spline <- function(n_comp, scale, along) {
    ans <- list(i_prior = 8L,
                const = c(scale = scale),
                specific = list(n_comp = n_comp,
                                scale = scale,
                                along = along))
    class(ans) <- c("bage_prior_spline", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_svd <- function(ssvd, nm_ssvd, n_comp, joint) {
    ans <- list(i_prior = 9L,
                const = 0, ## not used
                specific = list(ssvd = ssvd,
                                nm_ssvd = nm_ssvd,
                                n_comp = n_comp,
                                joint = joint))
    class(ans) <- c("bage_prior_svd", "bage_prior")
    ans
}

