
## User-visible constructors --------------------------------------------------

## HAS_TESTS
#' Autoregressive Prior
#'
#' Use an autoregressive process to model
#' a main effect, or use multiple autoregressive
#' processes to model an interaction.
#' Typically used with time effects or with
#' interactions that involve time.
#'
#' If `AR()` is used with an interaction, then
#' separate AR processes are constructed along
#' the 'along' variable, within each combination of the
#' 'by' variables.
#'
#' By default, the autoregressive processes
#' have order 2. Alternative choices can be
#' specified through the `n_coef` argument.
#' 
#' Argument `s` controls the size of innovations.
#' Smaller values for `s` tend to give smoother estimates.
#'
#' @section Mathematical details:
#'
#' When `AR()` is used with a main effect,
#'
#' \deqn{\beta_j = \phi_1 \beta_{j-1} + \cdots + \phi_{\mathtt{n\_coef}} \beta_{j-\mathtt{n\_coef}} + \epsilon_j}
#' \deqn{\epsilon_j \sim \text{N}(0, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \phi_1 \beta_{u,v-1} + \cdots + \phi_{\mathtt{n\_coef}} \beta_{u,v-\mathtt{n\_coef}} + \epsilon_{u,v}}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Internally, `AR()` derives a value for \eqn{\omega} that
#' gives every element of \eqn{\beta} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#'
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2).}
#'
#' The correlation coefficients \eqn{\phi_1, \cdots, \phi_{\mathtt{n\_coef}}}
#' each have prior
#'
#' \deqn{\phi_k \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).}
#'
#' @section Constraints:
#'
#' With some combinations of terms and priors, the values of
#' the intercept, main effects, and interactions are
#' are only weakly identified.
#' For instance, it may be possible to increase the value of the
#' intercept and reduce the value of the remaining terms in
#' the model with no effect on predicted rates and only a tiny
#' effect on prior probabilities. This weak identifiability is
#' typically harmless. However, in some applications, such as
#' when trying to obtain interpretable values
#' for main effects and interactions, it can be helpful to increase
#' identifiability through the use of constraints, specified through the
#' `con` argument.
#'
#' Current options for `con` are:
#'
#' - `"none"` No constraints. The default.
#' - `"by"` Only used in interaction terms that include 'along' and
#'   'by' dimensions. Within each value of the 'along'
#'   dimension, terms across each 'by' dimension are constrained
#'   to sum to 0.
#' 
#' @param n_coef Number of lagged terms in the
#' model, ie the order of the model. Default is `2`.
#' @param s Scale for the prior for the innovations.
#' Default is `1`.
#' @param shape1,shape2 Parameters for beta-distribution prior
#' for coefficients. Defaults are `5` and `5`.
#' @param along Name of the variable to be used
#' as the 'along' variable. Only used with
#' interactions.
#' @param con Constraints on parameters.
#' Current choices are `"none"` and `"by"`.
#' Default is `"none"`. See below for details.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR1()] Special case of `AR()`. Can be more
#'   numerically stable than higher-order models.
#' - [Lin_AR()], [Lin_AR1()] Straight line with AR errors
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
AR <- function(n_coef = 2,
               s = 1,
               shape1 = 5,
               shape2 = 5,
               along = NULL,
               con = c("none", "by")) {
  poputils::check_n(n = n_coef,
                    nm_n = "n_coef",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  con <- match.arg(con)
  new_bage_prior_ar(n_coef = n_coef,
                    shape1 = shape1,
                    shape2 = shape2,
                    min = -1,
                    max = 1,
                    scale = scale,
                    along = along,
                    con = con,
                    nm = "AR")
}


## HAS_TESTS
#' Autoregressive Prior of Order 1
#'
#' Use an autoregressive process
#' of order 1 to model
#' a main effect, or use multiple AR1
#' processes to model an interaction.
#' Typically used with time effects or with
#' interactions that involve time.
#'
#' If `AR()` is used with an interaction,
#' separate AR processes are constructed along
#' the 'along' variable, within each combination of the
#' 'by' variables.
#'
#' Arguments `min` and `max` can be used to specify
#' the permissible range for autocorrelation.
#'
#' Argument `s` controls the size of innovations. Smaller values
#' for `s` tend to give smoother estimates.
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
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Internally, `AR1()` derives a value for \eqn{\omega} that
#' gives every element of \eqn{\beta} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2),}
#' where `s` is provided by the user.
#' 
#' Coefficient \eqn{\phi} is constrained
#' to lie between `min` and `max`.
#' Its prior distribution is
#' 
#' \deqn{\phi = (\mathtt{max} - \mathtt{min}) \phi' - \mathtt{min}}
#' 
#' where
#' 
#' \deqn{\phi' \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).}
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_ar"`.
#'
#' @seealso
#' - [AR()] Generalization of `AR1()`
#' - [Lin_AR()], [Lin_AR1()] Line with AR errors
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
AR1 <- function(s = 1,
                shape1 = 5,
                shape2 = 5,
                min = 0.8,
                max = 0.98,
                along = NULL,
                con = c("none", "by")) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  check_min_max_ar(min = min, max = max)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  min <- as.double(min)
  max <- as.double(max)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  con <- match.arg(con)
  new_bage_prior_ar(n_coef = 1L,
                    scale = scale,
                    shape1 = shape1,
                    shape2 = shape2,
                    min = min,
                    max = max,
                    along = along,
                    con = con,
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
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
#' Linear Prior with Independent Normal Errors
#'
#' Use a line or lines with independent
#' normal errors to model a main effect
#' or interaction. Typically used with time.
#'
#' If `Lin()` is used with an interaction,
#' then separate lines are constructed along 
#' the 'along' variable, within each combination
#' of the 'by' variables.
#' 
#' Argument `s` controls the size of the errors.
#' Smaller values give smoother estimates.
#' `s` can be zero, in which case errors are zero,
#' and all values lie exactly on straight lines.
#' This is clearly a simplification, but it allows
#' the prior to be used with very large
#' interactions.
#' 
#' Argument `sd_slope` controls the size of the slopes of
#' the lines. Larger values can give more steeply
#' sloped lines.
#' 
#' @section Mathematical details:
#'
#' When `Lin()` is used with a main effect,
#'
#' \deqn{\beta_j = (j - (J+1)/2) \eta + \epsilon_j}
#' \deqn{\eta \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}
#' \deqn{\epsilon_j \sim \text{N}(0, \tau^2),}
#' 
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = (v - (V + 1)/2) \eta_u + \epsilon_{u,v}}
#' \deqn{\eta_u \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}
#' \deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2),}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#' 
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2).}
#'
#' When \eqn{\mathtt{s} = 0}, the model reduces to
#'
#' \deqn{\beta_j = (j - (J+1)/2) \eta}
#' \deqn{\eta \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}
#'
#' or
#'
#' \deqn{\beta_{u,v} = (v = (V + 1)/2) \eta_u}
#' \deqn{\eta_u \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}.
#'
#' @inheritSection AR Constraints
#'
#' @inheritParams AR
#' @param s Scale for the prior for the errors.
#' Default is `1`. Can be `0`.
#' @param mean_slope Mean in prior for slope
#' of line. Default is 0.
#' @param sd_slope Standard deviation in prior for slope
#' of line. Default is 1.
#'
#' @returns An object of class `"bage_prior_lin"`.
#'
#' @seealso
#' - [Lin_AR()] Linear with AR errors
#' - [Lin_AR1()] Linear with AR1 errors
#' - [RW2()] Second-order random walk
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' Lin()
#' Lin(s = 0.5, sd_slope = 2)
#' Lin(s = 0)
#' Lin(along = "cohort")
#' @export
Lin <- function(s = 1,
                mean_slope = 0,
                sd_slope = 1,
                along = NULL,
                con = c("none", "by")) {
  check_scale(s, nm_x = "s", zero_ok = TRUE)
  check_number(mean_slope, nm_x = "mean_slope")
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  con <- match.arg(con)
  scale <- as.double(s)
  mean_slope <- as.double(mean_slope)
  sd_slope <- as.double(sd_slope)
  if (isTRUE(all.equal(scale, 0)))
    new_bage_prior_linex(mean_slope,
                         sd_slope = sd_slope,
                         along = along,
                         con = con)
  else
    new_bage_prior_lin(scale = scale,
                       mean_slope,
                       sd_slope = sd_slope,
                       along = along,
                       con = con)
}


## HAS_TESTS
#' Linear Prior with Autoregressive Errors
#'
#' Use a line or lines with autoregressive
#' errors to model a main effect
#' or interaction. Typically used with time.
#'
#' If `Lin_AR()` is used with an interaction,
#' separate lines are constructed along 
#' the 'along' variable, within each combination
#' of the 'by' variables.
#'
#' The order of the autoregressive errors is
#' controlled by the `n_coef` argument. The
#' default is 2.
#' 
#' Argument `s` controls the size of the innovations.
#' Smaller values tend to give smoother estimates.
#'
#' Argument `sd_slope` controls the slopes of
#' the lines. Larger values can give more steeply
#' sloped lines.
#' 
#' @section Mathematical details:
#'
#' When `Lin_AR()` is used with a main effect,
#'
#' \deqn{\beta_1 = \alpha + \epsilon_1}
#' \deqn{\beta_j = \alpha + (j - 1) \eta + \epsilon_j, \quad j > 1}
#' \deqn{\alpha \sim \text{N}(0, 1)}
#' \deqn{\epsilon_j = \phi_1 \epsilon_{j-1} + \cdots + \phi_{\mathtt{n\_coef}} \epsilon_{j-\mathtt{n\_coef}} + \varepsilon_j}
#' \deqn{\varepsilon_j \sim \text{N}(0, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,1} = \alpha_u + \epsilon_{u,1}}
#' \deqn{\beta_{u,v} = \eta (v - 1) + \epsilon_{u,v}, \quad v = 2, \cdots, V}
#' \deqn{\alpha_u \sim \text{N}(0, 1)}
#' \deqn{\epsilon_{u,v} = \phi_1 \epsilon_{u,v-1} + \cdots + \phi_{\mathtt{n\_coef}} \epsilon_{u,v-\mathtt{n\_coef}} + \varepsilon_{u,v},}
#' \deqn{\varepsilon_{u,v} \sim \text{N}(0, \omega^2).}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{u} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' The slopes have priors
#' \deqn{\eta \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}
#' and
#' \deqn{\eta_u \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2).}
#'
#' Internally, `Lin_AR()` derives a value for \eqn{\omega} that
#' gives \eqn{\epsilon_j} or \eqn{\epsilon_{u,v}} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2).}
#'
#' The correlation coefficients \eqn{\phi_1, \cdots, \phi_{\mathtt{n\_coef}}}
#' each have prior
#'
#' \deqn{0.5 \phi_k - 0.5 \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).}
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param s Scale for the innovations in the
#' AR process. Default is `1`.
#' @param mean_slope Mean in prior for slope
#' of line. Default is 0.
#' @param sd_slope Standard deviation in the prior for
#' the slope of the line. Larger values imply
#' steeper slopes. Default is 1.
#'
#' @returns An object of class `"bage_prior_linar"`.
#'
#' @seealso
#' - [Lin_AR1()] Special case of `Lin_AR()`
#' - [Lin()] Line with independent normal errors
#' - [AR()] AR process with no line
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' Lin_AR()
#' Lin_AR(n_coef = 3, s = 0.5, sd_slope = 2)
#' @export
Lin_AR <- function(n_coef = 2,
                   s = 1,
                   shape1 = 5,
                   shape2 = 5,
                   mean_slope = 0,
                   sd_slope = 1,
                   along = NULL,
                   con = c("none", "by")) {
  poputils::check_n(n = n_coef,
                    nm_n = "n_coef",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  check_number(mean_slope, nm_x = "mean_slope")
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  con <- match.arg(con)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  mean_slope <- as.double(mean_slope)
  sd_slope <- as.double(sd_slope)
  new_bage_prior_linar(n_coef = n_coef,
                       shape1 = shape1,
                       shape2 = shape2,
                       mean_slope = mean_slope,
                       sd_slope = sd_slope,
                       min = -1,
                       max = 1,
                       scale = scale,
                       along = along,
                       con = con,
                       nm = "Lin_AR")
}


## HAS_TESTS
#' Linear Prior with Autoregressive Errors of Order 1
#'
#' Use a line or lines with AR1
#' errors to model a main effect
#' or interaction. Typically used with time.
#'
#' If `Lin_AR1()` is used with an interaction,
#' separate lines are constructed along 
#' the 'along' variable, within each combination
#' of the 'by' variables.
#'
#' Arguments `min` and `max` can be used to specify
#' the permissible range for autocorrelation.
#' 
#' Argument `s` controls the size of the innovations.
#' Smaller values tend to give smoother estimates.
#'
#' Argument `sd_slope` controls the slopes of
#' the lines. Larger values can give more steeply
#' sloped lines.
#'
#' @section Mathematical details:
#'
#' When `Lin_AR1()` is being used with a main effect,
#'
#' \deqn{\beta_1 = \alpha + \epsilon_1}
#' \deqn{\beta_j = \alpha + (j - 1) \eta + \epsilon_j, \quad j > 1}
#' \deqn{\alpha \sim \text{N}(0, 1)}
#' \deqn{\epsilon_j = \phi \epsilon_{j-1} + \varepsilon_j}
#' \deqn{\varepsilon \sim \text{N}(0, \omega^2),}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,1} = \alpha_u + \epsilon_{u,1}}
#' \deqn{\beta_{u,v} = \eta (v - 1) + \epsilon_{u,v}, \quad v = 2, \cdots, V}
#' \deqn{\alpha_u \sim \text{N}(0, 1)}
#' \deqn{\epsilon_{u,v} = \phi \epsilon_{u,v-1} + \varepsilon_{u,v},}
#' \deqn{\varepsilon_{u,v} \sim \text{N}(0, \omega^2).}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{u} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' The slopes have priors
#' \deqn{\eta \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2)}
#' and
#' \deqn{\eta_u \sim \text{N}(\mathtt{mean\_slope}, \mathtt{sd\_slope}^2).}
#'
#' Internally, `Lin_AR1()` derives a value for \eqn{\omega} that
#' gives \eqn{\epsilon_j} or \eqn{\epsilon_{u,v}} a marginal
#' variance of \eqn{\tau^2}. Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2),}
#' where a value for `s` is provided by the user.
#'
#' Coefficient \eqn{\phi} is constrained
#' to lie between `min` and `max`.
#' Its prior distribution is
#' \deqn{\phi = (\mathtt{max} - \mathtt{min}) \phi' - \mathtt{min}}
#' where
#' \deqn{\phi' \sim \text{Beta}(\mathtt{shape1}, \mathtt{shape2}).}
#' 
#' @inheritSection AR Constraints
#'
#' @inheritParams Lin_AR
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient.
#' Defaults are `0.8` and `0.98`.
#'
#' @returns An object of class `"bage_prior_linar"`.
#'
#' @seealso
#' - [Lin_AR()] Generalization of `Lin_AR1()`
#' - [Lin()] Line with independent normal errors
#' - [AR1()] AR1 process with no line
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @references
#' - The defaults for `min` and `max` are based on the
#'   defaults for `forecast::ets()`.
#'
#' @examples
#' Lin_AR1()
#' Lin_AR1(min = 0, s = 0.5, sd_slope = 2)
#' @export
Lin_AR1 <- function(s = 1,
                    shape1 = 5,
                    shape2 = 5,
                    min = 0.8,
                    max = 0.98,
                    mean_slope = 0,
                    sd_slope = 1,
                    along = NULL,
                    con = c("none", "by")) {
  check_min_max_ar(min = min, max = max)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  check_number(mean_slope, nm_x = "mean_slope")
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  con <- match.arg(con)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  min <- as.double(min)
  max <- as.double(max)
  mean_slope <- as.double(mean_slope)
  sd_slope <- as.double(sd_slope)
  new_bage_prior_linar(n_coef = 1L,
                       scale = scale,
                       shape1 = shape1,
                       shape2 = shape2,
                       min = min,
                       max = max,
                       mean_slope = mean_slope,
                       sd_slope = sd_slope,
                       along = along,
                       con = con,
                       nm = "Lin_AR1")
}


## HAS_TESTS
#' Normal Prior
#'
#' Use independent draws from a normal
#' distribution to model a main effect or interaction.
#' Typically used with variables other than
#' age or time, such as region or ethnicity,
#' where there is no natural ordering. 
#'
#' Argument `s` controls the size of errors. Smaller values
#' for `s` tend to give more tightly clustered estimates.
#'
#' @section Mathematical details:
#'
#' \deqn{\beta_j \sim \text{N}(0, \tau^2)}
#'
#' where \eqn{\beta} is the main effect or interaction.
#' 
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2),}
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
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
#' variance is treated as fixed and known. Typically
#' used for main effects or interactions where there
#' are too few elements to reliably estimate variance
#' from the available data.
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
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
#' Use a random walk as a model for
#' a main effect, or use multiple random walks
#' as a model for an interaction.
#' Typically used with terms that involve age or time.
#'
#' If `RW2()` is used with an interaction,
#' a separate random walk is constructed
#' within each combination of the
#' 'by' variables.
#'
#' Argument `s` controls the size of innovations.
#' Smaller values for `s` tend to produce smoother series.
#'
#' Argument `sd` controls variance in
#' initial values. Setting `sd` to `0` fixes initial
#' values at 0.
#' 
#' @section Mathematical details:
#'
#' When `RW()` is used with a main effect,
#'
#' \deqn{\beta_1 \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\beta_j \sim \text{N}(\beta_{j-1}, \tau^2), \quad j > 1}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,1} \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\beta_{u,v} \sim \text{N}(\beta_{u,v-1}, \tau^2), \quad v > 1}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2),}
#' where `s` is provided by the user.
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param sd Standard deviation
#' of initial value. Default is `1`.
#' Can be `0`.
#'
#'
#' @returns An object of class `"bage_prior_rwrandom"`
#' or `"bage_prior_rwzero"`.
#'
#' @seealso
#' - [RW_Seas()] Random walk with seasonal effect
#' - [RW2()] Second-order random walk
#' - [AR()] Autoregressive with order k
#' - [AR1()] Autoregressive with order 1
#' - [Sp()] Smoothing via splines
#' - [SVD()] Smoothing over age using singular value decomposition
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' RW()
#' RW(s = 0.5)
#' RW(sd = 0)
#' RW(along = "cohort")
#' @export
RW <- function(s = 1,
               sd = 1,
               along = NULL,
               con = c("none", "by")) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = TRUE)
  scale <- as.double(s)
  sd <- as.double(sd)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  con <- match.arg(con)
  if (sd > 0)
    new_bage_prior_rwrandom(scale = scale,
                            sd = sd,
                            along = along,
                            con = con)
  else
    new_bage_prior_rwzero(scale = scale,
                          along = along,
                          con = con)
}


## HAS_TESTS
#' Random Walk Prior with Seasonal Effect
#'
#' Use a random walk with seasonal effects as a model for
#' a main effect, or use multiple random walks,
#' each with their own seasonal effects,
#' as a model for an interaction.
#' Typically used with terms that involve time.
#'
#' If `RW_Seas()` is used with an interaction,
#' a separate series is constructed
#' within each combination of the
#' 'by' variables.
#'
#' Argument `s` controls the size of innovations in the random walk.
#' Smaller values for `s` tend to produce smoother series.
#'
#' Argument `sd` controls variance in
#' initial values of the random walk. `sd` can be `0`.
#'
#' Argument `n_seas` controls the number of seasons. 
#' When using quarterly data, for instance,
#' `n_seas` should be `4`.
#'
#' By default, the magnitude of seasonal effects
#' is fixed. However, setting `s_seas` to a value
#' greater than zero produces seasonal effects
#' that evolve over time.
#'
#' @section Mathematical details:
#'
#' When `RW_Seas()` is used with a main effect,
#'
#' \deqn{\beta_j = \alpha_j + \lambda_j, \quad j = 1, \cdots, J}
#' \deqn{\alpha_1 \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\alpha_j \sim \text{N}(\alpha_{j-1}, \tau^2), \quad j = 2, \cdots, J}
#' \deqn{\lambda_j \sim \text{N}(0, \mathtt{sd\_seas}^2), \quad j = 1, \cdots, \mathtt{n\_seas} - 1}
#' \deqn{\lambda_j = -\sum_{s=1}^{\mathtt{n\_seas} - 1} \lambda_{j - s}, \quad j = \mathtt{n\_seas}, 2 \mathtt{n\_seas}, \cdots}
#' \deqn{\lambda_j \sim \text{N}(\lambda_{j-\mathtt{n\_seas}}, \omega^2), \quad \text{otherwise},}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \alpha_{u,v} + \lambda_{u,v}, \quad v = 1, \cdots, V}
#' \deqn{\alpha_{u,1} \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\alpha_{u,v} \sim \text{N}(\alpha_{u,v-1}, \tau^2), \quad v = 2, \cdots, V}
#' \deqn{\lambda_{u,v} \sim \text{N}(0, \mathtt{sd\_seas}^2), \quad v = 1, \cdots, \mathtt{n\_seas} - 1}
#' \deqn{\lambda_{u,v} = -\sum_{s=1}^{\mathtt{n\_seas} - 1} \lambda_{u,v - s}, \quad v = \mathtt{n\_seas}, 2 \mathtt{n\_seas}, \cdots}
#' \deqn{\lambda_{u,v} \sim \text{N}(\lambda_{u,v-\mathtt{n\_seas}}, \omega^2), \quad \text{otherwise},}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{\alpha_j} or \eqn{\alpha_{u,v}} is an element of the random walk;
#' - \eqn{\lambda_j} or \eqn{\lambda_{u,v}} is an element of the seasonal effect;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Parameter \eqn{\omega} has a half-normal prior
#' \deqn{\omega \sim \text{N}^+(0, \mathtt{s\_seas}^2).}
#' If `s_seas` is set to 0, then \eqn{\omega} is 0,
#' and seasonal effects are time-invariant.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2).}
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param n_seas Number of seasons
#' @param s Scale for prior for innovations in
#' random walk. Default is `1`.
#' @param sd Standard deviation
#' of initial value. Default is `1`.
#' Can be `0`.
#' @param s_seas Scale for innovations
#' in seasonal effects. Default is `0`.
#' @param sd_seas Standard deviation for
#' initial values of seasonal effects.
#' Default is `1`.
#'
#' @returns Object of class
#' `"bage_prior_rwrandomseasvary"`,
#' `"bage_prior_rwrandomseasfix"`,
#' `"bage_prior_rwzeroseasvary"`, or
#' `"bage_prior_rwzeroseasfix"`.
#'
#' @seealso
#' - [RW()] Random walk without seasonal effect
#' - [RW2_Seas()] Second-order random walk with seasonal effect
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' RW_Seas(n_seas = 4)               ## seasonal effects fixed
#' RW_Seas(n_seas = 4, s_seas = 0.5) ## seasonal effects evolve
#' RW_Seas(n_seas = 4, sd = 0)       ## first term in random walk fixed at 0
#' @export
RW_Seas <- function(n_seas,
                    s = 1,
                    sd = 1,
                    s_seas = 0,
                    sd_seas = 1,
                    along = NULL,
                    con = c("none", "by")) {
  poputils::check_n(n = n_seas,
                    nm_n = "n_seas",
                    min = 2L,
                    max = NULL,
                    divisible_by = NULL)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = TRUE)
  check_scale(s_seas, nm_x = "s_seas", zero_ok = TRUE)
  check_scale(sd_seas, nm_x = "sd_seas", zero_ok = FALSE)
  n_seas <- as.integer(n_seas)
  scale <- as.double(s)
  sd <- as.double(sd)
  scale_seas = as.double(s_seas)
  sd_seas <- as.double(sd_seas)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  con <- match.arg(con)
  if (scale_seas > 0) {
    if (sd > 0)
      new_bage_prior_rwrandomseasvary(n_seas = n_seas,
                                      scale_seas = scale_seas,
                                      sd_seas = sd_seas,
                                      scale = scale,
                                      sd = sd,
                                      along = along,
                                      con = con)
    else
      new_bage_prior_rwzeroseasvary(n_seas = n_seas,
                                    scale_seas = scale_seas,
                                    sd_seas = sd_seas,
                                    scale = scale,
                                    along = along,
                                    con = con)
  }
  else {
    if (sd > 0)
      new_bage_prior_rwrandomseasfix(n_seas = n_seas,
                                     sd_seas = sd_seas,
                                     scale = scale,
                                     sd = sd,
                                     along = along,
                                     con = con)
    else
      new_bage_prior_rwzeroseasfix(n_seas = n_seas,
                                   sd_seas = sd_seas,
                                   scale = scale,
                                   along = along,
                                   con = con)
  }
}


## HAS_TESTS
#' Second-Order Random Walk Prior
#'
#' Use a second-order random walk as a model for
#' a main effect, or use multiple second-order random walks
#' as a model for an interaction.
#' A second-order random walk is
#' a random walk with drift where the
#' drift term varies. It is typically
#' used with terms that involve age or time,
#' where there are sustained
#' trends upward or downward.
#'
#' If `RW2()` is used with an interaction,
#' a separate random walk is constructed
#' within each combination of the
#' 'by' variables.
#'
#' Argument `s` controls the size of innovations.
#' Smaller values for `s` tend to give smoother series.
#'
#' Argument `sd` controls variance in
#' initial values. Setting `sd` to `0` fixes
#' initial values at `0`.
#'
#' Argument `sd_slope` controls variance in the
#' initial slope.
#'
#' @section Mathematical details:
#'
#' When `RW2()` is used with a main effect,
#'
#' \deqn{\beta_1 \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\beta_2 \sim \text{N}(\beta_1, \mathtt{sd\_slope}^2)} 
#' \deqn{\beta_j \sim \text{N}(2 \beta_{j-1} - \beta_{j-2}, \tau^2), \quad j = 2, \cdots, J}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,1} \sim \text{N}(0, \mathtt{sd}^2)} 
#' \deqn{\beta_{u,2} \sim \text{N}(\beta_{u,1}, \mathtt{sd\_slope}^2)} 
#' \deqn{\beta_{u,v} \sim \text{N}(2\beta_{u,v-1} - \beta_{u,v-2}, \tau^2), \quad v = 3, \cdots, V}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Parameter \eqn{\tau}
#' has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2)}.
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param sd Standard deviation
#' of initial value. Default is `1`. Can be `0`.
#' @param sd_slope Standard deviation
#' of initial slope. Default is `1`.
#'
#' @returns An object of class `"bage_prior_rw2random"`
#' or `"bage_prior_rw2zero"`.
#'
#' @seealso
#' - [RW()] Random walk
#' - [RW2_Seas()] Second order random walk with seasonal effect
#' - [AR()] Autoregressive with order k
#' - [AR1()] Autoregressive with order 1
#' - [Sp()] Smoothing via splines
#' - [SVD()] Smoothing over age via singular value decomposition
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' RW2()
#' RW2(s = 0.5)
#' @export
RW2 <- function(s = 1,
                sd = 1,
                sd_slope = 1,
                along = NULL,
                con = c("none", "by")) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = TRUE)
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  con <- match.arg(con)
  scale <- as.double(s)
  sd <- as.double(sd)
  sd_slope <- as.double(sd_slope)
  if (sd > 0)
    new_bage_prior_rw2random(scale = scale,
                             sd = sd,
                             sd_slope = sd_slope,
                             along = along,
                             con = con)
  else
    new_bage_prior_rw2zero(scale = scale,
                           sd_slope = sd_slope,
                           along = along,
                           con = con)
}


## HAS_TESTS
#' Second-Order Random Walk Prior with 'Infant' Indicator
#'
#' Use a second-order random walk to model variation
#' over age, with an indicator variable for the first age group.
#' Designed for use in models of mortality rates.
#' 
#' A second-order random walk prior [RW2()]
#' works well for smoothing
#' mortality rates over age, except at age 0, where there
#' is a sudden jump in rates, reflecting the
#' special risks of infancy. The `RW2_Infant()`
#' extends the [RW2()] prior by adding an indicator
#' variable for the first age group.
#'
#' If `RW2_Infant()` is used in an interaction,
#' the 'along' dimension is always age, implying that
#' there is a separate random walk along age within each
#' combination of the 'by' variables.
#'
#' Argument `s` controls the size of innovations in the random walk.
#' Smaller values for `s` tend to give smoother series.
#'
#' Argument `sd` controls the sl size of innovations in the random walk.
#' Smaller values for `s` tend to give smoother series.
#'
#' @section Mathematical details:
#'
#' When `RW2_Infant()` is used with a main effect,
#'
#' \deqn{\beta_1 \sim \text{N}(0, 1)}
#' \deqn{\beta_2 \sim \text{N}(0, \mathtt{sd\_slope}^2)}
#' \deqn{\beta_3 \sim \text{N}(2 \beta_2, \tau^2)}
#' \deqn{\beta_j \sim \text{N}(2 \beta_{j-1} - \beta_{j-2}, \tau^2), \quad j = 3, \cdots, J}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,1} \sim \text{N}(0, 1)}
#' \deqn{\beta_{u,2} \sim \text{N}(0, \mathtt{sd\_slope}^2)}
#' \deqn{\beta_{u,3} \sim \text{N}(2 \beta_{u,2}, \tau^2)}
#' \deqn{\beta_{u,v} \sim \text{N}(2 \beta_{u,v-1} - \beta_{u,v-2}, \tau^2), \quad v = 3, \cdots, V}
#' 
#' where
#' - \eqn{\pmb{\beta}} is a main effect or interaction;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2)}.
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams RW2
#' 
#' @param sd_slope Standard deviation
#' for initial slope of random walk. Default is `1`.
#'
#' @returns Object of class `"bage_prior_rw2infant"`.
#'
#' @seealso
#' - [RW2()] Second-order random walk, without infant indicator
#' - [Sp()] Smoothing via splines
#' - [SVD()] Smoothing over age via singular value decomposition
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' RW2_Infant()
#' RW2_Infant(s = 0.1)
#' @export
RW2_Infant <- function(s = 1,
                       sd_slope = 1,
                       con = c("none", "by")) {
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  scale <- as.double(s)
  sd_slope <- as.double(sd_slope)
  con <- match.arg(con)
  new_bage_prior_rw2infant(scale = scale,
                           sd_slope = sd_slope,
                           con = con)
}


## HAS_TESTS
#' Second-Order Random Walk Prior with Seasonal Effect
#'
#' Use a second-oder random walk with
#' seasonal effects as a model for
#' a main effect, or use multiple second-order random walks,
#' each with their own seasonal effects,
#' as a model for an interaction.
#' Typically used with temrs that involve time.
#'
#' If `RW2_Seas()` is used with an interaction,
#' a separate series is constructed within each
#' combination of the 'by' variables.
#'
#' Argument `s` controls the size of innovations in the random walk.
#' Smaller values for `s` tend to produce smoother series.
#'
#' Argument `n_seas` controls the number of seasons. 
#' When using quarterly data, for instance,
#' `n_seas` should be `4`.
#'
#' By default, the magnitude of seasonal effects
#' is fixed. However, setting `s_seas` to a value
#' greater than zero produces seasonal effects
#' that evolve over time.
#'
#' @section Mathematical details:
#'
#' When `RW2_Seas()` is used with a main effect,
#'
#' \deqn{\beta_j = \alpha_j + \lambda_j, \quad j = 1, \cdots, J}
#' \deqn{\alpha_1 \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\alpha_2 \sim \text{N}(0, \mathtt{sd\_slope}^2)}
#' \deqn{\alpha_j \sim \text{N}(2 \alpha_{j-1} - \alpha_{j-2}, \tau^2), \quad j = 3, \cdots, J}
#' \deqn{\lambda_j \sim \text{N}(0, \mathtt{sd\_seas}^2), \quad j = 1, \cdots, \mathtt{n\_seas} - 1}
#' \deqn{\lambda_j = -\sum_{s=1}^{\mathtt{n\_seas} - 1} \lambda_{j - s}, \quad j = \mathtt{n\_seas}, 2 \mathtt{n\_seas}, \cdots}
#' \deqn{\lambda_j \sim \text{N}(\lambda_{j-\mathtt{n\_seas}}, \omega^2), \quad \text{otherwise},}
#'
#' and when it is used with an interaction,
#'
#' \deqn{\beta_{u,v} = \alpha_{u,v} + \lambda_{u,v}, \quad v = 1, \cdots, V}
#' \deqn{\alpha_{u,1} \sim \text{N}(0, \mathtt{sd}^2)}
#' \deqn{\alpha_{u,2} \sim \text{N}(0, \mathtt{sd\_slope}^2)}
#' \deqn{\alpha_{u,v} \sim \text{N}(2 \alpha_{u,v-1} - \alpha_{u,v-2}, \tau^2), \quad v = 3, \cdots, V}
#' \deqn{\lambda_{u,v} \sim \text{N}(0, \mathtt{sd\_seas}^2), \quad v = 1, \cdots, \mathtt{n\_seas} - 1}
#' \deqn{\lambda_{u,v} = -\sum_{s=1}^{\mathtt{n\_seas} - 1} \lambda_{u,v - s}, \quad v = \mathtt{n\_seas}, 2 \mathtt{n\_seas}, \cdots}
#' \deqn{\lambda_{u,v} \sim \text{N}(\lambda_{u,v-\mathtt{n\_seas}}, \omega^2), \quad \text{otherwise},}
#' 
#' where
#' - \eqn{\pmb{\beta}} is the main effect or interaction;
#' - \eqn{\alpha_j} or \eqn{\alpha_{u,v}} is an element of the random walk;
#' - \eqn{\lambda_j} or \eqn{\lambda_{u,v}} is an element of the seasonal effect;
#' - \eqn{j} denotes position within the main effect;
#' - \eqn{v} denotes position within the 'along' variable of the interaction; and
#' - \eqn{u} denotes position within the 'by' variable(s) of the interaction.
#'
#' Parameter \eqn{\omega} has a half-normal prior
#' \deqn{\omega \sim \text{N}^+(0, \mathtt{s\_seas}^2)}.
#' If `s_seas` is set to 0, then \eqn{\omega} is 0,
#' and the seasonal effects are fixed over time.
#'
#' Parameter \eqn{\tau} has a half-normal prior
#' \deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2)}.
#' 
#' @inheritSection AR Constraints
#'
#' @inheritParams RW_Seas
#' 
#' @param sd_slope Standard deviation
#' for initial slope of random walk. Default is `1`.
#'
#' @returns Object of class
#' `"bage_prior_rw2randomseasvary"`,
#' `"bage_prior_rw2randomseasfix"`,
#' `"bage_prior_rw2zeroseasvary"`, or
#' `"bage_prior_rw2zeroseasfix"`.
#'
#' @seealso
#' - [RW2()] Second-order random walk without seasonal effect
#' - [RW_Seas()] Random walk with seasonal effect
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' RW2_Seas(n_seas = 4)               ## seasonal effects fixed
#' RW2_Seas(n_seas = 4, s_seas = 0.5) ## seasonal effects evolve
#' RW2_Seas(n_seas = 4, sd = 0)       ## first term in random walk fixed at 0
#' @export
RW2_Seas <- function(n_seas,
                     s = 1,
                     sd = 1,
                     sd_slope = 1,
                     s_seas = 0,
                     sd_seas = 1,
                     along = NULL,
                     con = c("none", "by")) {
  poputils::check_n(n = n_seas,
                    nm_n = "n_seas",
                    min = 2L,
                    max = NULL,
                    divisible_by = NULL)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = TRUE)
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  check_scale(s_seas, nm_x = "s_seas", zero_ok = TRUE)
  check_scale(sd_seas, nm_x = "sd_seas", zero_ok = FALSE)
  n_seas <- as.integer(n_seas)
  scale <- as.double(s)
  sd <- as.double(sd)
  sd_slope <- as.double(sd_slope)
  scale_seas = as.double(s_seas)
  sd_seas = as.double(sd_seas)
  if (!is.null(along))
    check_string(along, nm_x = "along")
  con <- match.arg(con)
  if (scale_seas > 0) {
    if (sd > 0) 
      new_bage_prior_rw2randomseasvary(n_seas = n_seas,
                                       scale_seas = scale_seas,
                                       sd_seas = sd_seas,
                                       scale = scale,
                                       sd = sd,
                                       sd_slope = sd_slope,
                                       along = along,
                                       con = con)
    else
      new_bage_prior_rw2zeroseasvary(n_seas = n_seas,
                                     scale_seas = scale_seas,
                                     sd_seas = sd_seas,
                                     scale = scale,
                                     sd_slope = sd_slope,
                                     along = along,
                                     con = con)
  }
  else {
    if (sd > 0) 
      new_bage_prior_rw2randomseasfix(n_seas = n_seas,
                                      sd_seas = sd_seas,
                                      scale = scale,
                                      sd = sd,
                                      sd_slope = sd_slope,
                                      along = along,
                                      con = con)
    else
      new_bage_prior_rw2zeroseasfix(n_seas = n_seas,
                                    sd_seas = sd_seas,
                                    scale = scale,
                                    sd_slope = sd_slope,
                                    along = along,
                                    con = con)
  }    
}


## HAS_TESTS
#' P-Spline Prior
#'
#' Use a p-spline (penalised spline) to model main
#' effects or interactions. Typically used with age,
#' but can be used with any variable where outcomes are
#' expected to vary smoothly from one element to the next.
#'
#' If `Sp()` is used with an interaction,
#' separate splines are used for the 'along' variable within
#' each combination of the
#' 'by' variables.
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
#' - \eqn{\pmb{\beta}_u} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the  \eqn{u}th combination of the 'by' variables;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{U} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{X} is a \eqn{J \times n} or \eqn{V \times n} matrix of
#'   spline basis functions; and
#' - \eqn{n} is `n_comp`.
#'
#' The elements of \eqn{\pmb{\alpha}} or \eqn{\pmb{\alpha}_u} are assumed
#' to follow a [second-order random walk][RW2()].
#'
#' @inheritSection AR Constraints
#' 
#' @inheritParams AR
#' @param sd Standard deviation in prior for first
#' element of random walk.
#' @param sd_slope Standard deviation in prior
#' for initial slope of random walk. Default is `1`.
#' @param n_comp Number of spline basis functions (components)
#' to use.
#'
#' @returns An object of class `"bage_prior_spline"`.
#'
#' @seealso
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via second-order random walk
#' - [SVD()] Smoothing of age via singular value decomposition
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [splines::bs()] Function used by \pkg{bage} to construct
#'   spline basis functions
#' - [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
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
Sp <- function(n_comp = NULL,
               s = 1,
               sd = 1,
               sd_slope = 1,
               along = NULL,
               con = c("none", "by")) {
  if (!is.null(n_comp)) {
    poputils::check_n(n = n_comp,
                      nm_n = "n_comp",
                      min = 4L,
                      max = NULL,
                      divisible_by = NULL)
    n_comp <- as.integer(n_comp)
  }
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = FALSE)
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  scale <- as.double(s)
  sd <- as.double(sd)
  sd_slope <- as.double(sd_slope)
  if (!is.null(along))
    check_string(x = along, nm_x = "along")
  con <- match.arg(con)
  new_bage_prior_spline(n_comp = n_comp,
                        scale = scale,
                        sd = sd, 
                        sd_slope = sd_slope,
                        along = along,
                        con = con)
}


## HAS_TESTS
#' SVD-Based Prior for Age or Age-Sex Profiles
#'
#' Use components from a Singular Value Decomposition (SVD)
#' to model a main effect or interaction involving age.
#'
#' A `SVD()` prior assumes that the age, age-sex, or age-gender
#' profiles for the quantity
#' being modelled looks like they were drawn at random
#' from an external demographic database. For instance,
#' the prior obtained via
#' ```
#' SVD(HMD)
#' ```
#' assumes that age or age-sex profiles look like
#' they were drawn from the
#' [Human Mortality Database](https://www.mortality.org).
#'
#' If `SVD()` is used with an interaction involving
#' variables other than age and sex/gender,
#' separate profiles are constructed
#' within each combination of other variables.
#'
#' \pkg{bage} chooses the appropriate age-specific
#' or age-sex-specific SVD values internally.
#' The choice depends on the model term that the
#' `SVD()` prior is applied to, and on the
#' age labels used in `data` argument to
#' [mod_pois()], [mod_binom()] or [mod_norm()].
#' \pkg{bage} makes its choice when [set_prior()]
#' is called.
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
#' argument  to `FALSE`. Option 1
#' is obtained by setting the `indep` argument to `TRUE`.
#' The default is `TRUE`.
#' 
#' @section Mathematical details:
#'
#' **Case 1: Term involving age and no other variables**
#' 
#' When `SVD()` is used with an age main effect,
#' 
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is a main effect or interaction involving age;
#' - \eqn{J} is the number of elements of \eqn{\pmb{\beta}};
#' - \eqn{n} is the number of components from the SVD;
#' - \eqn{\pmb{F}} is a known matrix with dimension \eqn{J \times n}; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{J} elements.
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by performing an SVD and standardizing.
#'
#' The elements of \eqn{\pmb{\alpha}} have prior
#' \deqn{\alpha_k \sim \text{N}(0, 1), \quad k = 1, \cdots, K.}
#'
#' **Case 2: Term involving age and non-sex, non-gender variable(s)**
#'
#' When `SVD()` is used with an interaction that involves age but that
#' does not involve sex or gender,
#'
#' \deqn{\pmb{\beta}_u = \pmb{F} \pmb{\alpha}_u + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}_u} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for the  \eqn{u}th combination of the non-age variables;
#' - \eqn{V} is the number of elements of \eqn{\pmb{\beta}_u};
#' - \eqn{n} is the number of components from the SVD;
#' - \eqn{\pmb{F}} is a known matrix with dimension \eqn{V \times n}; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{V} elements.
#' 
#' **Case 3: Term involving age, sex/gender, and no other variables**
#'
#' When `SVD()` is used with an interaction that involves
#' age and sex or gender, there are two sub-cases, depending on
#' the value of `indep`.
#'
#' When `indep` is `TRUE`,
#' 
#' \deqn{\pmb{\beta}_{s} = \pmb{F}_s \pmb{\alpha}_{s} + \pmb{g}_s,}
#'
#' and when `indep` is `FALSE`,
#'
#' \deqn{\pmb{\beta} = \pmb{F} \pmb{\alpha} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving age and sex/gender;
#' - \eqn{\pmb{\beta}_{s}} is a subvector of \eqn{\pmb{\beta}},
#'   holding values for sex/gender \eqn{s};
#' - \eqn{J} is the number of elements in \eqn{\pmb{\beta}};
#' - \eqn{S} is the number of sexes/genders;
#' - \eqn{n} is the number of components from the SVD;
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
#' **Case 4: Term involving age, sex/gender, and other variable(s)**
#'
#' When `SVD()` is used with an interaction that involves
#' age, sex or gender, and other variables, there are two sub-cases,
#' depending on the value of `indep`.
#'
#' When `indep` is `TRUE`,
#' 
#' \deqn{\pmb{\beta}_{u,s} = \pmb{F}_s \pmb{\alpha}_{u,s} + \pmb{g}_s,}
#'
#' and when `indep` is `FALSE`,
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
#' - \eqn{n} is the number of components from the SVD;
#' - \eqn{\pmb{F}_s} is a known \eqn{(V/S) \times n} matrix, specific
#'   to sex/gender \eqn{s};
#' - \eqn{\pmb{g}_s} is a known vector with \eqn{V/S} elements,
#'   specific to sex/gender \eqn{s};
#' - \eqn{\pmb{F}} is a known \eqn{V \times n} matrix, with values
#'   for all sexes/genders; and
#' - \eqn{\pmb{g}} is a known vector with \eqn{V} elements, with values
#'   for all sexes/genders.
#' 
#' @section Scaled SVDs of demographic databases in bage:
#'
#' - \code{\link{HMD}} Mortality rates from the
#' [Human Mortality Database](https://www.mortality.org).
#' - \code{\link{HFD}} Fertility rates from the
#' [Human Fertility Database](https://www.humanfertility.org).
#' - \code{\link{LFP}} Labor forcce participation
#' rates from the [OECD](https://data-explorer.oecd.org).
#'
#' @param ssvd Object of class `"bage_ssvd"`
#' holding a scaled SVD. See below for scaled SVDs
#' of databases currently available in \pkg{bage}.
#' @param v Version of scaled SVD components
#' to use. If no value is suppled, the most
#' recent version is used.
#' @param n_comp Number of components from scaled SVD
#' to use in modelling. The default is half
#' the number of components of `ssvd`.
#' @param indep Whether to use separate or
#' combined SVDs in terms involving sex or gender.
#' Default is `TRUE`.
#' See below for details.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @seealso
#' - [SVD_AR()], [SVD_AR1()], [SVD_RW()], [SVD_RW2()] SVD priors for
#'   for time-varying age profiles;
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via second-order random walk
#' - [Sp()] Smoothing via splines
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [set_var_sexgender()] Identify sex or gender variable in data
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the
#'   [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' SVD(HMD) 
#' SVD(HMD, n_comp = 3)
#' @export
SVD <- function(ssvd,
                v = NULL,
                n_comp = NULL,
                indep = TRUE) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_v_ssvd(v = v, ssvd = ssvd, nm_ssvd = nm_ssvd)
  n_comp <- n_comp_svd(n_comp = n_comp, nm_n_comp = "n_comp", ssvd = ssvd)
  check_flag(x = indep, nm_x = "indep")
  new_bage_prior_svd(ssvd = ssvd,
                     nm_ssvd = nm_ssvd,
                     v = v,
                     n_comp = n_comp,
                     indep = indep)
}


## HAS_TESTS
#' Dynamic SVD-Based Priors for Age Profiles or Age-Sex Profiles
#'
#' Use components from a Singular Value Decomposition (SVD)
#' to model an interaction involving age and time, or age,
#' sex/gender and time, where the coefficients evolve over time.
#'
#' `SVD_AR()`, `SVD_AR1()`, `SVD_RW()`, and `SVD_RW2()`
#' priors assume that, in any given period,
#' the age profiles or age-sex profiles for the quantity
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
#'
#' @section Mathematical details:
#' 
#' When the interaction being modelled only involves
#' age and time, or age, sex/gender, and time
#' 
#' \deqn{\pmb{\beta}_t = \pmb{F} \pmb{\alpha}_t + \pmb{g},}
#'
#' and when it involves other variables besides age, sex/gender, and time,
#'
#' \deqn{\pmb{\beta}_{u,t} = \pmb{F} \pmb{\alpha}_{u,t} + \pmb{g},}
#'
#' where
#' - \eqn{\pmb{\beta}} is an interaction involving age, time, possibly sex/gender,
#'   and possibly other variables;
#' - \eqn{\pmb{\beta}_t} is a subvector of \eqn{\pmb{\beta}} holding
#'   values for period \eqn{t};
#' - \eqn{\pmb{\beta}_{u,t}} is a subvector of \eqn{\pmb{\beta}_t} holding
#'   values for the  \eqn{u}th combination of the non-age, non-time,
#'   non-sex/gender variables for period \eqn{t};
#' - \eqn{\pmb{F}} is a known matrix; and
#' - \eqn{\pmb{g}} is a known vector.
#' 
#' \eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
#' a large database of age-specific demographic estimates
#' by applying the singular value decomposition, and then standardizing.
#'
#' With `SVD_AR()`, the prior for the \eqn{k}th element
#' of \eqn{\pmb{\alpha}_t} or \eqn{\pmb{\alpha}_{u,t}} is
#'
#' \deqn{\alpha_{k,t} = \phi_1 \alpha_{k,t-1} + \cdots + \phi_n \beta_{k,t-n} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi_1 \alpha_{k,u,t-1} + \cdots + \phi_n \beta_{k,u,t-n} + \epsilon_{k,u,t};}
#'
#' with `SVD_AR1()`, it is
#'
#' \deqn{\alpha_{k,t} = \phi \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \phi \alpha_{k,u,t-1} + \epsilon_{k,u,t};}
#'
#' with `SVD_RW()`, it is
#'
#' \deqn{\alpha_{k,t} = \alpha_{k,t-1} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = \alpha_{k,u,t-1} + \epsilon_{k,u,t};}
#' 
#' and with `SVD_RW2()`, it is
#'
#' \deqn{\alpha_{k,t} = 2 \alpha_{k,t-1} - \alpha_{k,t-2} + \epsilon_{k,t}}
#'
#' or
#'
#' \deqn{\alpha_{k,u,t} = 2 \alpha_{k,u,t-1} - \alpha_{k,u,t-2} + \epsilon_{k,u,t}.}
#'
#' For details, see [AR()], [AR1()],
#' [RW()], and [RW2()].
#' 
#' @inheritSection AR Constraints
#'
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#'
#' @inheritParams SVD
#' @param n_coef Number of AR coefficients in `SVD_RW()`.
#' @param s Scale for standard deviations terms.
#' @param sd Standard deviation
#' of initial value for random walks. Default is `1`.
#' Can be `0`.
#' @param sd_slope Standard deviation in prior
#' for initial slope. Default is `1`.
#' @param shape1,shape2 Parameters for prior
#' for coefficients in `SVD_AR()`.
#' Defaults are `5` and `5`.
#' @param min,max Minimum and maximum values
#' for autocorrelation coefficient in `SVD_AR1()`.
#' Defaults are `0.8` and `0.98`.
#' @param con Constraints on parameters.
#' Current choices are `"none"` and `"by"`.
#' Default is `"none"`. See below for details.
#'
#' @returns An object of class `"bage_prior_svd_ar"`,
#' `"bage_prior_svd_rw"`, or `"bage_prior_svd_rw2"`.
#'
#' @seealso
#' - [SVD()] SVD prior for non-time-varying terms
#' - [RW()] Smoothing via random walk
#' - [RW2()] Smoothing via second-order random walk
#' - [Sp()] Smoothing via splines
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [priors] Overview of priors implemented in \pkg{bage}
#' - [set_prior()] Specify prior for intercept,
#'   main effect, or interaction
#' - [set_var_sexgender()] Identify sex or gender variable in data
#'
#' @references
#' - For details of the construction of
#'   scaled SVDS see the
#'   [Mathematical Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
#'   vignette
#'
#' @examples
#' SVD_AR1(HMD)
#' SVD_RW(HMD, n_comp = 3)
#' SVD_RW2(HMD, indep = FALSE)
#' @export
SVD_AR <- function(ssvd,
                   v = NULL,
                   n_comp = NULL,
                   indep = TRUE,
                   n_coef = 2,
                   s = 1,
                   shape1 = 5,
                   shape2 = 5,
                   con = c("none", "by")) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_v_ssvd(v = v, ssvd = ssvd, nm_ssvd = nm_ssvd)
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_flag(x = indep, nm_x = "indep")
  poputils::check_n(n = n_coef,
                    nm_n = "n_coef",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  check_scale(x = s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  n_coef <- as.integer(n_coef)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  con <- match.arg(con)
  new_bage_prior_svd_ar(ssvd = ssvd,
                        v = v,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        indep = indep,
                        n_coef = n_coef,
                        shape1 = shape1,
                        shape2 = shape2,
                        min = -1,
                        max = 1,
                        scale = scale,
                        con = con,
                        nm = "SVD_AR")
}

## HAS_TESTS
#' @rdname SVD_AR
#' @export
SVD_AR1 <- function(ssvd,
                    v = NULL,
                    n_comp = NULL,
                    indep = TRUE,
                    min = 0.8,
                    max = 0.98,
                    s = 1,
                    shape1 = 5,
                    shape2 = 5,
                    con = c("none", "by")) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_v_ssvd(v = v, ssvd = ssvd, nm_ssvd = nm_ssvd)
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_flag(x = indep, nm_x = "indep")
  check_min_max_ar(min = min, max = max)
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(shape1, nm_x = "shape1", zero_ok = FALSE)
  check_scale(shape2, nm_x = "shape2", zero_ok = FALSE)
  con <- match.arg(con)
  min <- as.double(min)
  max <- as.double(max)
  scale <- as.double(s)
  shape1 <- as.double(shape1)
  shape2 <- as.double(shape2)
  new_bage_prior_svd_ar(ssvd = ssvd,
                        v = v,
                        nm_ssvd = nm_ssvd,
                        n_comp = n_comp,
                        indep = indep,
                        n_coef = 1L,
                        shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale,
                        con = con,
                        nm = "SVD_AR1")
}

#' @rdname SVD_AR
#' @export
SVD_RW <- function(ssvd,
                   v = NULL,
                   n_comp = NULL,
                   indep = TRUE,
                   s = 1,
                   sd = 1,
                   con = c("none", "by")) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_v_ssvd(v = v, ssvd = ssvd, nm_ssvd = nm_ssvd)
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_flag(x = indep, nm_x = "indep")
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "sd", zero_ok = TRUE)
  con <- match.arg(con)
  scale <- as.double(s)
  sd <- as.double(sd)
  if (sd > 0)
    new_bage_prior_svd_rwrandom(ssvd = ssvd,
                                v = v,
                                nm_ssvd = nm_ssvd,
                                n_comp = n_comp,
                                indep = indep,
                                scale = scale,
                                sd = sd,
                                con = con)
  else
    new_bage_prior_svd_rwzero(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              scale = scale,
                              con = con)
}

## HAS_TESTS
#' @rdname SVD_AR
#' @export
SVD_RW2 <- function(ssvd,
                    v = NULL,
                    n_comp = NULL,
                    indep = TRUE,
                    s = 1,
                    sd = 1,
                    sd_slope = 1,
                    con = c("none", "by")) {
  nm_ssvd <- deparse1(substitute(ssvd))
  check_is_ssvd(x = ssvd, nm_x = "ssvd")
  check_v_ssvd(v = v, ssvd = ssvd, nm_ssvd = nm_ssvd)
  n_comp <- n_comp_svd(n_comp = n_comp,
                       nm_n_comp = "n_comp",
                       ssvd = ssvd)
  check_flag(x = indep, nm_x = "indep")
  check_scale(s, nm_x = "s", zero_ok = FALSE)
  check_scale(sd, nm_x = "s", zero_ok = TRUE)
  check_scale(sd_slope, nm_x = "sd_slope", zero_ok = FALSE)
  con <- match.arg(con)
  scale <- as.double(s)
  sd <- as.double(sd)
  sd_slope <- as.double(sd_slope)
  if (sd > 0)
    new_bage_prior_svd_rw2random(ssvd = ssvd,
                                 v = v,
                                 nm_ssvd = nm_ssvd,
                                 n_comp = n_comp,
                                 indep = indep,
                                 scale = scale,
                                 sd = sd,
                                 sd_slope = sd_slope,
                                 con = con)
  else
    new_bage_prior_svd_rw2zero(ssvd = ssvd,
                               v = v,
                               nm_ssvd = nm_ssvd,
                               n_comp = n_comp,
                               indep = indep,
                               scale = scale,
                               sd_slope = sd_slope,
                               con = con)
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
new_bage_prior_ar <- function(n_coef,
                              shape1,
                              shape2,
                              min,
                              max,
                              scale,
                              nm,
                              along,
                              con) {
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
                              con = con,
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
new_bage_prior_lin <- function(scale,
                               mean_slope,
                               sd_slope,
                               along,
                               con) {
    ans <- list(i_prior = 2L,
                const = c(scale = scale,
                          mean_slope = mean_slope,
                          sd_slope = sd_slope),
                specific = list(scale = scale,
                                mean_slope = mean_slope,
                                sd_slope = sd_slope,
                                along = along,
                                con = con))
    class(ans) <- c("bage_prior_lin", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_linar <- function(n_coef,
                                 mean_slope,
                                 sd_slope,
                                 shape1,
                                 shape2,
                                 min,
                                 max,
                                 scale,
                                 along,
                                 con,
                                 nm) {
  ans <- list(i_prior = 3L,
              const = c(mean_slope = mean_slope,
                        sd_slope = sd_slope,
                        shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale),
              specific = list(n_coef = n_coef,
                              mean_slope = mean_slope,
                              sd_slope = sd_slope,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              scale = scale,
                              along = along,
                              con = con,
                              nm = nm))
  class(ans) <- c("bage_prior_linar", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_linex <- function(mean_slope,
                                 sd_slope,
                                 along,
                                 con) {
  ans <- list(i_prior = 17L,
              const = c(mean_slope = mean_slope,
                        sd_slope = sd_slope),
              specific = list(mean_slope = mean_slope,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_linex", "bage_prior")
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
new_bage_prior_rwrandom <- function(scale,
                                    sd,
                                    along,
                                    con) {
  ans <- list(i_prior = 19L,
              const = c(scale = scale,
                        sd = sd),
              specific = list(scale = scale,
                              sd = sd,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwrandom", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rwrandomseasfix <- function(n_seas,
                                           sd_seas,
                                           scale,
                                           sd,
                                           along,
                                           con) {
  ans <- list(i_prior = 20L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        sd_seas = sd_seas,
                        scale = scale,
                        sd = sd),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              sd_seas = sd_seas,
                              scale = scale,
                              sd = sd,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwrandomseasfix", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rwrandomseasvary <- function(n_seas,
                                            scale_seas,
                                            sd_seas,
                                            scale,
                                            sd,
                                            along,
                                            con) {
  ans <- list(i_prior = 21L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        scale_seas = scale_seas,
                        sd_seas = sd_seas,
                        scale = scale,
                        sd = sd),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              scale_seas = scale_seas,
                              sd_seas = sd_seas,
                              scale = scale,
                              sd = sd,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwrandomseasvary", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rwzero <- function(scale,
                                  along,
                                  con) {
  ans <- list(i_prior = 6L,
              const = c(scale = scale),
              specific = list(scale = scale,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwzero", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rwzeroseasfix <- function(n_seas,
                                         sd_seas,
                                         scale,
                                         along,
                                         con) {
  ans <- list(i_prior = 10L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        sd_seas = sd_seas,
                        scale = scale),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              sd_seas = sd_seas,
                              scale = scale,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwzeroseasfix", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rwzeroseasvary <- function(n_seas,
                                          scale_seas,
                                          sd_seas,
                                          scale,
                                          along,
                                          con) {
  ans <- list(i_prior = 11L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        scale_seas = scale_seas,
                        sd_seas = sd_seas,
                        scale = scale),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              scale_seas = scale_seas,
                              sd_seas = sd_seas,
                              scale = scale,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rwzeroseasvary", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2infant <- function(scale,
                                     sd_slope,
                                     con) {
    ans <- list(i_prior = 18L,
                const = c(scale = scale,
                          sd_slope = sd_slope),
                specific = list(scale = scale,
                                sd_slope = sd_slope,
                                along = NULL,
                                con = con))
    class(ans) <- c("bage_prior_rw2infant", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2random <- function(scale,
                                     sd,
                                     sd_slope,
                                     along,
                                     con) {
  ans <- list(i_prior = 22L,
              const = c(scale = scale,
                        sd = sd,
                        sd_slope = sd_slope),
              specific = list(scale = scale,
                              sd = sd,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2random", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2randomseasfix <- function(n_seas,
                                            sd_seas,
                                            scale,
                                            sd,
                                            sd_slope,
                                            along,
                                            con) {
  ans <- list(i_prior = 23L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        sd_seas = sd_seas,
                        scale = scale,
                        sd = sd,
                        sd_slope = sd_slope),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              sd_seas = sd_seas,
                              scale = scale,
                              sd = sd,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2randomseasfix", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2randomseasvary <- function(n_seas,
                                             scale_seas,
                                             sd_seas,
                                             scale,
                                             sd,
                                             sd_slope,
                                             along,
                                             con) {
  ans <- list(i_prior = 24L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        scale_seas = scale_seas,
                        sd_seas = sd_seas,
                        scale = scale,
                        sd = sd,
                        sd_slope = sd_slope),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              scale_seas = scale_seas,
                              sd_seas = sd_seas,
                              scale = scale,
                              sd = sd,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2randomseasvary", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2zero <- function(scale,
                                   sd_slope,
                                   along,
                                   con) {
  ans <- list(i_prior = 7L,
              const = c(scale = scale,
                        sd_slope = sd_slope),
              specific = list(scale = scale,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2zero", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2zeroseasfix <- function(n_seas,
                                          sd_seas,
                                          scale,
                                          sd_slope,
                                          along,
                                          con) {
  ans <- list(i_prior = 12L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        sd_seas = sd_seas,
                        scale = scale,
                        sd_slope = sd_slope),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              sd_seas = sd_seas,
                              scale = scale,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2zeroseasfix", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_rw2zeroseasvary <- function(n_seas,
                                           scale_seas,
                                           sd_seas,
                                           scale,
                                           sd_slope,
                                           along,
                                           con) {
  ans <- list(i_prior = 13L,
              const = c(n_seas = n_seas,       ## put season-related quantities at beginning
                        scale_seas = scale_seas,
                        sd_seas = sd_seas,
                        scale = scale,
                        sd_slope = sd_slope),
              specific = list(n_seas = n_seas, ## put season-related quantities at beginning
                              scale_seas = scale_seas,
                              sd_seas = sd_seas,
                              scale = scale,
                              sd_slope = sd_slope,
                              along = along,
                              con = con))
  class(ans) <- c("bage_prior_rw2zeroseasvary", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_spline <- function(n_comp,
                                  scale,
                                  sd,
                                  sd_slope,
                                  along,
                                  con) {
    ans <- list(i_prior = 8L,
                const = c(scale = scale,
                          sd = sd,
                          sd_slope = sd_slope),
                specific = list(n_comp = n_comp,
                                scale = scale,
                                sd = sd,
                                sd_slope = sd_slope,
                                along = along,
                                con = con))
    class(ans) <- c("bage_prior_spline", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_svd <- function(ssvd,
                               v,
                               nm_ssvd,
                               n_comp,
                               indep) {
    ans <- list(i_prior = 9L,
                const = 0, ## not used
                specific = list(ssvd = ssvd,
                                v = v,
                                nm_ssvd = nm_ssvd,
                                n_comp = n_comp,
                                indep = indep))
    class(ans) <- c("bage_prior_svd", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_svd_ar <- function(ssvd,
                                  v,
                                  nm_ssvd,
                                  n_comp,
                                  indep,
                                  n_coef,
                                  scale,
                                  shape1,
                                  shape2,
                                  min,
                                  max,
                                  con,
                                  nm) {
  ans <- list(i_prior = 14L,
              const = c(shape1 = shape1,
                        shape2 = shape2,
                        min = min,
                        max = max,
                        scale = scale),
              specific = list(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              n_coef = n_coef,
                              shape1 = shape1,
                              shape2 = shape2,
                              min = min,
                              max = max,
                              scale = scale,
                              along = NULL,
                              con = con,
                              nm = nm))
  class(ans) <- c("bage_prior_svd_ar", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_svd_rwrandom <- function(ssvd,
                                        v,
                                        nm_ssvd,
                                        n_comp,
                                        indep,
                                        scale,
                                        sd,
                                        con) {
  ans <- list(i_prior = 25L,
              const = c(scale = scale,
                        sd = sd),
              specific = list(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              scale = scale,
                              sd = sd,
                              along = NULL,
                              con = con))
  class(ans) <- c("bage_prior_svd_rwrandom", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_svd_rwzero <- function(ssvd,
                                      v,
                                      nm_ssvd,
                                      n_comp,
                                      indep,
                                      scale,
                                      con) {
  ans <- list(i_prior = 15L,
              const = c(scale = scale),
              specific = list(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              scale = scale,
                              along = NULL,
                              con = con))
  class(ans) <- c("bage_prior_svd_rwzero", "bage_prior")
  ans
}


## HAS_TESTS
new_bage_prior_svd_rw2random <- function(ssvd,
                                         v,
                                         nm_ssvd,
                                         n_comp,
                                         indep,
                                         scale,
                                         sd,
                                         sd_slope,
                                         con) {
  ans <- list(i_prior = 26L,
              const = c(scale = scale,
                        sd = sd,
                        sd_slope = sd_slope),
              specific = list(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              scale = scale,
                              sd = sd,
                              sd_slope = sd_slope,
                              along = NULL,
                              con = con))
  class(ans) <- c("bage_prior_svd_rw2random", "bage_prior")
  ans
}

## HAS_TESTS
new_bage_prior_svd_rw2zero <- function(ssvd,
                                       v,
                                       nm_ssvd,
                                       n_comp,
                                       indep,
                                       scale,
                                       sd_slope,
                                       con) {
  ans <- list(i_prior = 16L,
              const = c(scale = scale,
                        sd_slope = sd_slope),
              specific = list(ssvd = ssvd,
                              v = v,
                              nm_ssvd = nm_ssvd,
                              n_comp = n_comp,
                              indep = indep,
                              scale = scale,
                              sd_slope = sd_slope,
                              along = NULL,
                              con = con))
  class(ans) <- c("bage_prior_svd_rw2zero", "bage_prior")
  ans
}
