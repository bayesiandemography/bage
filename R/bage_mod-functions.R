
## User-visible functions that look like methods, but technically are not


## 'set_n_draw' ---------------------------------------------------------------

#' Set the number of draws
#'
#' Specify the number of draws from the posterior
#' distribution to be used in model output.
#' A newly-created `bage_mod` object has an
#' `n_draw` value of 1000. Higher values
#' may be appropriate for characterising
#' the tails of distributions, or for
#' publication-quality graphics and summaries.
#'
#' The value of `n_draw` does not affect
#' model fitting: it only affects posterior
#' summaries.
#'
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param n_draw Number of draws.
#'
#' @returns A `bage_mod` object
#' 
#' @seealso Functions whose output is affected
#' by the number of draws include [bage::augment()].
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#'
#' mod |>
#'   set_n_draw(n_draw = 5000)
#' @export
set_n_draw <- function(mod, n_draw = 1000L) {
    checkmate::assert_class(mod, "bage_mod")
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 0L,
                                    coerce = TRUE)
    mod$n_draw <- n_draw
    mod
}


## 'set_prior' ----------------------------------------------------------------

## HAS_TESTS
#' Change the prior for a model term
#'
#' Specify a non-default prior distribution
#' for a main effect or interaction.
#'
#' If the `mod` argument to `set_prior` is
#' a fitted model, then `set_prior` 'unfits'
#' `mod`, ie `set_prior` deletes existing
#' estimates and returns `mod` to an
#' unfitted state.
#'
#' `formula` gives the name of a main
#' effect or interaction, and a function
#' specifying a prior, eg
#' `age ~ RW2()`.
#'
#' @param mod A `bage_mod` object, created with
#' [mod_pois()], [mod_binom()], or [mod_norm()].
#' @param formula A formula giving the term
#' and a function for creating a prior.
#'
#' @returns A `bage_mod` object.
#'
#' @seealso [N()], [RW()], [RW2()], [AR1()], [Known()]
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod
#' mod |> set_prior(age ~ RW2())
#' @export
set_prior <- function(mod, formula) {
    checkmate::assert_class(mod, "bage_mod")
    check_format_prior_formula(formula)
    nm_response <- deparse1(formula[[2L]])
    nms_terms <- names(mod$priors)
    i <- match(nm_response, nms_terms, nomatch = 0L)
    if (i == 0L)
        stop(gettextf(paste("response in prior formula '%s' not a",
                            "valid term from model formula '%s' :",
                            "valid terms are %s"),
                      deparse1(formula),
                      deparse1(mod$formula),
                      paste(sprintf("'%s'", nms_terms), collapse = ", ")),
             call. = FALSE)
    prior <- tryCatch(eval(formula[[3L]]),
                      error = function(e) e)
    if (inherits(prior, "error"))
        stop(gettextf("prior formula '%s' invalid : %s",
                      deparse1(formula),
                      prior$message),
             call. = FALSE)
    mod$priors[[i]] <- prior
    mod["est"] <- list(NULL)
    mod["prec"] <- list(NULL)
    mod
}


## 'set_var_age' --------------------------------------------------------------

#' Set the age variable
#'
#' Specify which variable (if any) represents age.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the age variable
#' from variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#' 
#' `~ age + region + age:region`
#'
#' contains variables `age` and `region`,
#' and terms `age`, `region`, and `age:region`.
#'
#' By default, an age main effect has a random walk
#' ([RW()]) prior. Changing the age variable
#' via `set_var_age()` can change priors:
#' see below for an example.
#' 
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param name The name of the age variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_sexgender()] sets the sex or gender variable
#' - [set_var_time()] sets the time variable
#' - internally, `bage` uses [find_age()] to locate age variables
#' 
#' @examples
#' ## rename 'age' variable to something unusual
#' injuries2 <- injuries
#' injuries2$age_last_birthday <- injuries2$age
#'
#' ## mod_pois does not recognize age variable
#' mod <- mod_pois(injuries ~ age_last_birthday * ethnicity + year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the age variable explicitly
#' ## (which, as a side effect, changes the prior on
#' ## the age main effect)
#' mod |>
#'   set_var_age(name = "age_last_birthday")
#' @export
set_var_age <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "age")
}


## 'set_var_sexgender' --------------------------------------------------------

## HAS_TESTS
#' Set a sex or gender variable
#'
#' Specify which variable (if any) represents sex or gender.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to find a sex or gender variable
#' using variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#' 
#' `~ gender + region + gender:region`
#'
#' contains variables `gender` and `region`,
#' and terms `gender`, `region`, and `gender:region`.
#'
#' @param mod A `"bage_mod"` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param name The name of the sex or gender variable.
#'
#' @returns A `"bage_mod"` object
#'
#' @seealso
#' - [set_var_age()] sets the age variable
#' - [set_var_time()] sets the time variable
#' - internally, `bage` uses [find_sexgender()] to locate sex or gender variables
#' - internally, `bage` uses [find_female()] to locate female categories within a sex or gender variable
#' - internally, `bage` uses [find_male()] to locate male categories within a sex or gender variable
#' 
#' @examples
#' ## rename 'sex' variable to something unexpected
#' injuries2 <- injuries
#' injuries2$biological_sex <- injuries2$sex
#'
#' ## mod_pois does not recognize sex variable
#' mod <- mod_pois(injuries ~ age * biological_sex + year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the sex variable explicitly
#' mod |>
#'   set_var_sexgender(name = "biological_sex")
#' @export
set_var_sexgender <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "sexgender")
}



## 'set_var_time' --------------------------------------------------------------

## HAS_TESTS
#' Set the time variable
#'
#' Specify which variable (if any) represents time.
#' Functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] try to infer the time variable
#' from variable names, but do not always get it right.
#'
#' In an R \code{\link{formula}}, a 'variable' is different
#' from a 'term'. For instance,
#' 
#' `~ time + region + time:region`
#'
#' contains variables `time` and `region`,
#' and terms `time`, `region`, and `time:region`.
#'
#' By default, an time main effect has a random walk
#' ([RW()]) prior. Changing the time variable
#' via `set_var_time()` can change priors:
#' see below for an example.
#' 
#' @param mod A `bage_mod` object, typically
#' created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param name The name of the time variable.
#'
#' @returns A `bage_mod` object
#'
#' @seealso
#' - [set_var_age()] sets the age variable
#' - [set_var_sexgender()] sets the sex or gender
#' - internally, `bage` uses [find_time()] to locate time variables
#'
#' @examples
#' ## rename time variable to something unusual
#' injuries2 <- injuries
#' injuries2$calendar_year <- injuries2$year
#'
#' ## mod_pois does not recognize time variable
#' mod <- mod_pois(injuries ~ age * ethnicity + calendar_year,
#'                 data = injuries2,
#'                 exposure = popn)
#' mod
#'
#' ## so we set the time variable explicitly
#' ## (which, as a side effect, changes the prior on
#' ## the time main effect)
#' mod |>
#'   set_var_time(name = "calendar_year")
#' @export
set_var_time <- function(mod, name) {
    set_var_inner(mod = mod,
                  name = name,
                  var = "time")
}


## generate.bage_mod <- function(x, known = "(Intercept)", ...) {
##     nm_distn <- x$nm_distn
##     is_fitted <- !is.null(x$est)
##     if (!is_fitted)
##         x <- fit(x)
##     nms_priors <- names(x$priors)
##     check_known(known = known,
##                 nms_priors = nms_priors,
##                 nm_distn = nm_distn)
##     if (known == ".")
##         generate_all(x)
##     else if (known %in% c("rate", "prob", "mean"))
##         generate_outcome(x)
##     else
##         generate_partial(mod, known = known)
## }

## fit.bage_mod_sim <- function(x) {
##     NULL
## }


## HAS_TESTS
#' Set var_age, var_sexgender, or var_time
#'
#' Can include resetting priors.
#' Called by user-visible functions
#' 'set_var_age', 'sex_var_sexgender',
#' and 'set_var_time'.
#' 
#' @param mod A `bage_mod` object.
#' @param name The name of the variable.
#' @param var "age", "sexgender", or "time"
#'
#' @returns A `bage_mod` object
#'
#' @noRd
set_var_inner <- function(mod, name, var) {
    choices <- c("age", "sexgender", "time")
    checkmate::assert_class(mod, "bage_mod")
    checkmate::assert_choice(var, choices = choices)
    vars_oth <- setdiff(choices, var)
    attr_name <- paste0("var_", var)
    attr_names_oth <- paste0("var_", vars_oth)
    ## extract values
    formula <- mod$formula
    scale_prior <- mod$scale_prior
    priors <- mod$priors
    name_old <- mod[[attr_name]]
    names_oth <- lapply(attr_names_oth, function(nm) mod[[nm]])
    has_name_old <- !is.null(name_old)
    names_priors <- names(priors)
    ## check 'name'
    checkmate::assert_string(name, min.chars = 1L)
    check_formula_has_variable(name = name, formula = formula)
    for (i_oth in seq_along(names_oth)) {
        nm_oth <- names_oth[[i_oth]]
        if (!is.null(nm_oth)) {
            if (identical(name, nm_oth))
                stop(gettextf("%s variable and %s variable have same name [\"%s\"]",
                              var,
                              vars_oth[[i_oth]],
                              name),
                     call. = FALSE)
        }
    }
    ## modify var
    mod[[attr_name]] <- name
    ## reset priors
    var_age <- mod[["var_age"]]
    var_time <- mod[["var_time"]]
    priors[[name]] <- default_prior(nm_term = name,
                                    scale = scale_prior,
                                    var_age = var_age,
                                    var_time = var_time)
    if (has_name_old) {
        priors[[name_old]] <- default_prior(nm_term = name_old,
                                            scale = scale_prior,
                                            var_age = var_age, 
                                            var_time = var_time)
    }
    ## modify priors
    mod$priors <- priors
    ## return
    mod
}
