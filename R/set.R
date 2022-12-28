

## TODO - set_errors()
## TODO - set_season()
## TODO - set_data_source()


set_prior <- function(mod, formula) {
    check_valid_prior_formula(formula)
    nm_response <- as.character(formula[[2L]])
    prior <- tryCatch(eval(formula[[3L]]),
                      error = function(e) e)
    if (inherits(prior, "error"))
        stop(gettextf("prior '%s' invalid : %s",
                      nm_response,
                      prior$message),
             call. = FALSE)
    nms_priors <- names(mod$priors)
    i <- match(nm_response, nms_priors, nomatch = 0L)
    if (i == 0L)
        stop(gettextf("'%s' is not a valid for formula '%s' : valid terms are %s",
                      nm_response,
                      mod$formula,
                      paste(nms_priors, collapse = ", ")),
             call. = FALSE)
    mod$priors[[i]] <- prior
    mod
}

