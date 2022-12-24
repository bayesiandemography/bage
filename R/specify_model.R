
## new_bage_sysmod <- function(formula = NULL,
##                             distn = NULL,
##                             offset = NULL) {
##     ans <- list(formula = formula,
##                 distn = distn,
##                 offset = offset,
##                 data = NULL,
##                 priors = list(),
##                 params = NULL,
##                 cov = NULL,
##                 is_cov = NULL)
##     class(ans) <- "bage_sysmod"
##     ans
## }



## mod_pois <- function(formula, exposure) {
##     checkmate::assert_formula(formula)
##     check_formula_has_response(formula)
##     offset <- substitute(exposure)
##     check_offset(offset, name = "exposure")
##     new_base_sysmod(formula = formula,
##                     distn = "pois",
##                     offset = offset)
## }


## check_formula_has_response <- function(formula) {
##     has_response <- attr(stats::terms(formula), "response")
##     if (!has_response)
##         stop(gettextf("formula '%s' does not include a response",
##                       deparse(formula)),
##              call. = FALSE)
##     invisible(TRUE)
## }
    

## set_data <- function(x, data) {
##     formula <- x$formula
##     check_formula_vnames_in_data(formula = formula,
##                                  data = data)
##     x$data <- data
##     x
## }

## make_map_matrices <- function(formula, data) {
##     factors <- attr(stats::terms(formula), "factors")
##     factors <- factors[-1L, ]
##     nms_predictors <- rownames(factors)
##     formula_target <- paste0("~", paste(nms_predictors, sep = "+"))
##     formula_target <- as.formula(formula_target)
##     target_array <- stats::xtabs(formula_target, data = data)
##     dim_target <- dim(target_array)
##     apply(factors,
##           MARGIN = 2L,
##           FUN = make_map_matrix,
##           dim_target = dim_target,
##           simplify = FALSE)
## }

## make_map_matrix <- function(has_margin_target, dim_target) {
##     elements <- mapply(function(x, y) if (x) diag(y) else matrix(1L, nrow = y),
##                       x = as.list(has_margin_target),
##                       y = as.list(dim_target),
##                       SIMPLIFY = FALSE,
##                       USE.NAMES = FALSE)
##     Reduce(kronecker, rev(elements))
## }
                      


## check_formula_vnames_in_data <- function(formula, data) {
##     nms_formula <- rownames(attr(stats::terms(formula), "factors"))
##     nms_data <- names(data)
##     is_in_data <- nms_formula %in% nms_data
##     i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
##     if (i_not_in_data > 0L)
##         stop(gettextf("term '%s' from formula '%s' not found in '%s'",
##                       nms_formula[[i_not_in_data]],
##                       deparse(formula),
##                       "data"),
##              call. = FALSE)
##     invisible(TRUE)
## }


## set_prior(mod, formula) {
##     check_valid_prior_formula(formula)
##     response_name <- as.character(formula[[2L]])
##     prior <- eval(formula[[3L]])
##     mod$priors <- c(mod$priors,
##                     setNames(list(prior), nm = response_name))
##     mod
## }

## RW <- function(scale = 1) {
##     checkmate::assert_number(scale)
##     if (scale <= 0)
##         stop(gettextf("value for '%s' [%s] is non-positive",
##                       "scale", scale),
##              call. = FALSE)
##     new_bage_prior_rw(scale)
## }


## new_bage_prior_rw <- function(scale) {
##     ans <- list(scale = scale)
##     class(ans) <- c("bage_prior_rw", "bage_prior")
##     ans
## }
    

    
    





    
    
