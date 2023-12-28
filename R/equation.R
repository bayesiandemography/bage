equation.bage_mod <- function(object, minlength = 4, ...) {
    formula <- object@formula
    priors <- object@priors
    ans_likelihood <- equation_likelihood(object)
    ans_means <- equation_means(object)
    nms_priors <- make_abbr_nms_priors(formula)
    ans_priors <- .mapply(equation_prior,
                          prior = priors,
                          nm = nms_priors)
    ans_disp <- equation_disp(object)
    ans_season <- equation_season(object)
    sprintf("\begin{align}\n%s%s%s\end{align}\n",
            ans_likelihood,
            ans_priors,
            ans_disp,
            ans_season)
}


equation_likelihood.bage_mod_pois <- function(object) {
    formula <- object@formula
    offset <- object@offset
    is_ones <- all(offset == 1L)
    exposure <- if (is_ones) "w_i" else ""
    sprintf("  y_i & \sim \text{Poisson}(\gamma_i %s)\\", exposure)
}

equation_disp.bage_mod_pois <- function(object) {
    scale <- object$scale_disp
    if (isTRUE(all.equal(scale, 0)))
        return("")
    paste0("  p(\xi) & = \frac{A_{\xi}}{2 \sqrt{\xi}} e^{-A_{\xi} \sqrt{\xi}}\\",
           sprintf("  A_{\xi} & = %s\\", scale))
}
        
    
