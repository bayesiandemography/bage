
fit_inner_outer <- function(mod, vars_inner) {
  use_term <- make_use_term(mod = mod, vars_inner = vars_inner)
  mod_inner <- reduce_model_terms(mod = mod, use_term = use_term)
  mod_inner <- fit_default(mod_inner)
  comp_inner <- components(mod_inner)
  point_est_terms_inner <- make_point_est_terms(comp_inner)
  mod_outer <- set_priors_known(mod = mod, vals = point_est_inner)
  use_term[] <- TRUE
  mod_outer <- fit_default(mod_outer)
  mod <- make_stored_draws_inner_outer(mod = mod,
                                       mod_inner = mod_inner,
                                       mod_outer = mod_outer)
  mod
}


reduce_model_terms <- function(mod, use_term) {
  nms_components <- "xxx"
}




#' @noRd
make_stored_draws_inner_outer <- function(mod, mod_inner, mod_outer, use_term) {
  n_term <- length(use_term)
  nms_term <- names(mod$priors)
  draws_effectfree <- vector(mode = "list", length = n_term)
  draws_hyper <- vector(mode = "list", length = n_term)
  draws_hyperrand <- vector(mode = "list", length = n_term)
  terms_effectfree_inner <- make_terms_effectfree(mod_inner)
  terms_effectfree_outer <- make_terms_effectfree(mod_outer)
  terms_hyper_inner <- make_terms_hyper(mod_inner)
  terms_hyper_outer <- make_terms_hyper(mod_outer)
  terms_hyperrand_inner <- make_terms_hyperrand(mod_inner)
  terms_hyperrand_outer <- make_terms_hyperrand(mod_outer)
  for (i_term in seq_len(n_term)) {
    nm_term <- nms_term[[i_term]]
    use_inner <- use_term[[i_term]]
    if (use_inner) {
      is_effectfree <- terms_effectfree_inner == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_inner[is_effectfree, ]
      is_hyper <- terms_hyper_inner == nm_term
      draws_hyper[[i_term]] <- draws_hyper_inner[is_hyper, ]
      is_hyperrand <- terms_hyperrand_inner == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_inner[is_hyperrand, ]
    }
    else {
      is_effectfree <- terms_effectfree_outer == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_outer[is_effectfree, ]
      is_hyper <- terms_hyper_outer == nm_term
      draws_hyper[[i_term]] <- draws_hyper_outer[is_hyper, ]
      is_hyperrand <- terms_hyperrand_outer == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_outer[is_hyperrand, ]
    }
  }
  mod$draws_effectfree <- vctrs::vec_rbind(!!!draws_effectfree)
  mod$draws_hyper <- vctrs::vec_rbind(!!!draws_hyper)
  mod$draws_hyperrand <- vctrs::vec_rbind(!!!draws_hyperrand)
  if (has_disp(mod))
    mod$draws_disp <- mod_outer$draws_disp
  mod
}





check_vars_inner <- function(vars_inner, vars) {
  ## duplicated
  ## NAs
  ## blanks
  in_vars <- vars_inner %in% vars
  i_not_in_vars <- match(FALSE, in_vars, nomatch = 0L)
  if (i_not_in_vars > 0L) {
    cli::cli_abort(c("{.arg {vars_inner[[i_not_in_vars]]}} is not a variable in the model.",
                     i = "Model variables: {.val {vars}}."))
  }
  invisible(TRUE)
}  

get_vars <- function(formula) {
  factors <- attr(terms(formula), "factors")
  rownames(factors)[-1L]
}

make_is_term_inner <- function(formula, vars_inner) {
  factors <- attr(terms(formula), "factors")
  rn <- rownames(factors)
  apply(factors, 2L, function(i) all(rn[i] %in% vars_inner))
}


make_include_term <- function(mod) {
  priors <- mod$priors
  has_vars_inner <- !is.null(vars_inner)
  if (has_vars_inner)
    ans <- make_is_term_inner(mod)
  else
    ans <- rep(TRUE, times = length(priors))
  ans
}

  
  
  
  


