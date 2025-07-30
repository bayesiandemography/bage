


## HAS_TESTS
#' @export
draw_vals_outcome_true.bage_datamod_outcome_rr3 <- function(datamod,
                                                            nm_distn,
                                                            outcome_obs,
                                                            fitted,
                                                            expected,
                                                            disp,
                                                            offset) {
  n_val <- length(outcome_obs)
  has_disp <- !is.null(disp)
  if (has_disp) {
    n_draw <- rvec::n_draw(expected)
    expected <- as.matrix(expected)
    disp <- matrix(as.numeric(disp), nrow = n_val, ncol = n_draw, byrow = TRUE)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "nbinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(shape = shape, rate = rate)
    }
    else if (nm_distn == "binom") {
      nm <- "betabinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(size = offset, shape = shape, rate = rate)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  else {
    n_draw <- rvec::n_draw(fitted)
    fitted <- as.matrix(fitted)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "pois"
      lambda <- fitted * offset
      args <- list(lambda = lambda)
    }
    else if (nm_distn == "binom") {
      nm <- "binom"
      args <- list(size = offset, prob = fitted)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  args <- lapply(seq_along(n_val),
                 function(i) lapply(args, function(j) arg[i, ]))
  draw_vals_outcome_true_rr3(nm_distn_detailed = nm_dist_detailed,
                             outcome_obs = outcome_obs,
                             args = args,
                             offset = offset,
                             n_draw = n_draw)
}                            


#' Draw Values for Outcome Variable When Value Not Supplied
#' 
draw_vals_outcome_no_obs <- function(nm_distn_detailed,
                                     args,
                                     offset) {
  if (nm_distn_detailed == "pois") {
    lambda <- args$lambda
    stats::rpois(n = length(lambda),
                 lambda = lambda)
  }
  else if (nm_distn_detailed == "nbinom") {
    shape <- args$shape
    rate <- args$rate
    stats::rnbinom(n = length(shape),
                   shape = shape,
                   rate = rate)
  }
  else if (nm_distn_detailed == "binom") {
    size <- args$size
    prob <- args$prob
    stats::rbinom(n = length(size),
                  size = size,
                  prob = prob)
  }
  else if (nm_distn_detailed == "betabinom") {
    size <- args$size
    shape1 <- args$shape1
    shape2 <- args$shape2
    stats::rbetabinom(n = length(size),
                      size = size,
                      shape1 = shape1,
                      shape2 = shape2)
  }
  else
    cli::cli_abort(paste("Internal error: Invalid value for",
                         "{.var nm_distn_detailed}."))
}


draw_vals_outcome_true_rr3 <- function(nm_distn_detailed,
                                       outcome_obs,
                                       args,
                                       offset,
                                       n_draw) {
  n_val <- length(outcome_obs)
  ans <- matrix(NA_integer_, nrow = n_val, ncol = n_draw)
  for (i_val in seq_len(n_val)) {
    outcome_obs_i <- outcome_obs[[i_val]]
    offset_i <- offset[[i_val]]
    has_outcome <- is.na(outcome_obs_i)
    has_offset <- is.na(offset_i)
    args_i <- args[[i_val]]
    if (has_offset) {
      if (has_outcome) {
        ans[i_val, ] <- draw_vals_outcome_no_outcome(nm_distn_detailed = nm_dist_detailed,
                                                     args = args_i,
                                                     offset = offset_i)
      }
      else {
        ans[i_val, ] <- draw_vals_outcome_has_outcome_obs_rr3(nm_distn_detailed = nm_dist_detailed,
                                                              n_draw = n_draw,
                                                              args = args_i,
                                                              outcome_obs = outcome_obs_i,
                                                              offset = offset_i)
      }
    }
  }
  ans <- rvec::rvec(ans)
  ans
}



draw_vals_outcome_has_obs_rr3 <- function(nm_distn_detailed,
                                          n_draw,
                                          args,
                                          observed_obs,
                                          offset) {
  is_obs_zero <- observed_obs == 0L
  if (is_obs_zero) {
    outcome_true <- 0:2
    prob_obs_given_true <- c(1, 2/3, 1/3)
  }
  else {
    outcome_true <- seq.int(from = observed_obs - 2L, to = observed_obs + 2L)
    prob_obs_given_true <- c(1/3, 2/3, 1, 2/3, 1/3)
  }
  prob_obs_true <- make_prob_obs_true(nm_distn_detailed = nm_distn_detailed,
                                      n_draw = n_draw,
                                      args = args,
                                      observed_obs = observed_obs,
                                      offset = offset,
                                      outcome_true = outcome_true,
                                      prob_obs_given_true = prob_obs_given_true)
  ans <- integer(length = n_draw)
  for (i_draw in seq_len(n_draw)) {
    ans[[i]] <- sample(x = outcome_true,
                       size = 1L,
                       prob = prob_obs_true[, i_draw])
  }
  ans
}
  


## calculate 'prob_obs_true', the
## (unnormalised) probability of
## each possible value of true outcome
make_prob_obs_true <- function(nm_distn_detailed,
                               n_draw,
                               args,
                               observed,
                               offset,
                               outcome_true,
                               prob_obs_given_true) {
  n_poss <- length(outcome_true)
  outcome_true <- rep.int(outcome_true, times = n_draw)
  prob_obs_given_true <- rep.int(prob_obs_given_true, times = n_draw)
  if (nm_distn_detailed == "pois") {
    lambda <- rep(args$lambda, each = n_poss)
    prob_prior <- stats::rpois(n = length(lambda),
                               lambda = lambda)
  }
  else if (nm_distn_detailed == "nbinom") {
    shape <- rep(args$shape, each = n_poss)
    rate <- repo(args$rate, each = n_poss)
    prob_prior <- stats::rnbinom(n = length(shape),
                                 shape = shape,
                                 rate = rate)
  }
  else if (nm_distn_detailed == "binom") {
    size <- rep(args$size, each = n_poss)
    prob <- rep(args$prob, each = n_poss)
    prob_prior <- stats::rbinom(n = length(size),
                                size = size,
                                prob = prob)
  }
  else if (nm_distn_detailed == "betabinom") {
    size <- args$size
    shape1 <- args$shape1
    shape2 <- args$shape2
    prob_prior <- stats::rbetabinom(n = length(size),
                                    size = size,
                                    shape1 = shape1,
                                    shape2 = shape2)
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.var nm_distn_detailed}.")
  ans <- prob_obs_given_true * prob_prior
  ans <- matrix(ans, nrow = n_poss, ncol = n_draw)
  ans  
}  
  
  
    


