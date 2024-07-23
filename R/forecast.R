
## HAS_TESTS
#' Estimate Values on a Line or Lines
#'
#' @param slope Slope(s) of line(s). An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#'
#' @returns An rvec
#'
#' @noRd
estimate_lin <- function(slope,
                         matrix_along_by_est) {
  n_along <- nrow(matrix_along_by_est)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(slope[[1L]], times = n_along * n_by)
  q <- seq(from = -1, to = 1, length.out = n_along)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_est[, i_by] + 1L
    ans[i_ans] <- slope[i_by] * q
  }
  ans
}


## HAS_TESTS
#' Forecast an AR Process
#'
#' @param ar_est Historical estimates. An rvec.
#' @param coef AR coefficients. An rvec.
#' @param sd Standard deviation for AR model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_ar <- function(ar_est,
                        coef,
                        sd,
                        matrix_along_by_est,
                        matrix_along_by_forecast) {
  n_coef <- length(coef)
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(ar_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(ar_est[[1L]], times = n_along_forecast + n_coef)
  s_head <- seq_len(n_coef)
  s_tail <- seq(to = n_along_est, length.out = n_coef)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L ## matrix uses 0-based index
    tmp[s_head] <- ar_est[i_tail]
    for (j in seq_len(n_along_forecast)) {
      s_ar <- seq(from = j, to = j + n_coef - 1L)
      mean <- sum(coef * tmp[s_ar])
      tmp[[j + n_coef]] <- rvec::rnorm_rvec(n = 1L, mean = mean, sd = sd)
    }
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Forecast SVD Coefficients that Follow an AR Process
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sex/gender variable, or NULL
#' @param components Tibble with with output
#' from function 'components'
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns An rvec
#'
#' @noRd
forecast_ar_svd <- function(prior,
                            dimnames_term,
                            dimnames_forecast,
                            var_time,
                            var_age,
                            var_sexgender,
                            components,
                            labels_forecast) {
  matrix_along_by_est <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                             dimnames_term = dimnames_term,
                                                             var_time = var_time,
                                                             var_age = var_age,
                                                             var_sexgender = var_sexgender)
  matrix_along_by_forecast <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                                  dimnames_term = dimnames_forecast,
                                                                  var_time = var_time,
                                                                  var_age = var_age,
                                                                  var_sexgender = var_sexgender)
  nm <- dimnames_to_nm(dimnames_term)
  is_svd <- with(components, term == nm & component == "svd")
  is_coef <- with(components, term == nm & component == "hyper" & startsWith(level, "coef"))
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  svd <- components$.fitted[is_svd]
  coef <- components$.fitted[is_coef]
  sd <- components$.fitted[is_sd]
  forecast_ar(ar_est = svd,
              coef = coef,
              sd = sd,
              matrix_along_by_est = matrix_along_by_est,
              matrix_along_by_forecast = matrix_along_by_forecast)
}


## HAS_TESTS
#' Forecast Time-Varying Effects in 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param components_est Tibble with results
#' of call to 'components'.
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns A tibble
#'
#' @noRd
forecast_components <- function(mod,
                                components_est,
                                labels_forecast) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  is_time_varying <- vapply(dimnames_terms, function(x) var_time %in% names(x), TRUE)
  ans <- .mapply(forecast_term,
                 dots = list(prior = priors[is_time_varying],
                             dimnames_term = dimnames_terms[is_time_varying]),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender,
                                 components = components_est,
                                 labels_forecast = labels_forecast))
  ans <- vctrs::vec_rbind(!!!ans)
  ans <- sort_components(ans, mod = mod)
  ans
}



## HAS_TESTS
#' Forecast Line or Lines
#'
#' @param slope Slope of line(s).
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_lin <- function(slope,
                         matrix_along_by_est,
                         matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(slope[[1L]], times = n_along_forecast * n_by)
  incr_q <- 2 / (n_along_est - 1)
  q <- seq(from = 1 + incr_q,
           by = incr_q,
           length.out = n_along_forecast)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- slope[i_by] * q
  }
  ans
}


## HAS_TESTS
#' Forecast a Random Walk
#'
#' @param rw_est Historical estimates. An rvec.
#' @param sd Standard deviation for RW model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw <- function(rw_est,
                        sd,
                        matrix_along_by_est,
                        matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(rw_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(rw_est[[1L]], times = n_along_forecast + 1L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    tmp[[1L]] <- rw_est[[i_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 1L]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-1L]
  }
  ans
}


## HAS_TESTS
#' Forecast SVD Cofficients that Follow a Random Walk
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sex/gender variable, or NULL
#' @param components Tibble with with output
#' from function 'components'
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw_svd <- function(prior,
                            dimnames_term,
                            dimnames_forecast,
                            var_time,
                            var_age,
                            var_sexgender,
                            components,
                            labels_forecast) {
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_est <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                             dimnames_term = dimnames_term,
                                                             var_time = var_time,
                                                             var_age = var_age,
                                                             var_sexgender = var_sexgender)
  matrix_along_by_forecast <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                                  dimnames_term = dimnames_forecast,
                                                                  var_time = var_time,
                                                                  var_age = var_age,
                                                                  var_sexgender = var_sexgender)
  is_svd <- with(components, term == nm & component == "svd")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  svd <- components$.fitted[is_svd]
  sd <- components$.fitted[is_sd]
  forecast_rw(rw_est = svd,
              sd = sd,
              matrix_along_by_est = matrix_along_by_est,
              matrix_along_by_forecast = matrix_along_by_forecast)
}


## HAS_TESTS
#' Forecast a Second Order Random Walk
#'
#' @param rw2_est Historical estimates. An rvec.
#' @param sd Standard deviation for RW model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw2 <- function(rw2_est,
                         sd,
                         matrix_along_by_est,
                         matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)  
  ans <- rep(rw2_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(rw2_est[[1L]], times = n_along_forecast + 2L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    i_second_last <- matrix_along_by_est[n_along_est - 1L, i_by] + 1L
    tmp[[2L]] <- rw2_est[[i_last]]
    tmp[[1L]] <- rw2_est[[i_second_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 2L]] <- rvec::rnorm_rvec(n = 1L,
                                        mean = 2 * tmp[[j + 1L]] - tmp[[j]],
                                        sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-(1:2)]
  }
  ans
}


## HAS_TESTS
#' Forecast SVD Cofficients that Follow a Random Walk
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sex/gender variable, or NULL
#' @param components Tibble with with output
#' from function 'components'
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw2_svd <- function(prior,
                             dimnames_term,
                             dimnames_forecast,
                             var_time,
                             var_age,
                             var_sexgender,
                             components,
                             labels_forecast) {
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_est <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                             dimnames_term = dimnames_term,
                                                             var_time = var_time,
                                                             var_age = var_age,
                                                             var_sexgender = var_sexgender)
  matrix_along_by_forecast <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                                  dimnames_term = dimnames_forecast,
                                                                  var_time = var_time,
                                                                  var_age = var_age,
                                                                  var_sexgender = var_sexgender)
  is_svd <- with(components, term == nm & component == "svd")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  svd <- components$.fitted[is_svd]
  sd <- components$.fitted[is_sd]
  forecast_rw2(rw2_est = svd,
               sd = sd,
               matrix_along_by_est = matrix_along_by_est,
               matrix_along_by_forecast = matrix_along_by_forecast)
}


## HAS_TESTS
#' Forecast Fixed Seasonal Effects
#'
#' Use first 'n_seas' entries from historical
#' estimates, within each combination of 'by' variables
#'
#' @param n_seas Number of seasons.
#' @param seas_est Historical estimates. An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_seasfix <- function(n_seas,
                             seas_est,
                             matrix_along_by_est,
                             matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(seas_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(seas_est[[1L]], times = n_along_forecast + n_seas)
  s_head <- seq_len(n_seas)
  s_tail <- seq.int(to = n_along_est, length.out = n_seas)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L
    tmp[s_head] <- seas_est[i_tail]
    for (j in seq_len(n_along_forecast))
      tmp[[j + n_seas]] <- tmp[[j]]
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Forecast Time-Varying Seasonal Effects
#'
#' @param n_seas Number of seasons.
#' @param seas_est Historical estimates. An rvec.
#' @param sd Standard deviation for seasonal effects. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_seasvary <- function(n_seas,
                              seas_est,
                              sd,
                              matrix_along_by_est,
                              matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(seas_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(seas_est[[1L]], times = n_along_forecast + n_seas)
  s_head <- seq_len(n_seas)
  s_tail <- seq.int(to = n_along_est, length.out = n_seas)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L
    tmp[s_head] <- seas_est[i_tail]
    for (j in seq_len(n_along_forecast))
      tmp[[j + n_seas]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Create Extra Rows for 'data', holding Values for
#' Predictor Variables Used in Forecast
#'
#' @param mod Object of class 'bage_mod'
#' @param labels_forecast Vector
#' with labels for future time periods
#'
#' @returns A tibble.
#'
#' @noRd
make_data_forecast <- function(mod, labels_forecast) {
  formula <- mod$formula
  data <- mod$data
  var_time <- mod$var_time
  nms_model <- all.vars(formula[-2L])
  ans <- lapply(data[nms_model], unique)
  ans[[var_time]] <- labels_forecast
  ans <- vctrs::vec_expand_grid(!!!ans)
  ans <- vctrs::vec_rbind(data, ans)
  i_original <- seq_len(nrow(data))
  ans <- ans[-i_original, ]
  ans
}


## HAS_TESTS
#' Make Mapping Between 'level' Value from Final Year of Estimates
#' and 'level' Value from Forecasts
#'
#' Helper function for 'standardize_forecast'
#'
#' @param mod Objec of class 'bage_mod'
#' @param labels_forecast Vector
#'
#' @returns Tibble
#'
#' @noRd
make_mapping_final_time <- function(mod, labels_forecast) {
  var_time <- mod$var_time
  dimnames_terms <- mod$dimnames_terms
  nms_terms <- names(dimnames_terms)
  ans <- vector(mode = "list", length = length(nms_terms))
  final_time <- utils::tail(dimnames_terms[[var_time]][[var_time]], n = 1L)
  final_time <- as.character(final_time)
  for (i in seq_along(dimnames_terms)) {
    nm_term <- nms_terms[[i]]
    nm_term_split <- strsplit(nm_term, split = ":")[[1L]]
    has_time <- var_time %in% nm_term_split
    if (has_time) {
      dn <- dimnames_terms[[i]]
      dn[[var_time]] <- as.character(labels_forecast)
      level <- vctrs::vec_expand_grid(!!!dn)
      level_final <- replace(level, var_time, final_time)
      level <- Reduce(paste_dot, level)
      level_final <- Reduce(paste_dot, level_final)
      val <- tibble::tibble(level = level,
                            level_final = level_final)
      ans[[i]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}


## HAS_TESTS
#' Extract the 'term' and 'level' Values for the Last Period
#' of 'components'
#'
#' Helper function for 'standardize_forecast'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
make_term_level_final_time <- function(mod) {
  var_time <- mod$var_time
  dimnames_terms <- mod$dimnames_terms
  nms_terms <- names(dimnames_terms)
  ans <- vector(mode = "list", length = length(nms_terms))
  for (i in seq_along(dimnames_terms)) {
    nm_term <- nms_terms[[i]]
    nm_term_split <- strsplit(nm_term, split = ":")[[1L]]
    has_time <- var_time %in% nm_term_split
    if (has_time) {
      dn <- dimnames_terms[[i]]
      dn[[var_time]] <- utils::tail(dn[[var_time]], 1L)
      level <- vctrs::vec_expand_grid(!!!dn)
      level <- Reduce(paste_dot, level)
      val <- tibble::tibble(term = nm_term,
                            level = level)
      ans[[i]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}


## HAS_TESTS
#' Standardize trend, cyclical, seasonal, error Terms
#'
#' @param mod Object of class 'bage_mod'
#' @param component Rows from 'components'
#' for a type of component
#'
#' @return A modified version of 'component'
#'
#' @noRd
standardize_component <- function(mod, component) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  terms <- split(x = component, f = component$term)
  nms_terms <- names(terms)
  for (nm_term in nms_terms) {
    prior <- priors[[nm_term]]
    dimnames_term <- dimnames_terms[[nm_term]]
    along <- prior$specific$along ## component must have 'along'
    matrix_along_by <- make_matrix_along_by_effect(along = along,
                                                   dimnames_term = dimnames_term,
                                                   var_time = var_time,
                                                   var_age = var_age)
    terms[[nm_term]]$.fitted <- center_within_across_by(x = terms[[nm_term]]$.fitted,
                                                        matrix_along_by = matrix_along_by)
  }
  vctrs::vec_rbind(!!!terms)
}


## HAS_TESTS
#' Standardize Forecasted 'components' Values
#'
#' Standardization consists of adding the quantity
#' (standardized_val_final_est_period - unstandardized_valu_final_est_period)
#' to all forecasted values - within each combination of the 'by' variables.
#'
#' @param mod Object of class 'bage_mod'
#' @param comp_forecast Unstandardized forecast of 'components'
#' @param comp_est_st Standardized estimates of 'components'
#' @param comp_est_unst Unstandardized estimates of 'components'
#' @param labels_forecast Time labels for forecasted periods
#'
#' @returns A tibble
#' 
#' @noRd
standardize_forecast <- function(mod,
                                 comp_forecast,
                                 comp_est_st,
                                 comp_est_unst,
                                 labels_forecast) {
  ## obtain values for 'term' and 'level' for final period of estimates
  term_level <- make_term_level_final_time(mod)
  ## create mapping between levels for final period of estimates and levels for forecasts
  mapping <- make_mapping_final_time(mod = mod, labels_forecast = labels_forecast)
  ## obtain values of .fitted for time-varying quantities for final period of estimates
  tail_unst <- merge(comp_est_unst, term_level, by = c("term", "level"), sort = FALSE)
  tail_st <- merge(comp_est_st, term_level, by = c("term", "level"), sort = FALSE)
  names(tail_unst)[match(".fitted", names(tail_unst))] <- "nonstandardized"
  names(tail_st)[match(".fitted", names(tail_st))] <- "standardized"
  ## calculate difference between standardized and non-standardized .fitted in final period
  diff <- merge(tail_unst, tail_st, by = c("term", "level", "component"), sort = FALSE)
  diff$diff <- diff$standardized - diff$nonstandardized
  names(diff)[match("level", names(diff))] <- "level_final"
  ## attach differences to forecasts
  ans <- merge(comp_forecast, mapping, by = "level", sort = FALSE)
  ans <- merge(ans, diff, by = c("term", "component", "level_final"), sort = FALSE)
  ## revise forecasts
  ans$.fitted <- ans$.fitted + ans$diff
  ans <- ans[c("term", "component", "level", ".fitted")]
  ans <- tibble::tibble(ans)
  ## return
  ans
}
    
    



  
  

