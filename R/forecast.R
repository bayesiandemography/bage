## HAS_TESTS
#' Standardize Forecasted 'components' Values
#'
#' Standardization consists of adding the quantity
#' (standardized_val_final_est_period - unstandardized_val_final_est_period)
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
align_forecast <- function(mod,
                                 comp_forecast,
                                 comp_est_st,
                                 comp_est_unst,
                                 labels_forecast) {
  ## obtain values for 'term' and 'level' for final period of estimates
  term_level_effect <- make_term_level_final_time_effect(mod)
  term_level_svd <- make_term_level_final_time_svd(mod)
  term_level <- vctrs::vec_rbind(term_level_effect, term_level_svd)
  ## create mapping between levels for final period of estimates and levels for forecasts
  mapping_effect <- make_mapping_final_time_effect(mod = mod, labels_forecast = labels_forecast)
  mapping_svd <- make_mapping_final_time_svd(mod = mod, labels_forecast = labels_forecast)
  mapping <- vctrs::vec_rbind(mapping_effect, mapping_svd)
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
  ord <- order(match(ans[[1L]], comp_forecast[[1L]]),
               match(ans[[2L]], comp_forecast[[2L]]),
               match(ans[[3L]], comp_forecast[[3L]]))
  ans <- ans[ord, ]
  ans <- tibble::tibble(ans)
  ## return
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
#' @param intercept Intercepts of line(s). An rvec.
#' @param slope Slope of line(s). An rvec.
#' @param sd Standard devation of errors. An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_lin <- function(intercept,
                         slope,
                         sd,
                         matrix_along_by_est,
                         matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  s <- seq.int(from = n_along_est + 1L, length.out = n_along_forecast)
  ans <- rep(intercept[[1L]], times = n_along_forecast * n_by)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- rvec::rnorm_rvec(n = n_along_forecast,
                                   mean = intercept[i_by] + s * slope[i_by],
                                   sd = sd)
  }
  ans
}


## HAS_TESTS
#' Forecast Trend Component of Line or Lines
#'
#' @param intercept Intercepts of line(s). An rvec.
#' @param slope Slope of line(s). An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estimates
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_lin_trend <- function(intercept,
                               slope,
                               matrix_along_by_est,
                               matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  s <- seq.int(from = n_along_est + 1L, length.out = n_along_forecast)
  ans <- rep(intercept[[1L]], times = n_along_forecast * n_by)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- intercept[i_by] + s * slope[i_by]
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
#' Convert Dimnames for Estimates into Dimnames for Forecasts
#'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param labels_forecast Vector
#' with labels for future time periods.
#' @param time_only If TRUE, only keep terms
#' that involve time
#'
#' @returns Modified version of 'dimnames_terms'
#'
#' @noRd
make_dimnames_terms_forecast <- function(dimnames_terms,
                                         var_time,
                                         labels_forecast,
                                         time_only) {
  labels_forecast <- as.character(labels_forecast)
  for (i in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    has_time <- var_time %in% nm_split
    if (has_time)
      dimnames_terms[[i]][[var_time]] <- labels_forecast
    else {
      if (time_only)
        dimnames_terms[i] <- list(NULL)
    }
  }
  is_null <- vapply(dimnames_terms, is.null, FALSE)
  dimnames_terms <- dimnames_terms[!is_null]
  dimnames_terms
}
    

## HAS_TESTS
#' Make Mapping Between 'level' Value from Final Year of Estimates
#' and 'level' Value from Forecasts - On Original Scale
#'
#' Helper function for 'align_forecast'
#'
#' @param mod Objec of class 'bage_mod'
#' @param labels_forecast Vector
#'
#' @returns Tibble
#'
#' @noRd
make_mapping_final_time_effect <- function(mod, labels_forecast) {
  var_time <- mod$var_time
  dimnames_terms <- mod$dimnames_terms
  ans <- vector(mode = "list", length = length(dimnames_terms))
  final_time <- utils::tail(dimnames_terms[[var_time]][[var_time]], n = 1L)
  final_time <- as.character(final_time)
  for (i_term in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    has_time <- var_time %in% nm_split
    if (has_time) {
      dimnames_term[[var_time]] <- as.character(labels_forecast)
      level <- vctrs::vec_expand_grid(!!!dimnames_term)
      level_final <- replace(level, var_time, final_time)
      level <- Reduce(paste_dot, level)
      level_final <- Reduce(paste_dot, level_final)
      val <- tibble::tibble(level = level,
                            level_final = level_final)
      ans[[i_term]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}


## HAS_TESTS
#' Make Mapping Between 'level' Value from Final Year of Estimates
#' and 'level' Value from Forecasts - On SVD Scale
#'
#' Helper function for 'align_forecast'
#'
#' @param mod Objec of class 'bage_mod'
#' @param labels_forecast Vector
#'
#' @returns Tibble
#'
#' @noRd
make_mapping_final_time_svd <- function(mod, labels_forecast) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  ans <- vector(mode = "list", length = length(dimnames_terms))
  final_time <- utils::tail(dimnames_terms[[var_time]][[var_time]], n = 1L)
  final_time <- as.character(final_time)
  for (i_term in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i_term]]
    prior <- priors[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    has_time <- var_time %in% nm_split
    is_svd <- is_svd(prior)
    if (has_time && is_svd) {
      dimnames_term[[var_time]] <- as.character(labels_forecast)
      labels_svd <- get_labels_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   var_sexgender = var_sexgender)
      non_agesex <- setdiff(nm_split, c(var_age, var_sexgender))
      dimnames_nonagesex <- dimnames_term[non_agesex]
      dimnames_svd <- c(list(.svd = labels_svd), dimnames_nonagesex)
      level <- vctrs::vec_expand_grid(!!!dimnames_svd)
      level_final <- replace(level, var_time, final_time)
      level <- Reduce(paste_dot, level)
      level_final <- Reduce(paste_dot, level_final)
      val <- tibble::tibble(level = level,
                            level_final = level_final)
      ans[[i_term]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}



## HAS_TESTS
#' Extract the 'term' and 'level' Values for the Last Period
#' of 'components' - on Original Scale
#'
#' Helper function for 'align_forecast'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
make_term_level_final_time_effect <- function(mod) {
  var_time <- mod$var_time
  dimnames_terms <- mod$dimnames_terms
  ans <- vector(mode = "list", length = length(dimnames_terms))
  for (i_term in seq_along(ans)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    nm <- dimnames_to_nm(dimnames_term)
    has_time <- var_time %in% nm_split
    if (has_time) {
      dimnames_term[[var_time]] <- utils::tail(dimnames_term[[var_time]], 1L)
      level <- vctrs::vec_expand_grid(!!!dimnames_term)
      level <- Reduce(paste_dot, level)
      val <- tibble::tibble(term = nm,
                            level = level)
      ans[[i_term]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}

## HAS_TESTS
#' Extract the 'term' and 'level' Values for the Last Period
#' of 'components' - On SVD Scale
#'
#' Helper function for 'align_forecast'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
make_term_level_final_time_svd <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  ans <- vector(mode = "list", length = length(dimnames_terms))
  for (i_term in seq_along(ans)) {
    prior <- priors[[i_term]]
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    nm <- dimnames_to_nm(dimnames_term)
    has_time <- var_time %in% nm_split
    is_svd <- is_svd(prior)
    if (has_time && is_svd) {
      dimnames_term[[var_time]] <- utils::tail(dimnames_term[[var_time]], 1L)
      labels_svd <- get_labels_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   var_sexgender = var_sexgender)
      non_agesex <- setdiff(nm_split, c(var_age, var_sexgender))
      dimnames_nonagesex <- dimnames_term[non_agesex]
      dimnames_svd <- c(list(.svd = labels_svd), dimnames_nonagesex)
      level <- vctrs::vec_expand_grid(!!!dimnames_svd)
      level <- Reduce(paste_dot, level)
      val <- tibble::tibble(term = nm,
                            level = level)
      ans[[i_term]] <- val
    }
  }
  vctrs::vec_rbind(!!!ans)
}


