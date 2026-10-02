# Independent reference: formulas used before the blocked implementation.
fitted_draw_reference <- function(distribution, outcome, offset, expected, disp) {
  n <- length(expected)
  n_draw <- rvec::n_draw(expected)
  expand <- function(x) {
    if (rvec::is_rvec(x)) as.numeric(x) else rep(x, times = n_draw)
  }
  outcome <- expand(outcome)
  offset <- expand(offset)
  missing <- is.na(outcome) | is.na(offset)
  outcome[missing] <- 0
  offset[missing] <- 0
  expected <- as.numeric(expected)
  disp <- rep(as.numeric(disp), each = n)
  if (distribution == "pois")
    ans <- stats::rgamma(length(expected), shape = outcome + 1 / disp,
                         rate = offset + 1 / (disp * expected))
  else
    ans <- stats::rbeta(length(expected), shape1 = outcome + expected / disp,
                        shape2 = offset - outcome + (1 - expected) / disp)
  rvec::rvec_dbl(matrix(ans, nrow = n, ncol = n_draw))
}

test_that("blocked fitted draws preserve values and RNG across input forms", {
  set.seed(404)
  n <- 6L
  n_draw <- 7L
  outcome <- c(0, 1, NA, 3, 4, 5)
  offset <- c(10, NA, 30, 40, 50, 60)
  expected <- rvec::rvec_dbl(matrix(runif(n * n_draw, 0.05, 0.8), n))
  disp <- rvec::rvec_dbl(matrix(seq(0.1, 0.7, length.out = n_draw), 1))
  outcome_rvec <- rvec::rvec_dbl(matrix(rep(outcome, n_draw), n))
  offset_rvec <- rvec::rvec_dbl(matrix(rep(offset, n_draw), n))
  # Missingness in just one draw must not blank the whole observation.
  m <- as.matrix(outcome_rvec)
  m[4, 5] <- NA
  outcome_rvec <- rvec::rvec_dbl(m)
  cases <- list(list(outcome, offset), list(outcome_rvec, offset),
                list(outcome, offset_rvec), list(outcome_rvec, offset_rvec),
                list(2, 100), list(c(1, 2), c(10, 20)))
  for (chunk in c(1L, 3L, 7L)) {
    local_mocked_bindings(chunk_size_linpred = function(n_row, n_draw) chunk)
    for (distribution in c("pois", "binom")) {
      mod <- structure(list(), class = paste0("bage_mod_", distribution))
      for (case in cases) {
        set.seed(405)
        obtained <- draw_fitted_given_outcome(mod, case[[1]], case[[2]], expected, disp)
        rng <- .Random.seed
        set.seed(405)
        reference <- fitted_draw_reference(distribution, case[[1]], case[[2]], expected, disp)
        expect_identical(obtained, reference)
        expect_identical(rng, .Random.seed)
      }
    }
  }
})

test_that("blocked fitted draws support a single draw and zero observations", {
  for (n in c(0L, 1L, 4L)) {
    for (distribution in c("pois", "binom")) {
      mod <- structure(list(), class = paste0("bage_mod_", distribution))
      expected <- rvec::rvec_dbl(matrix(0.2, n, 1))
      outcome <- rep(2, n)
      offset <- rep(10, n)
      disp <- rvec::rvec_dbl(matrix(0.3, 1, 1))
      set.seed(406)
      obtained <- draw_fitted_given_outcome(mod, outcome, offset, expected, disp)
      rng <- .Random.seed
      set.seed(406)
      reference <- fitted_draw_reference(distribution, outcome, offset, expected, disp)
      expect_identical(obtained, reference)
      expect_identical(rng, .Random.seed)
    }
  }
})

test_that("blocked linear predictor matches direct term expansion", {
  local_mocked_bindings(chunk_size_linpred = function(n_row, n_draw) min(3L, n_draw))
  set.seed(407)
  data <- expand.grid(age = 1:3, sex = c("F", "M"), time = 2020:2022)
  data$y <- 10
  data$income <- seq_len(nrow(data))
  for (n_draw in c(1L, 7L)) {
    for (with_covariates in c(FALSE, TRUE)) {
      mod <- mod_pois(y ~ age * sex + time, data = data, exposure = NULL) |>
        set_n_draw(n_draw)
      if (with_covariates) mod <- set_covariates(mod, ~ income)
      comp <- components(mod, quiet = TRUE)
      # Directly sum expanded terms using rvec operations, as before blocking.
      reference <- rvec::rvec_dbl(matrix(0, nrow(data), n_draw))
      data_terms <- mod$data
      data_terms[["(Intercept)"]] <- "(Intercept)"
      for (dn in mod$dimnames_terms) {
        term <- dimnames_to_nm(dn)
        lev <- dimnames_to_levels(dn)
        term_values <- comp$.fitted[match(paste(term, "effect", lev),
                                          with(comp, paste(term, component, level)))]
        cell_keys <- Reduce(paste_dot, data_terms[dimnames_to_nm_split(dn)])
        reference <- reference + term_values[match(cell_keys, lev)]
      }
      if (with_covariates) {
        x <- make_matrix_covariates(mod$formula_covariates, mod$data, rows = NULL)
        coefs <- comp$.fitted[comp$term == "covariates" & comp$component == "coef"]
        reference <- reference + x %*% coefs
      }
      for (rows in list(NULL, c(9L, 2L, 9L, 1L), integer())) {
        obtained <- make_linpred_from_components(mod, comp, mod$data,
                                                  mod$dimnames_terms, rows)
        wanted <- if (is.null(rows)) reference else reference[rows]
        expect_equal(obtained, wanted, tolerance = 1e-14)
      }
    }
  }
})
