# Expected tables are built year by year, independently of the replication code.
forecast_label_fixture <- function() {
  data <- expand.grid(age = 1:3, region = c("a", "b"), time = 2020:2022,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  data <- subset(data, !(age == 3 & region == "b"))
  data$y <- 10L
  data$exposure <- 100
  data
}

forecast_label_reference <- function(data, labels, keys) {
  cells <- tibble::as_tibble(unique(data[keys]))
  vctrs::vec_rbind(!!!lapply(seq_along(labels), function(i) {
    ans <- cells
    ans$time <- labels[i]
    ans
  }))
}

test_that("forecast labels preserve complete cell/year keys across grid shapes", {
  set.seed(401)
  for (n_cell in c(1L, 2L, 5L)) {
    for (n_year in c(1L, 2L, 3L, 6L)) {
      data <- forecast_label_fixture()
      cells <- unique(data[c("age", "region")])[seq_len(n_cell), ]
      data <- merge(data, cells, by = c("age", "region"), sort = FALSE)
      data <- data[sample(nrow(data)), ]
      # A lightweight model list isolates the helper from prior restrictions
      # on single-level classification variables.
      mod <- list(data = tibble::as_tibble(data), formula = y ~ age * region + time,
                  var_time = "time", formula_covariates = NULL)
      class(mod) <- "bage_mod"
      labels <- seq.int(2023L, length.out = n_year)
      obtained <- make_data_forecast_labels(mod, labels)
      reference <- forecast_label_reference(data, labels, c("age", "region"))
      expect_identical(obtained[names(reference)], reference)
      expect_equal(nrow(obtained), n_cell * n_year)
      expect_equal(anyDuplicated(obtained[names(reference)]), 0L)
      expect_identical(names(obtained), names(data))
      expect_identical(obtained$y, rep(NA_integer_, nrow(reference)))
      expect_identical(obtained$exposure, rep(NA_real_, nrow(reference)))
    }
  }
})

test_that("forecast labels preserve cells in randomized incomplete panels", {
  set.seed(402)
  for (i in seq_len(20L)) {
    data <- forecast_label_fixture()
    data <- data[sample(nrow(data), sample(4:nrow(data), 1)), ]
    data <- rbind(data, data[1, ])
    mod <- list(data = tibble::as_tibble(data), formula = y ~ region * age + time,
                var_time = "time", formula_covariates = NULL)
    class(mod) <- "bage_mod"
    labels <- sample(2023:2030, sample(1:5, 1))
    reference <- forecast_label_reference(data, labels, c("region", "age"))
    obtained <- make_data_forecast_labels(mod, labels)
    expect_identical(obtained[names(reference)], reference)
  }
})

test_that("forecast labels preserve supported classes and covariate alignment", {
  for (type in c("integer", "double", "character", "factor", "Date")) {
    data <- forecast_label_fixture()
    convert <- switch(type, integer = as.integer, double = as.double,
                       character = as.character, factor = factor,
                       Date = function(x) as.Date(paste0(x, "-01-01")))
    data$time <- convert(data$time)
    data$region <- factor(data$region, levels = c("b", "a", "unused"))
    data$income <- data$age * 3 + as.integer(data$region)
    data$group <- factor(ifelse(data$age == 1, "low", "high"))
    mod <- mod_pois(y ~ age * region + time, data = data, exposure = exposure) |>
      set_covariates(~ income + group)
    labels <- convert(2023:2025)
    obtained <- make_data_forecast_labels(mod, labels)
    reference <- forecast_label_reference(data, labels, c("age", "region"))
    if (is.factor(data$time))
      reference$time <- factor(reference$time,
                               levels = union(levels(data$time), levels(labels)))
    expect_identical(obtained[names(reference)], reference)
    expect_identical(obtained$income,
                     obtained$age * 3 + as.integer(obtained$region))
    expect_identical(obtained$group,
                     factor(ifelse(obtained$age == 1, "low", "high")))
    expect_identical(levels(obtained$region), levels(data$region))
  }
})

test_that("time-only forecast labels produce one row per year", {
  data <- tibble::tibble(time = 2020:2022, y = 1L)
  mod <- mod_pois(y ~ time, data = data, exposure = NULL)
  obtained <- make_data_forecast_labels(mod, 2023:2025)
  expect_identical(obtained, tibble::tibble(time = 2023:2025, y = NA_integer_))
})

test_that("invalid forecast labels fail before constructing cells", {
  data <- forecast_label_fixture()
  mod <- mod_pois(y ~ age * region + time, data = data, exposure = exposure)
  for (labels in list(integer(), character()))
    expect_error(make_data_forecast_labels(mod, labels), "at least one")
  for (labels in list(c(2023, NA), NA_character_, NaN))
    expect_error(make_data_forecast_labels(mod, labels), "missing")
  for (labels in list(Inf, -Inf, "Inf", "-Inf"))
    expect_error(make_data_forecast_labels(mod, labels), "infinite")
  for (labels in list(c(2023, 2023), c("2023", "02023")))
    expect_error(make_data_forecast_labels(mod, labels), "duplicated")
  for (labels in list(2022:2023, c("02022", "2023")))
    expect_error(make_data_forecast_labels(mod, labels), "already present")
  for (labels in list(list(2023), matrix(2023), TRUE))
    expect_error(make_data_forecast_labels(mod, labels), "must be a")
  mod$data$time <- as.Date(paste0(mod$data$time, "-01-01"))
  expect_error(make_data_forecast_labels(mod, list(2023)), "must be a")
  expect_error(make_data_forecast_labels(mod, 2023), "combine")
})

test_that("public labels and independently supplied newdata give identical forecasts", {
  set.seed(403)
  data <- forecast_label_fixture()
  data$income <- data$age * 2 + match(data$region, c("a", "b"))
  mod <- mod_pois(y ~ age * region + time, data = data, exposure = exposure) |>
    set_n_draw(10) |>
    set_covariates(~ income) |>
    fit()
  newdata <- forecast_label_reference(data, 2023:2025, c("age", "region", "income"))
  obtained <- forecast(mod, labels = 2023:2025)
  reference <- forecast(mod, newdata = newdata)
  expect_identical(obtained, reference)
  expect_identical(obtained[c("age", "region", "time")],
                   newdata[c("age", "region", "time")])
  expect_identical(forecast(mod, labels = 2023:2025, rows = age == 2),
                   forecast(mod, newdata = newdata, rows = age == 2))
  expect_identical(forecast(mod, labels = 2023:2025, include_estimates = TRUE),
                   vctrs::vec_rbind(augment(mod), reference))
  expect_identical(forecast(mod, labels = as.character(2023:2025)), reference)
})


test_that("large numeric labels are not coerced to missing integers", {
  data <- forecast_label_fixture()
  mod <- mod_pois(y ~ age * region + time, data = data, exposure = exposure)
  obtained <- make_data_forecast_labels(mod, "2147483648")
  expect_identical(obtained$time, rep(2147483648, 5L))
})
