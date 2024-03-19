
## 'ssvd_comp' ----------------------------------------------------------

test_that("'ssvd_comp' performs correctly with valid inputs - log", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rgamma(nrow(x), shape = 0.2)
    x <- poputils::to_matrix(x,
                             rows = c(age, sex),
                             cols = c(country, year), measure = value)
    ans <- ssvd_comp(x = x, transform = "log")
    expect_identical(names(ans), c("matrix", "offset"))
    expect_identical(dim(ans$matrix), c(18L, 10L))
    expect_identical(length(ans$offset), 18L)
    expect_false(any(vapply(ans, anyNA, FALSE)))
    expect_false(any(vapply(ans, function(x) any(is.infinite(x)), FALSE)))
})

test_that("'ssvd_comp' performs correctly with valid inputs - logit", {
    set.seed(0)
    x <- expand.grid(age = 0:9,
                     sex = c("F", "M", "T"),
                     year = 2000:2005,
                     country = letters)
    x$value <- rbeta(nrow(x), shape1 = 2, shape2 = 3)
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- ssvd_comp(x = x, transform = "logit")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
})

test_that("'ssvd_comp' performs correctly with valid inputs - none", {
    set.seed(0)
    x <- expand.grid(age = 0:9,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- ssvd_comp(x = x, transform = "none")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
})

test_that("'ssvd_comp' gives expected error when too few columns", {
    set.seed(0)
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = c(age, sex), cols = c(year, country), measure = value)
    expect_error(ssvd_comp(x = x, transform = "none"),
                 "`x` does not have enough columns.")
})

test_that("'ssvd_comp' gives expected error when too few columns", {
    set.seed(0)
    x <- expand.grid(age = 0:2,
                     sex = c("F", "M", "T"),
                     year = 2000:2011,
                     country = c("a", "b"))
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = c(age, sex), cols = c(year, country), measure = value)
    expect_error(ssvd_comp(x = x, transform = "none"),
                 "`x` does not have enough rows.")
})

test_that("'ssvd_comp' gives expected error when negative values", {
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- c(-2, -2, rep(0.1, times = nrow(x) - 2))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    expect_error(ssvd_comp(x = x, transform = "log"),
                 "`transform` is \"log\" but `x` has 2 negative values")
})

test_that("'ssvd_comp' gives expected error when negative values", {
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- c(2, 2, rep(0.1, times = nrow(x) - 2))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    expect_error(ssvd_comp(x = x, transform = "logit"),
                 "`transform` is \"logit\" but `x` has 2 values greater than 1.")
})


## 'replace_zeros' ------------------------------------------------------------

test_that("'replace_zeros' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 2), nrow = 5)
    x[sample(100, 10)] <- 0
    ans_obtained <- replace_zeros(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained[x > 0] == x[x > 0]))
})


## 'replace_zeros_ones' -------------------------------------------------------

test_that("'replace_zeros_ones' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 1), nrow = 5)
    x[sample(100, 5)] <- 0
    x[sample(100, 5)] <- 1
    ans_obtained <- replace_zeros_ones(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained < 1))
    expect_true(all(ans_obtained[(x > 0) & (x < 1)] == x[(x > 0) & (x < 1)]))
})


    
## 'hmd_unzip' ----------------------------------------------------------------

test_that("'hmd_unzip' works with valid inputs - unzipped has hmd_statistics_test folder", {
  fn <- file.path("data_for_tests", "hmd_statistics_test.zip")
  ans <- hmd_unzip(fn)
  expect_setequal(names(ans),
                  c("country", "Year", "Age", "mx", "Lx", "sex", "type_age"))
})

test_that("'hmd_unzip' works with valid inputs - unzipped does not have hmd_statistics_test folder", {
  fn <- file.path("data_for_tests", "hmd_statistics_test_v2.zip")
  ans <- hmd_unzip(fn)
  expect_setequal(names(ans),
                  c("country", "Year", "Age", "mx", "Lx", "sex", "type_age"))
})



## 'hmd_tidy_data' ------------------------------------------------------------

test_that("'hmd_tidy_data' works with valid inputs", {
  data <- tibble::tribble(~Age, ~Year, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          0,    2001,  "both",   "a",      "1x1",     0.1, 2,
                          1,    2002,  "female", "b",      "5x1",     0.1, 2,
                          2,    2003,  "male",   "c",      "1x1",     0.1, 2,
                          3,    2004,  "both",   "d",      "1x1",     NA,  0)
  ans_obtained <- hmd_tidy_data(data)
  expect_setequal(names(ans_obtained),
                  c("age", "time", "sex", "country", "type_age", "mx", "Lx"))
  expect_identical(nrow(ans_obtained), 3L)
  expect_identical(ans_obtained$sex,
                   factor(c("Total", "Female", "Male"),
                          levels = c("Total", "Female", "Male")))
  expect_identical(ans_obtained$type_age,
                   factor(c("single", "lt", "single"),
                          levels = c("single", "five", "lt")))
})


## 'hmd_aggregate_mx_Lx' ------------------------------------------------------

test_that("'hmd_aggregate_mx_Lx' works with valid inputs", {
  data <- tibble::tribble(~age, ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          "0",  2001,  "Total",   "a",      "single",   0.1, 2,
                          "0",  2001,  "Total",   "a",      "single",   0.2, 3,
                          "1",  2001,  "Total",   "a",      "single",   0.1, 0,
                          "1",  2001,  "Total",   "a",      "single",   0.2, 0,
                          "0",  2001,  "Total",   "a",      "lt",       0.1, 2,
                          "0",  2001,  "Total",   "a",      "lt",       0.2, 3,
                          "1-4",2001,  "Total",   "a",      "lt",       0.1, 0,
                          "1-4",2001,  "Total",   "a",      "lt",       0.2, 0)
  ans_obtained <- hmd_aggregate_mx_Lx(data)
  ans_expected <-
    tibble::tribble(~age, ~time, ~sex,     ~country, ~type_age, ~mx,            ~Lx,
                    "0",  2001,  "Total",   "a",      "single", 0.4*0.1+0.6*0.2, 2+3,
                    "1",  2001,  "Total",   "a",      "single", 0.5*0.1 + 0.5*0.2, 0+0,
                    "0",  2001,  "Total",   "a",      "lt",     0.4*0.1+0.6*0.2, 2+3,
                    "1-4",2001,  "Total",   "a",      "lt",     0.5*0.1 + 0.5*0.2, 0+0)
  expect_identical(ans_obtained, ans_expected)
})


## 'hmd_add_age_five' ---------------------------------------------------------

test_that("'hmd_aggregate_mx_Lx' works with valid inputs", {
  data <- tibble::tribble(~age,  ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          "0",     2001,  "Total",   "a",      "lt",   0.1, 2,
                          "1-4", 2001,  "Total",   "a",      "lt",   0.2, 3,
                          "5-9", 2001,  "Total",   "a",      "lt",   0.4, 4)
  ans_obtained <- hmd_add_age_five(data)
  ans_expected <-
    tibble::tribble(~age,  ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                    "0",   2001,  "Total",   "a",      "lt",   0.1, 2,
                    "1-4", 2001,  "Total",   "a",      "lt",   0.2, 3,
                    "5-9", 2001,  "Total",   "a",      "lt",   0.4, 4,
                    "0-4", 2001,  "Total",   "a",      "five", 0.4*0.1+0.6*0.2, 5,
                    "5-9", 2001,  "Total",   "a",      "five", 0.4, 4)
  expect_identical(ans_obtained, ans_expected)
})


## 'hmd_vary_age_open' --------------------------------------------------------

test_that("'hmd_vary_age_open' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "five"),
                expand.grid(age = poputils::age_labels(type = "lt", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "lt"))
  data$mx <- runif(n = nrow(data))
  data$Lx <- runif(n = nrow(data), max = 5)
  ans_obtained <- hmd_vary_age_open(data)
  expect_true(all(paste0(seq(from = 60, 110, by = 5), "+") %in% ans_obtained$age))
  expect_identical(unique(ans_obtained$age_open),
                   seq.int(from = 60L, to = 110L, by = 5L))
})


## 'hmd_vary_age_open_type_age' -----------------------------------------------

test_that("'hmd_vary_age_open_type_age' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "five", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "five")
  data$mx <- runif(n = nrow(data))
  data$Lx <- runif(n = nrow(data), max = 5)
  ans_obtained <- hmd_vary_age_open_type_age(data)
  expect_true(all(paste0(seq(from = 60, 110, by = 5), "+") %in% ans_obtained$age))
  expect_identical(unique(ans_obtained$age_open),
                   seq.int(from = 60L, to = 110L, by = 5L))
})


## 'hmd_total' ----------------------------------------------------------------

test_that("'hmd_total' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = "Total",
                            time = 2001:2010,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = "Total",
                            time = 2001:2010,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_total(data, transform = "none")
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})


## 'hmd_joint' ----------------------------------------------------------------

test_that("'hmd_joint' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_joint(data, transform = "log")
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(.mapply(paste,
                           dots = list(x = ans_obtained$labels_sexgender,
                                       y = ans_obtained$labels_age),
                           MoreArgs = list(sep = ".")),
                   lapply(ans_obtained$matrix, rownames))
})


## 'hmd_indep' ---------------------------------------------------------

test_that("'hmd_indep' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_indep(data, transform = "logit")
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(.mapply(paste,
                           dots = list(x = ans_obtained$labels_sexgender,
                                       y = ans_obtained$labels_age),
                           MoreArgs = list(sep = ".")),
                   lapply(ans_obtained$matrix, rownames))
})


## 'ssvd_hmd' -----------------------------------------------------------------

test_that("'ssvd_hmd' works with valid inputs", {
  fn <- file.path("data_for_tests", "hmd_statistics_test.zip")
  suppressMessages(ans <- ssvd_hmd(fn, transform = "log"))
  expect_s3_class(ans, "bage_ssvd")
})

