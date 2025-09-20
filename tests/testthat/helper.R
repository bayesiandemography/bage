
make_small_mod_pois <- function() {
  data <- expand.grid(age = 0:4, time = 2021:2025,
                      KEEP.OUT.ATTRS = FALSE)
  data$popn <- runif(n = nrow(data), min = 1, max = 100)
  data$deaths <- 3 * rpois(n = nrow(data), lambda = 0.2 * data$popn)
  mod_pois(deaths ~ age + time,
           data = data,
           exposure = popn)
}

make_small_mod_binom <- function() {
  data <- expand.grid(age = 0:4, time = 2021:2025,
                      KEEP.OUT.ATTRS = FALSE)
  data$popn <- rpois(n = nrow(data), lambda = 50)
  data$deaths <- pmin(3 * rbinom(n = nrow(data), size = data$popn, prob = 0.1),
                      data$popn)
  mod_binom(deaths ~ age + time,
            data = data,
            size = popn)
}

make_small_mod_norm <- function() {
  data <- expand.grid(age = 0:4, time = 2021:2025,
                      KEEP.OUT.ATTRS = FALSE)
  data$wt <- runif(n = nrow(data), min = 1, max = 100)
  data$income <- rnorm(nrow(data), mean = 1000, sd = 100)
  mod_norm(income ~ age + time,
           data = data,
           weights = wt)
}


sim_ssvd <- function() {
  data <- data.frame(type = c("total", "joint", "indep"))
  data$labels_age <- list(c("0-4", "5-9"),
                          c("0-4", "5-9", "0-4", "5-9"),
                          c("0-4", "5-9", "0-4", "5-9"))
  data$labels_sexgender <- list(NULL,
                                c("Female", "Female", "Male", "Male"),
                                c("Female", "Female", "Male", "Male"))
  data$matrix <- list(Matrix::sparseMatrix(i = rep(1:2, times = 10),
                                           j = rep(1:10, each = 2),
                                           x = 1, 
                                           dimnames = list(c("0-4", "5-9"),
                                                           NULL)),
                      Matrix::sparseMatrix(i = rep(1:4, times = 10),
                                           j = rep(1:10, each = 4),
                                           x = 2, 
                                           dimnames = list(c("Female.0-4",
                                                             "Female.5-9",
                                                             "Male.0-4",
                                                             "Male.5-9"),
                                                           NULL)),
                      Matrix::sparseMatrix(i = rep(1:4, times = 20),
                                           j = rep(1:20, each = 4),
                                           x = 3,
                                           dimnames = list(c("Female.0-4",
                                                             "Female.5-9",
                                                             "Male.0-4",
                                                             "Male.5-9"),
                                                           NULL)))
  data$offset <- list(c("0-4" = 1,
                        "5-9" = 2),
                      c("Female.0-4" = 1,
                        "Female.5-9" = 2,
                        "Male.0-4" = 3,
                        "Male.5-9" = 4),
                      c("Female.0-4" = 11,
                        "Female.5-9" = 12,
                        "Male.0-4" = 13,
                        "Male.5-9" = 14))
  ssvd(data)
}
