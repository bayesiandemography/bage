
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
