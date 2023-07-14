
sim_scaled_svd <- function() {
    data <- data.frame(type = c("total", "joint", "indep"))
    data$labels_age <- list(c("0-4", "5-9"),
                            c("0-4", "5-9", "0-4", "5-9"),
                            c("0-4", "5-9", "0-4", "5-9"))
    data$labels_sexgender <- list(NULL,
                                  c("Female", "Female", "Male", "Male"),
                                  c("Female", "Female", "Male", "Male"))
    data$matrix <- list(matrix(1, nr = 2, nc = 10,
                               dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(2, nr = 4, nc = 10,
                               dimnames = list(c("Female.0-4", "Female.5-9", "Male.0-4", "Male.5-9"), NULL)),
                        matrix(3, nr = 4, nc = 10,
                               dimnames = list(c("0-4", "5-9", "0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 1, "5-9" = 2),
                        c("Female.0-4" = 1, "Female.5-9" = 2, "Male.0-4" = 3, "Male.5-9" = 4),
                        c("0-4" = 11, "5-9" = 12, "0-4" = 13, "5-9" = 14))
    scaled_svd(data)
}
