
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(dplyr)
  library(command)
})

cmd_assign(.himd = "ssvd_himd/himd_20241023.zip",
           .out = "../data/HIMD_P5.rda")

data <- data_ssvd_himd(.himd,
                       measure = "prob",
                       time_interval = 5,
                       n_comp = 5) |>
  mutate(version = "v2024")

HIMD_P5 <- ssvd(data)

save(HIMD_P5, file = .out, compress = "bzip2")

