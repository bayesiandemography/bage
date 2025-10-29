
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(dplyr)
  library(command)
})

cmd_assign(.himd = "ssvd_himd/himd_20241023.zip",
           .out = "../data/HIMD_P1.rda")

data <- data_ssvd_himd(.himd,
                       measure = "prob",
                       time_interval = 1,
                       n_comp = 5) |>
  mutate(version = "v2024")
         
HIMD_P1 <- ssvd(data)

save(HIMD_P1, file = .out, compress = "bzip2")

