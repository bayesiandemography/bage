
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(dplyr)
  library(command)
})

cmd_assign(.himd = "ssvd_himd/himd_20241023.zip",
           .out = "../data/HIMD_R.rda")

data <- data_ssvd_himd(.himd,
                       measure = "rate",
                       time_interval = 1,
                       n_comp = 5) |>
  mutate(version = "v2024") |>
  relocate()

HIMD_R <- ssvd(data)

save(HIMD_R, file = .out, compress = "bzip2")

