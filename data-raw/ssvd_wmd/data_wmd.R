
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(dplyr)
  library(command)
})

cmd_assign(.wmd19 = "ssvd_wmd/undesa_pd_2019_wmd_marital_status.xlsx",
           .out = "../data/data_wmd.rda")

data_wmd <- data_ssvd_wmd(.wmd19, status = "current") |>
  mutate(version = "v2019") |>
  relocate(version) |>
  slice(c(1:3, 8:10))

save(data_wmd, file = .out, compress = "bzip2")

