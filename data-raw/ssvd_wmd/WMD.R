
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.wmd19 = "undesa_pd_2019_wmd_marital_status.xlsx",
           .out = "../data/WMD.rda")

WMD <- data_ssvd_wmd(.wmd19) |>
  mutate(version = "v2019") |>
  ssvd()

save(WMD, file = .out, compress = "bzip2")

