
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(dplyr)
  library(command)
})

cmd_assign(.wmd19 = "undesa_pd_2019_wmd_marital_status.xlsx",
           .out = "../data/WMD_C.rda")

WMD_C <- data_ssvd_wmd(.wmd19, status = "current") |>
  mutate(version = "v2019") |>
  ssvd()

save(WMD_C, file = .out, compress = "bzip2")

