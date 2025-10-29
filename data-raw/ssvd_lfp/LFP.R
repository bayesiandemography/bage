
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.lfp25 = "ssvd_lfp/lfp_data_2025-10-17.zip",
           .out = "../data/LFP.rda")

data <- read_csv(.lfp25)

LFP <- data |>
  data_ssvd_lfp() |>
  mutate(version = "v2025")
  ssvd()

save(LFP, file = .out, compress = "bzip2")

