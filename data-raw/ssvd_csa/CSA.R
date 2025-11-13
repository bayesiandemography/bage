
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.csa1 = "ssvd_csa/UNdata_Export_20251022_221132488.csv.zip",
           .csa2 = "ssvd_csa/UNdata_Export_20251022_221508651.csv.zip",
           .out = "../data/CSA.rda")

csa1 <- read_csv(.csa1, show_col_types = FALSE, n_max = 100000) |>
  filter(`Country or Area` != "Republic of Korea") ## in csa2

csa2 <- read_csv(.csa2, show_col_types = FALSE, n_max = 36286)

CSA <- bind_rows(csa1, csa2) |>
  data_ssvd_csa() |>
  mutate(version = "v2025") |>
  ssvd()

save(CSA, file = .out, compress = "bzip2")

