
suppressPackageStartupMessages({
  library(bssvd)
  library(bage)
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.asfr24 = "ssvd_hfd/asfrRR_20240523.txt.zip",
           .asfr25 = "ssvd_hfd/asfrRR_20250724.txt.zip",
           .out = "../data/HFD.rda")

asfr24 <- read_table(.asfr24, skip = 2) 
asfr25 <- read_table(.asfr25, skip = 2)

data_24 <- data_ssvd_hfd(asfr24, n_comp = 5) |>
  mutate(version = "v2024")

data_25 <- data_ssvd_hfd(asfr25, n_comp = 5) |>
  mutate(version = "v2025")

data <- bind_rows(data_24, data_25)

HFD <- ssvd(data)

save(HFD, file = .out, compress = "bzip2")


