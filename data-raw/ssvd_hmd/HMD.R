
suppressPackageStartupMessages({
  library(dplyr)
  library(bssvd)
  library(bage)
  library(command)
})

cmd_assign(.hmd25 = "ssvd_hmd/hmd_statistics_20250925.zip",
           .hmd24 = "ssvd_hmd/hmd_statistics_20240226.zip",
           .out = "../data/HMD.rda")

data_2025 <- data_ssvd_hmd(zipfile = .hmd25,
                           n_comp = 5,
                           date = "2025-09-25")
data_2025_50 <- data_ssvd_hmd(zipfile = .hmd25,
                              n_comp = 5,
                              date = "2025-09-25",
                              year_min = 1950)
data_2024 <- data_ssvd_hmd(zipfile = .hmd24,
                           n_comp = 5,
                           date = "2024-02-26")
data <- bind_rows("v2025" = data_2025,
                  "v2025_50" = data_2025_50,
                  "v2024" = data_2024,
                  .id = "version")

HMD <- bage:::ssvd(data)

save(HMD, file = .out, compress = "bzip2")

