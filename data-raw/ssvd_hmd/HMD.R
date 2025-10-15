
library(command)
library(bssvd)
library(bage)

cmd_assign(.hmd24 = "ssvd_hmd/hmd_statistics_20240226.zip",
           .hmd25 = "ssvd_hmd/hmd_statistics_20250925.zip",
           .out = "../data/HMD.rda")

data <- data_ssvd_hmd(.hmd25, n_comp = 5)
HMD <- bage:::ssvd(data)

save(HMD, file = .out, compress = "bzip2")

