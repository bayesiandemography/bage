
library(command)
library(bssvd)
library(bage)

cmd_assign(.hmd = "ssvd_hmd/hmd_statistics_20240226.zip",
           .out = "../data/HMD.rda")

data <- data_ssvd_hmd(.hmd, n_comp = 5)
HMD <- ssvd(data)

save(HMD, file = .out, compress = "bzip2")

