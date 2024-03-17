
library(command)
library(bage)

cmd_assign(.hmd = "svd_hmd/hmd_statistics_20240226.zip",
           .out = "../data/HMD.rda")

HMD <- ssvd_hmd(.hmd)

save(HMD, file = .out, compress = "bzip2")

