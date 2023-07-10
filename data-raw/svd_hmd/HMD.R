
library(command)
library(bage)

cmd_assign(hmd = "svd_hmd/hmd_20221129.rds",
           .out = "../data/HMD.rda")

HMD <- scaled_svd(hmd)

