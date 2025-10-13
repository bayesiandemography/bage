
library(command)
library(bssvd)
library(bage)

cmd_assign(.himd = "ssvd_himd/himd_2025-10-06.zip",
           .out = "../data/HIMD_R.rda")

data <- data_ssvd_himd(.hmd,
                       measure = "rate",
                       time_interval = 1,
                       n_comp = 5)
HIMD_R <- bage:::ssvd(data)

save(HIMD_R, file = .out, compress = "bzip2")

