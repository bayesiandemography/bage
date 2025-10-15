
library(command)
library(bssvd)
library(bage)

cmd_assign(.himd = "ssvd_himd/himd_2024-10-24.zip",
           .out = "../data/HIMD_P5.rda")

data <- data_ssvd_himd(.himd,
                       measure = "prob",
                       time_interval = 5,
                       n_comp = 5)
HIMD_P5 <- bage:::ssvd(data)

save(HIMD_P5, file = .out, compress = "bzip2")

