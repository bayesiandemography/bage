
library(command)
library(bssvd)
library(bage)

cmd_assign(.himd = "ssvd_himd/himd_2024-10-24.zip",
           .out = "../data/HIMD_P1.rda")

data <- data_ssvd_himd(.himd,
                       measure = "prob",
                       time_interval = 1,
                       n_comp = 5)
HIMD_P1 <- bage:::ssvd(data)

save(HIMD_P1, file = .out, compress = "bzip2")

