
library(command)
library(bssvd)
library(bage)

cmd_assign(.asfr = "ssvd_hfd/asfrRR.txt.zip",
           .out = "../data/HFD.rda")

asfr <- readr::read_table(.asfr, skip = 2)

data <- data_ssvd_hfd(asfr, n_comp = 5)
HFD <- bage:::ssvd(data)

save(HFD, file = .out, compress = "bzip2")


