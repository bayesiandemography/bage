
library(command)
library(bage)
library(bssvd)
library(rsdmx)

cmd_assign(.out = "../data/LFS.rda")

url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")
lfp_sdmx <- rsdmx::readSDMX(url)
lfp_df <- as.data.frame(lfp_sdmx)
LFP <- ssvd_lfp(lfp_df)

save(LFP, file = .out, compress = "bzip2")

