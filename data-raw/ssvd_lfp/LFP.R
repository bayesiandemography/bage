
library(command)
library(bssvd)
library(rsdmx)
library(bage)

cmd_assign(.out = "../data/LFP.rda")

url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")
lfp_sdmx <- rsdmx::readSDMX(url)
lfp_df <- as.data.frame(lfp_sdmx)
data <- data_ssvd_lfp(lfp_df)
LFP <- ssvd(data)

save(LFP, file = .out, compress = "bzip2")

