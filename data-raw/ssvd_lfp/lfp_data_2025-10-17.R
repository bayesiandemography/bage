
library(rsdmx)
library(zip)
library(readr)

url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")

base <- "ssvd_lfp/lfp_data_2025-10-17"
file <- paste0(base, ".csv")
zipfile <- paste0(base, ".zip")

data_sdmx <- rsdmx::readSDMX(url)
data_df <- as.data.frame(data_sdmx)

write_csv(data_df, file = paste0(base, ".csv"))
zip(zipfile = zipfile, files = file)
unlink(file)

save(data_df, file = .out, compress = "bzip2")

