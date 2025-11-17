
suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

cmd_assign(.divorces = "nzl_divorces/divorces_raw.rds",
           .popn = "nzl_divorces/popn_raw.rds",
           .out = "../data/nzl_divorces.rda")
           

divorces <- readRDS(.divorces)
popn <- readRDS(.popn)

nzl_divorces <- inner_join(divorces, popn,
                           by = c("age", "sex", "time")) |>
  filter(time >= 2011)

save(nzl_divorces, file = .out, compress = "bzip2")

