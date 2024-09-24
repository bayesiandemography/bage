
library(dplyr, warn.conflicts = FALSE)
library(command)

cmd_assign(divorces_raw = "nzl_divorces/divorces_raw.rds",
           popn_raw = "nzl_divorces/popn_raw.rds",
           .out = "../data/nzl_divorces.rda")
           

nzl_divorces <- inner_join(divorces_raw,
                       popn_raw,
                       by = c("age", "sex", "time")) |>
  filter(time >= 2011)

save(nzl_divorces, file = .out, compress = "bzip2")

