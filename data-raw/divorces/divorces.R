
library(dplyr, warn.conflicts = FALSE)
library(command)

cmd_assign(divorces_raw = "divorces/divorces_raw.rds",
           popn_raw = "divorces/popn_raw.rds",
           .out = "../data/divorces.rda")
           

divorces <- inner_join(divorces_raw,
                       popn_raw,
                       by = c("age", "sex", "time")) |>
  filter(time >= 2011)

save(divorces, file = .out, compress = "bzip2")

