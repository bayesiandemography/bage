
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(poputils)
library(command)

cmd_assign(.raw = "divorces/DPE403905_20230322_103055_56.csv.gz",
           .out = "divorces/popn_raw.rds")


age_labels <- age_labels(type = "single", max = 95)
col_names <- c("time",
               paste("Male", age_labels, sep = "."),
               paste("Female", age_labels, sep = "."))
col_types <- paste(rep(c("c", "d", "-", "d", "-", "d", "-", "d", "-"),
                       times = c(1, 95, 38, 1, 2, 95, 38, 1, 2)),
                       collapse = "")
levels_time <- 1992:2021
age_max <- 65

popn_raw <- read_csv(.raw,
                     skip = 5, ## skip values for 1991, which have NAs
                     n_max = 31,
                     na = "..",
                     col_types = col_types,
                     col_names = col_names) |>
  pivot_longer(cols = -time,
               names_to = c("sex", "age"),
               names_sep = "\\.") |>
  filter(time %in% levels_time) |>
  mutate(age = reformat_age(age),
         age = combine_age(age, to = "five"),
         age = set_age_open(age, lower = 65)) |>
  filter(age_lower(age) >= 15) |>
  droplevels() |>
  mutate(time = as.integer(time)) |>
  count(age, sex, time, wt = value, name = "population")

saveRDS(popn_raw, file = .out)

