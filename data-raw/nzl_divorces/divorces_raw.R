
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(poputils)
library(command)

cmd_assign(.raw = "divorces/VSM480501_20230322_102557_40.csv.gz",
           .out = "divorces/divorces_raw.rds")
           

levels_time <- 1992:2021

## recode age group 16-19 to 15-19
age_labels <- age_labels(type = "five", min = 15, max = 65, open = TRUE)
col_names <- c("time",
               paste("Male", age_labels, sep = "."),
               paste("Female", age_labels, sep = "."))
col_types <- paste(rep(c("c", "d"), times = c(1, 2 * length(age_labels))),
                       collapse = "")

divorces_raw <- read_csv(.raw,
                         skip = 3, ## skip values for 1991, which have NAs
                         n_max = 40,
                         na = "..",
                         col_types = col_types,
                         col_names = col_names) |>
  pivot_longer(cols = -time,
               names_to = c("sex", "age"),
               names_sep = "\\.") |>
  filter(time %in% levels_time) |>
  mutate(time = as.integer(time)) |>
  mutate(age = reformat_age(age)) |>
  count(age, sex, time, wt = value, name = "divorces")

saveRDS(divorces_raw, file = .out)

