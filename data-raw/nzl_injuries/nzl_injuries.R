
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(poputils)
  library(command)
})

cmd_assign(.injuries_raw = "nzl_injuries/TABLECODE7935_Data.csv.gz",
           .popn_maori_raw = "nzl_injuries/DPE479901_20230101_021445_73.csv.gz",
           .popn_total_raw = "nzl_injuries/DPE403903_20230101_021709_80.csv.gz",
           .out = "../data/nzl_injuries.rda")

injuries_all <- read_csv(.injuries_raw, col_types = "i") |>
  select(age = "Age group",
         sex = Sex,
         ethnicity = Ethnicity,
         year = Year, count = Value) |>
  filter(age != "Total all ages") |>
  mutate(age = if_else(age %in% c("60 - 89 years", "90 years and over"),
                       "60+",
                       age)) |>
  mutate(age = reformat_age(age)) |>
  count(age, sex, ethnicity, year, wt = count, name = "injuries") |>
  complete(age, sex, ethnicity, year, fill = list(injuries = 0L))

ages <- age_labels(type = "five", max = 60)
col_names_popn <- c("year",
                    paste(rep(c("Male", "Female"), each = length(ages)),
                          ages))

popn_maori <- read_csv(.popn_maori_raw,
                       skip = 3,
                       col_names = col_names_popn,
                       col_types = "i",
                       n_max = 32) |>
  pivot_longer(-year,
               names_to = c("sex", "age"),
               names_sep = " ",
               values_to = "Maori") |>
  mutate(age = reformat_age(age))

popn_total <- read_csv(.popn_total_raw,
                       skip = 4,
                       col_names = col_names_popn,
                       col_types = "i",
                       n_max = 32) |>
  pivot_longer(-year,
               names_to = c("sex", "age"),
               names_sep = " ",
               values_to = "Total") |>
  mutate(age = reformat_age(age))

popn <- inner_join(popn_maori, popn_total, by = c("age", "sex", "year")) |>
  mutate(`Non Maori` = Total - Maori,
         Total = NULL) |>
  pivot_longer(c(Maori, `Non Maori`),
               names_to = "ethnicity",
               values_to = "popn")

nzl_injuries <- inner_join(injuries_all, popn, by = c("age", "sex", "year", "ethnicity")) |>
  arrange(year, ethnicity, sex, age) |>
  mutate(injuries = as.integer(injuries),
         popn = as.integer(popn)) |>
  filter(age_upper(age) <= 60) |>
  droplevels()

save(nzl_injuries, file = .out, compress = "bzip2")
