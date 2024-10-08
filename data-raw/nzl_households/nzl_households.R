
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(forcats)
library(poputils)
library(command)

cmd_assign(.raw = "nzl_households/TABLECODE8420_Data.csv.gz",
           .out = "../data/nzl_households.rda")

nzl_households <- read_csv(.raw, col_types = "cccii-") |>
  rename(region = Area,
         age = `Age group`,
         composition = `Household composition`,
         year = Year,
         count = Value) |>
  filter(!grepl("^Total", region)) |>
  mutate(region = sub(" Region$", "", region),
         region = fct_inorder(region)) |>
  mutate(age = reformat_age(age)) |>
  mutate(composition = fct_recode(composition,
                                  oneperson = "One-person household",
                                  total = "Total people in households stated")) |>
  pivot_wider(names_from = composition, values_from = count) |>
  select(age, region, year, oneperson, total)

save(nzl_households, file = .out, compress = "bzip2")
