
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(stringi)
library(forcats)
library(command)

cmd_assign(.births = "swe_infant/BE0101E2.csv",
           .deaths = "swe_infant/BE0101D9.csv",
           .out = "../data/swe_infant.rda")

births <- read_csv(.births,
                   skip = 2,
                   locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) |>
  rename(county = region) |>
  pivot_longer(-county, names_to = "time", values_to = "births") |>
  mutate(county = sub("[0-9]+ (.*) county", "\\1", county),
         county = stri_trans_general(county, "latin-ascii"))

deaths <- read_csv(.deaths,
                   skip = 2,
                   locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) |>
  select(-age) |>
  rename(county = region) |>
  pivot_longer(-county, names_to = "time", values_to = "deaths") |>
  mutate(county = sub("[0-9]+ (.*) county", "\\1", county),
         county = stri_trans_general(county, "latin-ascii"))

swe_infant <- inner_join(births, deaths, by = c("county", "time")) |>
  mutate(county = fct_reorder(county, .x = -births)) |>
  mutate(time = as.integer(time),
         births = as.integer(births),
         deaths = as.integer(deaths))

save(swe_infant,
     file = "../data/swe_infant.rda",
     compress = "bzip2")

