
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(poputils)
  library(command)
})

cmd_assign(.deaths = "prt_deaths/Deaths_1x1.txt.gz",
           .exposure = "prt_deaths/PRT.Exposures_1x1.txt.gz",
           .out = "../data/prt_deaths.rda")

deaths <- read.table(.deaths,
                     skip = 2,
                     header = TRUE,
                     na.string = ".") |>
  select(time = Year, age = Age, Female, Male) |>
  pivot_longer(-c(time, age), names_to = "sex", values_to = "deaths") |>
  filter(time %in% 1950:2015) |>
  mutate(age = combine_age(age, to = "lt")) |>
  count(age, sex, time, wt = deaths, name = "deaths")

exposure <- read.table(.exposure,
                       skip = 2,
                       header = TRUE,
                       na.string = ".") |>
  select(time = Year, age = Age, Female, Male) |>
  pivot_longer(-c(time, age), names_to = "sex", values_to = "exposure") |>
  filter(time %in% 1950:2015) |>
  mutate(age = combine_age(age, to = "lt")) |>
  count(age, sex, time, wt = exposure, name = "exposure")

prt_deaths <- inner_join(deaths, exposure, by = c("age", "sex", "time")) |>
  mutate(age = factor(age, levels = unique(age)))


save(prt_deaths,
     file = "../data/prt_deaths.rda",
     compress = "bzip2")

