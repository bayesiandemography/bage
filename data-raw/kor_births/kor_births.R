
library(readr)
library(command)
library(dplyr, warn.conflicts = FALSE)
library(poputils)
library(tidyr)
library(forcats)


cmd_assign(.births = "kor_births/101_DT_1B81A12_20240924103050.csv",
           .popn = "kor_births/Resident_Population_in_FiveYear_Age_Groups__2011_._20240924104155.csv",
           .out = "../data/kor_births.rda")

col_types <- paste0("cccc-",
                    paste(rep("i", times = 33), collapse = ""),
                    "-")
col_names <- c("region", "age", "birth_order", "sex", "x1", 1991:2023, "x2")
births <- read_csv(.births,
                   col_types = col_types,
                   col_names = col_names,
                   skip = 1) |>
filter(region != "Total") |>
  filter(sex != "Total[Person]") |>
  mutate(sex = sub("\\[Person\\]", "", sex)) |>
  filter(!(age %in% c("Total", "Age unknown"))) |>
  mutate(age = if_else(age == "Less than 15 years old", "10-14", age),
         age = sub("old", "", age),
         age = reformat_age(age),
         age = if_else(age == "50+", "50-54", age)) |>
  pivot_longer(`1991`:`2023`, names_to = "time", values_to = "births") |>
  filter(birth_order == "Total") |>
  count(age, region, time, wt = births, name = "births")


labels_age <- c("Total", poputils::age_labels(type = "five"))
n_age <- length(labels_age)
col_types <- paste0("cc",
                    paste(rep("i", times = n_age), collapse = ""),
                    paste(rep("-", times = 2 * n_age), collapse = ""),
                    paste(rep("i", times = n_age * 13)))
col_names <- c("region", "sex",
               paste("2024", labels_age, sep = "."),
               paste("2024.1", labels_age, sep = "."),
               paste("2024.2", labels_age, sep = "."),
               paste(rep(2011:2023, each = n_age), labels_age, sep = "."))
popn <- read_csv(.popn,
                 col_types = col_types,
                 col_names = col_names,
                 skip = 2,
                 na = "-") |>
  pivot_longer(-c(region, sex), names_to = "time.age", values_to = "popn") |>
  mutate(sex = sub(" \\(Person\\)", "", sex)) |>
  filter(sex != "Population") |>
  filter(region != "Whole country") |>
  separate_wider_delim(time.age, delim = ".", names = c("time", "age")) |>
  filter(age != "Total") |>
  mutate(age = reformat_age(age)) |>
  filter(sex == "Female") |>
  select(-sex)

kor_births <- inner_join(births, popn, by = c("age", "region", "time"))
levels_region <- intersect(unique(popn$region), kor_births$region)
kor_births <- kor_births |>
  mutate(region = factor(region, levels = levels_region)) |>
  mutate(age = as.character(age),
         time = as.integer(time),
         popn = as.integer(popn))

save(kor_births, file = .out, compress = "bzip2")

## library(ggplot2)
## ggplot(kor_births, aes(x = time, y = births / popn, color = age)) +
##   facet_wrap(vars(region)) +
##   geom_line()


## kor_births |>
## mutate(rate = births / popn) |>
## group_by(region, time) |>
## summarise(tfr = 5 * sum(rate)) |>
## ggplot(aes(x = time, y = tfr)) +
##   facet_wrap(vars(region)) +
##   geom_line()


