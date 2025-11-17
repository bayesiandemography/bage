
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(poputils)
  library(tidyr)
  library(forcats)
  library(command)
})


cmd_assign(.births = "kor_births/101_DT_1B81A12_20240924103050.csv.gz",
           .popn = "kor_births/Resident_Population_in_FiveYear_Age_Groups__2011_._20240924104155.csv.gz",
           .gdp = "kor_births/List_of_South_Korean_regions_by_GDP_2.csv.gz",
           .dens = "kor_births/Administrative_divisions_of_South_Korea_2.csv.gz",
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


gdp <- read_csv(.gdp, skip = 1, col_types = "-cc", col_names = c("-", "region", "gdp_pc_2023")) |>
  filter(!(region %in% c("South Korea", "Sejong"))) |>
  mutate(region = case_when(region == "South Chungcheong Province" ~ "Chungcheongnam-do",
                            region == "South Jeolla Province" ~ "Jeollanam-do",
                            region == "North Chungcheong Province" ~ "Chungcheongbuk-do",
                            region == "North Gyeongsang Province" ~ "Gyeongsangbuk-do",
                            region == "Gyeonggi Province" ~ "Gyeonggi-do",
                            region == "South Gyeongsang Province" ~ "Gyeongsangnam-do",
                            region == "Gangwon Province, South Korea" ~ "Gangwon-do",
                            region == "North Jeolla Province" ~ "Jeollabuk-do",
                            region == "Jeju Province" ~ "Jeju",
                            TRUE ~ region)) |>
  mutate(gdp_pc_2023 = sub("US\\$ ", "", gdp_pc_2023),
         gdp_pc_2023 = sub(",", ".", gdp_pc_2023),
         gdp_pc_2023 = as.numeric(gdp_pc_2023))

dens <- read_csv(.dens,
                 skip = 1,
                 col_types = "--c------c",
                 col_names = c("x1", "x2",
                               "region",
                               "x3", "x4", "x5", "x6", "x7", "x8",
                               "dens_2020")) |>
  filter(region != "Sejong special self-governing city") |>
  mutate(region = sub(" special city", "", region),
         region = sub(" metropolitan city", "", region),
         region = sub(" special self-governing province", "", region),
         region = case_when(region == "Gangwon" ~ "Gangwon-do",
                            region == "Jeonbuk" ~ "Jeollabuk-do",
                            TRUE ~ region)) |>
  mutate(dens_2020 = sub(",", "", dens_2020),
         dens_2020 = as.numeric(dens_2020),
         dens_2020 = case_when(dens_2020 < 1000 ~ "Low",
                          dens_2020 >= 1000 & dens_2020 < 4000 ~ "Medium",
                          dens_2020 >= 4000 ~ "High"))


kor_births <- inner_join(births, popn, by = c("age", "region", "time")) |>
  inner_join(gdp, by = "region") |>
  inner_join(dens, by = "region")
  
levels_region <- intersect(unique(popn$region), kor_births$region)

kor_births <- kor_births |>
  mutate(region = factor(region, levels = levels_region)) |>
  mutate(age = as.character(age),
         time = as.integer(time),
         popn = as.integer(popn))



save(kor_births, file = .out, compress = "bzip2")


