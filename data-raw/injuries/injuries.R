
library(readr)
library(dplyr)
library(tidyr)
library(poputils)

injuries_all <- read_csv("injuries/TABLECODE7935_Data.csv.gz",
                         col_types = "i") %>%
    select(age = "Age group",
           sex = Sex,
           ethnicity = Ethnicity,
           year = Year, count = Value) %>%
    filter(age != "Total all ages") %>%
    mutate(age = if_else(age %in% c("60 - 89 years", "90 years and over"),
                         "60+",
                         age)) %>%
    mutate(age = clean_age(age)) %>%
    count(age, sex, ethnicity, year, wt = count, name = "injuries") %>%
    complete(age, sex, ethnicity, year, fill = list(injuries = 0L))
    
ages <- age_labels(type = "five", max = 60)
col_names_popn <- c("year",
                    paste(rep(c("Male", "Female"), each = length(ages)),
                        ages))

popn_maori <- read_csv("injuries/DPE479901_20230101_021445_73.csv.gz",
                       skip = 3,
                       col_names = col_names_popn,
                       col_types = "i",
                       n_max = 32) %>%
    pivot_longer(-year,
                 names_to = c("sex", "age"),
                 names_sep = " ",
                 values_to = "Maori") %>%
    mutate(age = clean_age(age))


popn_total <- read_csv("injuries/DPE403903_20230101_021709_80.csv.gz",
                       skip = 4,
                       col_names = col_names_popn,
                       col_types = "i",
                       n_max = 32) %>%
    pivot_longer(-year,
                 names_to = c("sex", "age"),
                 names_sep = " ",
                 values_to = "Total") %>%
    mutate(age = clean_age(age))

popn <- inner_join(popn_maori, popn_total, by = c("age", "sex", "year")) %>%
    mutate(`Non Maori` = Total - Maori,
           Total = NULL) %>%
    pivot_longer(c(Maori, `Non Maori`),
                 names_to = "ethnicity",
                 values_to = "popn")

injuries <- inner_join(injuries_all, popn, by = c("age", "sex", "year", "ethnicity")) %>%
    arrange(year, ethnicity, sex, age) %>%
    mutate(injuries = as.integer(injuries),
           popn = as.integer(popn))

save(injuries,
     file = "../data/injuries.rda",
     compress = "bzip2")
