
library(readr)
library(dplyr)
library(tidyr)

lfpr_oecd <- read_csv("LFS_SEXAGE_I_R_12102022025744175.csv",
                      col_types = "-c-c-c-----d------d--") %>%
    mutate(sex = factor(Sex, levels = c("Women", "Men"), labels = c("Female", "Male"))) %>%
    mutate(age = sub(" to ", "-", Age),
           age = factor(age, levels = unique(age))) %>%
    arrange(Country, Time, sex, age) %>%
    unite(col = sex_age, sex, age) %>%
    mutate(sex_age = factor(sex_age, levels = unique(sex_age))) %>%
    unite(col = country_time, Country, Time) %>%
    mutate(country_time = factor(country_time, levels = unique(country_time))) %>%
    mutate(value = Value / 100,
           value = if_else(value > 1, 1, value)) %>%
    xtabs(value ~ sex_age + country_time, data = .) %>%
    as.matrix()


save(lfpr_oecd,
     file = "../data/lfpr_oecd.rda",
     compress = "bzip2")
    

