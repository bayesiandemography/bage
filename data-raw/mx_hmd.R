
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

## Female

path <- "fltper_5x1"
fnames <- dir(path)
codes <- sub("([A-Z]+)\\..*", "\\1", fnames)

mx_f <- fnames %>%
    map(~file.path(path, .x)) %>%
    map(read_table,
        col_types = "ic----ii--",
        na = ".",
        skip = 1) %>%
    set_names(codes) %>%
    bind_rows(.id = "country") %>%
    mutate(sex = "Female")


## Male

path <- "mltper_5x1"
fnames <- dir(path)
codes <- sub("([A-Z]+)\\..*", "\\1", fnames)

mx_m <- fnames %>%
    map(~file.path(path, .x)) %>%
    map(read_table,
        col_types = "ic----ii--",
        na = ".",
        skip = 1) %>%
    set_names(codes) %>%
    bind_rows(.id = "country") %>%
    mutate(sex = "Male")


## Combined

age_last <- c("85-89", "90-94", "95-99", "100-104", "105-109", "110+")
levels_age <- c("0", "1-4", paste(seq(5, 80, 5), seq(9, 84, 5), sep = "-"), "85+")
mx_hmd <- bind_rows(mx_f, mx_m) %>%
    filter(!(country == "BEL" & Year %in% 1914:1918)) %>% ## all values missing
    mutate(Age = if_else(Age %in% age_last, "85+", Age)) %>%
    group_by(country, Year, Age, sex) %>%
    summarise(dx = sum(dx), Lx = sum(Lx)) %>%
    ungroup() %>%
    mutate(mx = dx / Lx) %>%
    mutate(age = factor(Age, levels = levels_age)) %>%
    select(country, year = Year, sex, age, mx) %>%
    arrange(country, year, sex, age) %>%
    unite(col = "country_year", country, year) %>%
    unite(col = "sex_age", sex, age) %>%
    mutate(sex_age = factor(sex_age, levels = unique(sex_age))) %>%
    xtabs(mx ~ sex_age + country_year, data = .)


## Save

save(mx_hmd,
     file = "../data/mx_hmd.rda",
     compress = "bzip2")
