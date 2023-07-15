
library(command)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(poputils)

cmd_assign(popn_raw = "deaths/MAN02008_20230712-132130.csv.zip",
           deaths_raw = "deaths/MAN05221_20230712-131721.csv.zip",
           .out = "../data/deaths.rda")

col_types <- paste(c("cc", rep("d", times = 25)), collapse = "")
popn <- popn_raw %>%
    read_delim(skip = 1,
               delim = ";",
               col_types = col_types) %>%
    pivot_longer(-c(Age, Sex), names_to = "time", values_to = "popn") %>%
    separate(time, into = c("delete", "time"), convert = TRUE) %>%
    mutate(age = if_else(Age == "In 1st year", "0", Age),
           age = reformat_age(age)) %>%
    mutate(sex = reformat_sex(Sex)) %>%
    select(age, sex, time, popn)

deaths <- deaths_raw %>%
    read_delim(skip = 1,
               delim = ";",
               col_types = "i-cii") %>%
    pivot_longer(c(Males, Females), names_to = "sex", values_to = "deaths") %>%
    mutate(sex = reformat_sex(sex)) %>%
    mutate(age = if_else(Age == "On 1st year", "0", Age),
           age = reformat_age(age)) %>%
    select(age, sex, time = Year, deaths) %>%
    inner_join(popn, by = c("age", "sex", "time")) %>%
    mutate(age = set_age_open(age, lower = 105)) %>%
    group_by(age, sex, time) %>%
    summarise(deaths = sum(deaths), popn = sum(popn), .groups = "drop") %>%
    arrange(time, sex, age)

save(deaths, file = .out, compress = "bzip2")


## library(ggplot2)
## ggplot(popn, aes(x = age_mid(age), y = popn, col = sex)) +
##     facet_wrap(vars(time)) +
##     geom_line()
## ggplot(deaths, aes(x = age_mid(age), y = deaths, col = sex)) +
##     facet_wrap(vars(time)) +
##     geom_line()













