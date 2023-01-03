
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(poputils)

households <- read_csv("households/TABLECODE8420_Data.csv.gz",
                       col_types = "cccii-") %>%
    rename(region = Area,
           age = `Age group`,
           composition = `Household composition`,
           year = Year,
           count = Value) %>%
    filter(!grepl("^Total", region)) %>%
    mutate(region = sub(" Region$", "", region),
           region = fct_inorder(region)) %>%
    mutate(age = clean_age(age)) %>%
    mutate(composition = fct_recode(composition,
                                    oneperson = "One-person household",
                                    total = "Total people in households stated")) %>%
    pivot_wider(names_from = composition, values_from = count) %>%
    select(age, region, year, oneperson, total)

save(households,
     file = "../data/households.rda",
     compress = "bzip2")
                                    
                                    

    
