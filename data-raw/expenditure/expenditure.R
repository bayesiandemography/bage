
library(readr)
library(dplyr)
library(forcats)
library(poputils)

expenditure_total <- read_csv("expenditure/EBDAG_24052016055802288.csv.gz",
                        col_types = "---------c-c---c-id--") %>%
    filter(Unit == "Million of national currency units") %>%
    select(diag = "Diagnostic Category",
           age = "Age Group",
           year = Year,
           total = Value) %>%
    mutate(diag = fct_inorder(diag)) %>%
    mutate(age = clean_age(age, factor = FALSE),
           age = case_when(age %in% c("0", "1-4") ~ "0-4",
                           age %in% c("85-89", "90-94", "95+") ~ "85+",
                           TRUE ~ age),
           age = clean_age(age)) %>%
    count(diag, age, year, wt = total, name = "total")
    

popn <- read_csv("expenditure/POP_PROJ_04062016225548685.csv.gz",
                 col_types = "-----c---i------d--") %>%
    select(age = Age,
           year = Time,
           popn = Value) %>%
    mutate(age = sub("Population (hist&proj)  ", "", age, fixed = TRUE),
           age = clean_age(age)) %>%
    count(age, year, wt = popn, name = "popn")

expenditure <- expenditure_total %>%
    inner_join(popn, by = c("age", "year")) %>%
    mutate(value = 1000 * total / popn) %>%
    select(diag, age, year, value)

                        
save(expenditure,
     file = "../data/expenditure.rda",
     compress = "bzip2")


