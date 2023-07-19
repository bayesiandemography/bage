
library(readr)
library(dplyr)
library(forcats)
library(poputils)
library(command)

cmd_assign(.expenditure_raw = "expenditure/EBDAG_24052016055802288.csv.gz",
           .popn_raw = "expenditure/POP_PROJ_04062016225548685.csv.gz",
           .out = "../data/expenditure.rda")
                 
expenditure_total <- read_csv(.expenditure_raw, col_types = "---------c-c---c-id--") %>%
    filter(Unit == "Million of national currency units") %>%
    select(diag = "Diagnostic Category",
           age = "Age Group",
           year = Year,
           total = Value) %>%
    mutate(diag = fct_inorder(diag)) %>%
    mutate(age = reformat_age(age),
           age = combine_age(age, to = "five"),
           age = set_age_open(age, lower = 85)) %>%
    count(diag, age, year, wt = total, name = "total")
    

popn <- read_csv(.popn_raw, col_types = "-----c---i------d--") %>%
    select(age = Age,
           year = Time,
           popn = Value) %>%
    mutate(age = sub("Population (hist&proj)  ", "", age, fixed = TRUE),
           age = reformat_age(age)) %>%
    count(age, year, wt = popn, name = "popn")

expenditure <- expenditure_total %>%
    inner_join(popn, by = c("age", "year")) %>%
    mutate(value = 1000 * total / popn) %>%
    select(diag, age, year, value)
                        
save(expenditure, file = .out, compress = "bzip2")


