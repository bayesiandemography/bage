
library(dplyr, warn.conflicts = FALSE)
library(command)

cmd_assign(.out = "../data/us_acc_deaths.rda")

us_acc_deaths <- USAccDeaths %>%
    as.data.frame() %>%
    mutate(month = seq.Date(from = as.Date("1973-01-01"),
                            to = as.Date("1978-12-01"),
                            by = "month"),
           month = format(month, format = "%Y-%b")) %>%
    select(month, deaths = x)

save(us_acc_deaths, file = .out, compress = "bzip2")
           
