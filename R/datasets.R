
## #' Labour force participation rates from the OECD
## #'
## #' Estimated labour force participation rates,
## #' is the proportion of the population that is in the
## #' labour force, by age and sex.
## #'
## #' @format A matrix, in which each row holds a combination
## #' of sex and age, and each column holds a combination
## #' of country and year.
## #'
## #' @source OECD (2022), "Labour Market Statistics:
## #' Labour force statistics by sex and age: indicators",
## #' OECD Employment and Labour Market Statistics (database),
## #' https://doi.org/10.1787/data-00310-en
## #' (accessed on 12 October 2022).
## "lfpr_oecd"


#' Deaths in Iceland
#'
#' Deaths and mid-year populations in Iceland,
#' by age, sex, and calendar year.
#'
#' @format
#' A tibble with 5300 rows and the following
#' columns:
#' `age` Single year of age, up to `"105+"`.
#' `sex` `"Female"` and `"Male"`.
#' `time` Calendar year, 1998-2022.
#' `deaths` Counts of deaths.
#' `popn` Mid-year population.
#'
#' @source Tables "Deaths by municipalities, sex and age 1981-2022",
#' and "Average annual population by municipality,
#' age and sex 1998-2022 - Current municipalities",
#' on the Statistics Iceland website. Data downloaded
#' on 12 July 2023.
"deaths"


#' Components from Human Mortality Database
#'
#' An object of class [bage_scaled_svd][scaled_svd()]
#' holding components extracted from mortality
#' data from the [Human Mortality Database](https://www.mortality.org).
#'
#' @source Derived from data at
#' Human Mortality Database. Max Planck Institute for
#' Demographic Research (Germany), University of California,
#' Berkeley (USA), and French Institute for Demographic Studies
#' (France). Available at www.mortality.org.
#' Code for processing the data is
#' [here](https://github.com/bayesiandemography/svd_hmd).
"HMD"

#' Fatal injuries in New Zealand
#'
#' Counts of fatal injuries in New Zealand, by age, sex,
#' ethnicity, and year, plus estimates of the population
#' at risk.
#'
#' @format A data frame with 912 rows and the
#' following variables:
#' - `age`: Age, in 5-year age groups, up to age 55-59.
#' - `sex`: `"Female"` or `"Male"`
#' - `ethnicity`: `"Maori"` or `"Non Maori"`
#' - `year`: Calendar year
#' - `injuries`: Count of injuries, randomly rounded to base 3.
#' - `popn`: Population on 30 June.
#'
#' @source Derived from data in tables "Estimated Resident
#' Population by Age and Sex (1991+) (Annual-Jun)" and
#' "Maori Ethnic Group Estimated Resident Population
#' by Age and Sex (1991+) (Annual-Jun)" in the online
#' database Infoshare, and table
#' "Count of fatal and serious non-fatal
#' injuries by sex, age group, ethnicity, cause, and severity
#' of injury, 2000-2021" in the online database NZ.Stat,
#' on the Statistics New Zealand website.
#' Data downloaded on 1 January 2023.
"injuries"



#' People in one-person households in New Zealand
#'
#' Counts of people in one-person households, and
#' counts of people living in any household, by
#' age, region, and year.
#'
#' @format A data frame with 528 rows and the
#' following variables:
#' - `age`: Age, in 5-year age groups, with open age
#'    group of 65+
#' - `region`: Region within New Zealand
#' - `year`: Calendar year
#' - `oneperson`: Count of people living in one-person
#' households
#' - `total`: Count of people living in all types of
#' household
#'
#' @source Derived from data in table
#' "Household composition by age group, for people in
#' households in occupied private dwellings,
#' 2006, 2013, and 2018 Censuses (RC, TA, DHB, SA2)"
#' in the online database NZ.Stat,
#' on the Statistics New Zealand website.
#' Data downloaded on 3 January 2023.
"households"


#' Per capital health expenditure in the Netherlands, 2003-2011
#' 
#' Per capita health expenditure, in Euros, for all providers,
#' by diagnostic group, age group, and year.
#'
#' @format A data frame with 1296 rows and the
#' following variables:
#' - `diag` Diagnostic group
#' - `age` Age, in 5-year age groups, with open age
#' group of 85+
#' - `year` 2003, 2005, 2007, and 2011
#'
#' @source Calculated from data in table
#' "Expenditure by disease, age and gender under the System
#' of Health Accounts (SHA) Framework :
#' Current health spending by age"
#' from OECD database 'OECD.Stat' (downloaded on 25 May 2016)
#' and in table "Historical population data and projections
#' (1950-2050)" from OECD database 'OECD.Stat' (downloaded
#' 5 June 2016).
"expenditure"
