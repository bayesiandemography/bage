
## Data Frames ----------------------------------------------------------------

#' Deaths in Iceland
#'
#' Deaths and mid-year populations in Iceland,
#' by age, sex, and calendar year.
#'
#' @format
#' A [tibble][tibble::tibble-package] with 5,300 rows and the following
#' columns:
#' 
#' - `age` Single year of age, up to `"105+"`
#' - `sex` `"Female"` and `"Male"`
#' - `time` Calendar year, 1998-2022
#' - `deaths` Counts of deaths
#' - `popn` Mid-year population
#'
#' @source Tables "Deaths by municipalities, sex and age 1981-2022",
#' and "Average annual population by municipality,
#' age and sex 1998-2022 - Current municipalities",
#' on the Statistics Iceland website. Data downloaded
#' on 12 July 2023.
"deaths"


#' Divorces in New Zealand
#'
#' Counts of divorces and population, by age, sex,
#' and calendar year, in New Zealand, 2011-2021.
#'
#' @format
#' A [tibble][tibble::tibble-package] with 242 rows
#' and the following columns:
#' 
#' - `age`: 5-year age groups, `"15-19"` to `"65+"`
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `divorces`: Numbers of divorces during year
#' - `population`: Person-years lived during year
#'
#'
#' @source Divorce counts from data in table "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)"
#' in the online database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 22 March 2023.
#' Population estimates derived from data in table
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 26 March 2023.
"divorces"


#' Per Capita Health Expenditure in the Netherlands, 2003-2011
#' 
#' Per capita health expenditure, in Euros,
#' by diagnostic group, age group, and year, in the Netherlands.
#'
#' @format A [tibble][tibble::tibble-package] with 1,296 rows and the
#' following columns:
#' 
#' - `diag` Diagnostic group
#' - `age` 5-year age groups, with open age
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


#' People in One-Person households in New Zealand
#'
#' Counts of people in one-person households, and
#' counts of people living in any household, by
#' age, region, and year.
#'
#' @format
#' A [tibble][tibble::tibble-package] with 528 rows and the
#' following columns:
#' 
#' - `age`: 5-year age groups, with open age
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


#' Fatal Injuries in New Zealand
#'
#' Counts of fatal injuries in New Zealand, by age, sex,
#' ethnicity, and year, plus estimates of the population
#' at risk.
#'
#' @format
#' A [tibble][tibble::tibble-package]
#' with 912 rows and the following columns:
#' 
#' - `age`: 5-year age groups, up to age 55-59
#' - `sex`: `"Female"` or `"Male"`
#' - `ethnicity`: `"Maori"` or `"Non Maori"`
#' - `year`: Calendar year
#' - `injuries`: Count of injuries, randomly rounded to base 3
#' - `popn`: Population on 30 June
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


#' Accidental Deaths in the USA
#'
#' Counts of accidental deaths in the USA,
#' by month, for 1973-1978.
#'
#' @format
#' A [tibble][tibble::tibble-package]
#' with 72 rows and the following columns:
#' 
#' - `month`: Year and month.
#' - `deaths`: Count of deaths.
#'
#' @source Reformatted version of
#' `datasets::USAccDeaths`.
"us_acc_deaths"


## SVD ------------------------------------------------------------------------

#' Components from OECD Labor Force Participation Data
#'
#' An object of class `"bage_ssvd"`
#' holding components extracted from labor force participation
#' data from the
#' [OECD Data Explorer](https://data-explorer.oecd.org).
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' @source Derived from data in the "Labor Force Indicators"
#' table of the OECD Data Explorer.
#' Code to create `LFS`
#' is in folder 'data-raw/ssvd_lfp' in
#' the source code for the **bage**
#' package.
"LFP"


#' Components from Human Mortality Database
#'
#' An object of class `"bage_ssvd"`
#' holding components extracted from mortality
#' data from the
#' [Human Mortality Database](https://www.mortality.org).
#' The object holds 5 components.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' @source Derived from data at
#' Human Mortality Database. Max Planck Institute for
#' Demographic Research (Germany), University of California,
#' Berkeley (USA), and French Institute for Demographic Studies
#' (France). Available at www.mortality.org.
#' Code to create `HMD`
#' is in folder 'data-raw/ssvd_hmd'
#' in the source code for **bage**
#' package.
"HMD"
