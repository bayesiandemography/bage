
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
"isl_deaths"


#' Births in South Korea
#'
#' Births and mid-year population by age of mother,
#' region, and calendar year,
#' 2011-2023, plus regional data on GDP per capita and
#' population density.
#'
#' @format
#' A [tibble][tibble::tibble-package] with 1,872 rows and the following
#' columns:
#'
#' - `age` Five-year age groups from `"10-14" to `"50-54"`
#' - `region` Administrative region
#' - `time` Calendar year, 2011-2023
#' - `births` Counts of births
#' - `popn` Mid-year population
#' - `gdp_pc_2023` Regional GDP per capita in 2023
#' - `dens_2020` Regional population density
#'   (people per km-squared) in 2020
#'
#' @source Tables "Live Births by Age Group of Mother,
#' Sex and Birth Order for Provinces",
#' and "Resident Population in Five-Year Age Groups",
#' on the Korean Statistical Information Service website.
#' Data downloaded on 24 September 2024. Data on GDP per capita
#' and population density from Wikipedia
#' https://w.wiki/DMFA, data downloaded on 8 March 2025, and
#' https://w.wiki/DMF9, data downloaded on 8 March 2025.
"kor_births"


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
"nzl_divorces"


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
"nld_expenditure"


#' People in One-Person Households in New Zealand
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
"nzl_households"


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
"nzl_injuries"


#' Deaths in Portugal
#'
#' Deaths and exposure in Portugal, by age, sex, and year.
#'
#' The data are from the Human Mortality Database. Deaths are
#' rounded to the nearest integer.  More recent versions,
#' and a comprehensive description of the data,
#' are available at the HMD website.
#'
#' @format
#' A [tibble][tibble::tibble-package]
#' with 3,168 rows and the following columns:
#'
#' - `age`: Age groups `"0"`, `"1-4"`, "5-9", ..., `"95-99"`, `"100+"`
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `deaths`: Count of deaths
#' - `exposure`: Person-years lived by population
#'
#' @source Human Mortality Database. University of California, Berkeley (USA),
#' and Max Planck Institute for Demographic Research (Germany).
#' Available at \url{https://www.mortality.org}.
#' (data downloaded on 17 July 2018).
"prt_deaths"


#' Infant Mortality in Sweden
#'
#' Counts of births and infant deaths in Sweden by county and year, 1995-2015
#'
#' Dataset used in Chapter 11 of the book
#' *Bayesian Demographic Estimation and Forecasting*.
#'
#' @format A tibble with 441 rows and the following columns:
#' - `county`: A factor with 21 levels, where the levels are
#'   ordered by number of births, from `"Stockholm"` down to `"Gotland"`
#' - `time: Calendar year
#' - `births`: Count of births
#' - `deaths`: Count of infant deaths
#'
#' @references Bryant J and Zhang J. 2018.
#' *Bayesian Demographic Estimation and Forecasting*. CRC Press.
#'
#' @source Database "Live births by region, mother's age and child's sex.
#' Year 1968 - 2017" and database "Deaths by region, age (during the year) and sex.
#' Year 1968 - 2017" on the Statistics Sweden website. Downloaded on 13 July 2018.
"swe_infant"


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
"usa_deaths"


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


#' Components from Human Fertility Database
#'
#' An object of class `"bage_ssvd"`
#' holding components extracted from mortality
#' data from the
#' [Human Fertility Database](https://www.humanfertility.org).
#' The object holds 5 components.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' @source Derived from data from the
#' Human Fertility Database.Max Planck Institute
#' for Demographic Research (Germany) and Vienna
#' Institute of Demography (Austria). .
#' Code to create `HFD`
#' is in folder 'data-raw/ssvd_hfd'
#' in the source code for **bage**
#' package.
"HFD"


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
#' @source Derived from data from the
#' Human Mortality Database. Max Planck Institute for
#' Demographic Research (Germany), University of California,
#' Berkeley (USA), and French Institute for Demographic Studies
#' (France). Code to create `HMD`
#' is in folder 'data-raw/ssvd_hmd'
#' in the source code for **bage**
#' package.
"HMD"
