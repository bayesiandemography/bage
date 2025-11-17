
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' - `value` Expenditures, in Euros
#'
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
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
#' @seealso
#' - [datasets] Overview of datasets in \pkg{bage}
#' 
#' @source Reformatted version of
#' `datasets::USAccDeaths`.
"usa_deaths"


## SVD ------------------------------------------------------------------------

#' Scaled SVD Components from Census School Attendance Data
#'
#' An object of class `"bage_ssvd"`
#' holding scaled SVD components derived from
#' census data on school attendance.
#' The attendance data is assembed by
#' the United Nations Statistics Division.
#' `CSA` holds 5 components.
#'
#' @section Warning:
#'
#' Compared other age-sex patterns for
#' other demographic processes such as
#' mortality, age-sex patterns for school attendance
#' show substantial variation across populations. More
#' components may be needed to obtain satisfactory
#' models of age-sex patterns
#' for school attendance than for other processes.
#' 
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2025"` (default). Data downloaded on 2025-11-05
#'
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Derived from data in the
#' "Population 5 to 24 years of age by school
#' attendance, sex and urban/rural residence" table
#' from the
#' [Population Censuses' Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets) 
#' database assembled by the United Nations
#' Statistics Division.
#' Code to create `CSA`
#' is in folder \file{data-raw/ssvd_csa}
#' in the source code for the \pkg{bage}
#' package.
"CSA"


#' Scaled SVD Components from Human Fertility Database
#'
#' An object of class `"bage_ssvd"`
#' holding scaled SVD components derived from
#' data from the Human Fertility Database.
#' `HFD` holds 5 components.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2025"` (default) Data published on 2025-07-24
#' - `"v2024"` Data published on October 2024-10-23
#'
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Derived from data from the
#' [Human Fertility Database](https://www.humanfertility.org).
#' Max Planck Institute
#' for Demographic Research (Germany) and Vienna
#' Institute of Demography (Austria).
#' Code to create `HFD`
#' is in folder \file{data-raw/ssvd_hfd}
#' in the source code for the \pkg{bage}
#' package.
"HFD"


#' Scaled SVD Components from Human
#' Internal Migration Database
#'
#' Objects of class `"bage_ssvd"`
#' holding scaled SVD components derived from
#' data from the Human Internal Migration Database.
#' `HIMD_P1`, `HIMD_P5`, and `HIMD_R` each
#'  hold 5 components
#'
#' - `HIMD_P1` is derived from data on
#'   1-year migration probabilities, ie the
#'   probability that a person will migrate
#'   during a time interval of 1 year.
#' - `HIMD_P5` is derived from data on
#'   5-year migration probabilities, ie the
#'   probability that a person will migrate
#'   during a time interval of 5 years.
#' - `HIMD_R` is derived from data on 1-year
#'   migration probabilities, using the
#'   formula \eqn{r = -\log(1 - p)}.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2024"` (default) Data published on 2024-10-23
#'
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Dyrting, S. (2024, October 23).
#' Data from: [Estimating Complete Migration Probabilities
#' from Grouped Data](https://osf.io/vmrfk/).
#' Retrieved from osf.io/vmrfk
#' on 1 September 2025.
#' Code to create `HIMD_R`,
#' `HIMD_P1` and `HIMD_P5`
#' is in folder \file{data-raw/ssvd_himd}
#' in the source code for the \pkg{bage}
#' package.
"HIMD_R"

#' @rdname HIMD_R
#' @format NULL
"HIMD_P1"

#' @rdname HIMD_R
#' @format NULL
"HIMD_P5"


#' Scaled SVD Components from
#' Human Mortality Database
#'
#' An object of class `"bage_ssvd"`
#' holding scaled SVD components derived from
#' data from the
#' Human Mortality Database.
#' `HMD` holds 5 components.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2025"` (default) Data published on
#'   2025-09-25, all years
#' - `"v2025-50"` Data published on
#'   2025-09-25, 1950 and later
#' - `"v2024"` Data published on
#'   2024-02-26, all years
#' 
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Derived from data from the
#' [Human Mortality Database](https://www.mortality.org).
#' Max Planck Institute for
#' Demographic Research (Germany), University of California,
#' Berkeley (USA), and French Institute for Demographic Studies
#' (France). Code to create `HMD`
#' is in folder \file{data-raw/ssvd_hmd}
#' in the source code for the \pkg{bage}
#' package.
"HMD"


#' Scaled SVD Components from OECD
#' Labor Force Participation Data
#'
#' An object of class `"bage_ssvd"`
#' holding scaled SVD components
#' derived from labor force participation data
#' assembled by the OECD.
#' `LFP` holds 5 components.
#' 
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2025"` Data downloaded on 2025-10-17
#' 
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Derived from data in the "Labor Force Indicators"
#' table of the
#' [OECD Data Explorer](https://data-explorer.oecd.org).
#' Code to create `LFS`
#' is in folder \file{data-raw/ssvd_lfp} in
#' the source code for the \pkg{bage}
#' package.
"LFP"


#' Scaled SVD Components from
#' World Marriage Database
#'
#' Object of class `"bage_ssvd"`
#' holding scaled SVD components derived from
#' data from the census and survey data on marriage
#' assembled by the United Nations
#' Population Division.
#' `WMD_C` and `WMD_E` each hold 5 components.
#'
#' - `WMD_C` is based on data on the proportion
#'   of the population that is currently married.
#'   It should be used for modelling the proportion
#'   of people whose marital status is
#'   "Currently Married"
#' - `WMD_E` is based on data on the proportion
#'   of the population that has ever been married.
#'   It should be used for modelling the proportion
#'   of people whose marital status is
#'   "Ever Married".
#'
#' In both cases "marriage" includes de facto
#' marriages and consensual unions, in addition
#' to legal marriages.
#'
#' @format
#' Object of class `"bage_ssvd"`.
#'
#' Versions:
#'
#' - `"v2019"` (default) Data published in 2019
#' 
#' @seealso
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#' - [SVD()] A prior based on a scaled SVD
#'
#' @source Derived from data from the
#' *World Marriage Data 2019* database
#' available on the United Nations Population
#' Division website, and assembled by the
#' UNPD from national census and survey data.
#' Code to create `WMD`
#' is in folder \file{data-raw/ssvd_wmd}
#' in the source code for thet \pkg{bage}
#' package.
"WMD_C"

#' @rdname WMD_C
#' @format NULL
"WMD_E"


## Data for SVD ---------------------------------------------------------------

#' Data to Create Scaled SVD Object
#' Based on World Marriage Database
#'
#' A subset of the data needed to produce
#' a scaled SVD object, derived from
#' data from the World Marriage Database.
#' The data is formatted using function
#' `data_ssvd_wmd()` in package \pkg{bssvd}.
#'
#' @format
#' A tibble with 6 rows and with columns
#' `version`, `type`, `labels_age`,
#' `labels_sexgender`, `matrix`, and `offset`.
#'
#' @seealso
#' - [ssvd()] Function to create scaled SVD objects
#' - [WMD_C] Scaled SVD object based on a
#'   full set of World Marriage Database data.
#' - [Scaled SVDs][svds] Overview of scaled SVDs
#'   implemented in \pkg{bage}
#'
#' @source Derived from data from the
#' *World Marriage Data 2019* database
#' available on the United Nations Population
#' Division website, and assembled by the
#' UNPD from national census and survey data.
"data_wmd"
