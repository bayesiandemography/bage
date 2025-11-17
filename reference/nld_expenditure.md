# Per Capita Health Expenditure in the Netherlands, 2003-2011

Per capita health expenditure, in Euros, by diagnostic group, age group,
and year, in the Netherlands.

## Usage

``` r
nld_expenditure
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 1,296 rows and the following columns:

- `diag` Diagnostic group

- `age` 5-year age groups, with open age group of 85+

- `year` 2003, 2005, 2007, and 2011

- `value` Expenditures, in Euros

## Source

Calculated from data in table "Expenditure by disease, age and gender
under the System of Health Accounts (SHA) Framework : Current health
spending by age" from OECD database 'OECD.Stat' (downloaded on 25 May
2016) and in table "Historical population data and projections
(1950-2050)" from OECD database 'OECD.Stat' (downloaded 5 June 2016).

## See also

- [datasets](https://bayesiandemography.github.io/bage/reference/datasets.md)
  Overview of datasets in bage
