# People in One-Person Households in New Zealand

Counts of people in one-person households, and counts of people living
in any household, by age, region, and year.

## Usage

``` r
nzl_households
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 528 rows and the following columns:

- `age`: 5-year age groups, with open age group of 65+

- `region`: Region within New Zealand

- `year`: Calendar year

- `oneperson`: Count of people living in one-person households

- `total`: Count of people living in all types of household

## Source

Derived from data in table "Household composition by age group, for
people in households in occupied private dwellings, 2006, 2013, and 2018
Censuses (RC, TA, DHB, SA2)" in the online database NZ.Stat, on the
Statistics New Zealand website. Data downloaded on 3 January 2023.

## See also

- [datasets](https://bayesiandemography.github.io/bage/reference/datasets.md)
  Overview of datasets in bage
