# Fatal Injuries in New Zealand

Counts of fatal injuries in New Zealand, by age, sex, ethnicity, and
year, plus estimates of the population at risk.

## Usage

``` r
nzl_injuries
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 912 rows and the following columns:

- `age`: 5-year age groups, up to age 55-59

- `sex`: `"Female"` or `"Male"`

- `ethnicity`: `"Maori"` or `"Non Maori"`

- `year`: Calendar year

- `injuries`: Count of injuries, randomly rounded to base 3

- `popn`: Population on 30 June

## Source

Derived from data in tables "Estimated Resident Population by Age and
Sex (1991+) (Annual-Jun)" and "Maori Ethnic Group Estimated Resident
Population by Age and Sex (1991+) (Annual-Jun)" in the online database
Infoshare, and table "Count of fatal and serious non-fatal injuries by
sex, age group, ethnicity, cause, and severity of injury, 2000-2021" in
the online database NZ.Stat, on the Statistics New Zealand website. Data
downloaded on 1 January 2023.

## See also

- [datasets](https://bayesiandemography.github.io/bage/reference/datasets.md)
  Overview of datasets in bage
