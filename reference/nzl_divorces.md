# Divorces in New Zealand

Counts of divorces and population, by age, sex, and calendar year, in
New Zealand, 2011-2021.

## Usage

``` r
nzl_divorces
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 242 rows and the following columns:

- `age`: 5-year age groups, `"15-19"` to `"65+"`

- `sex`: `"Female"` or `"Male"`

- `time`: Calendar year

- `divorces`: Numbers of divorces during year

- `population`: Person-years lived during year

## Source

Divorce counts from data in table "Age at divorces by sex (marriages and
civil unions) (Annual-Dec)" in the online database Infoshare on the
Statistics New Zealand website. Data downloaded on 22 March 2023.
Population estimates derived from data in table "Estimated Resident
Population by Age and Sex (1991+) (Annual-Dec)" in the online database
Infoshare on the Statistics New Zealand website. Data downloaded on 26
March 2023.

## See also

- [datasets](https://bayesiandemography.github.io/bage/reference/datasets.md)
  Overview of datasets in bage
