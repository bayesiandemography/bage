# Infant Mortality in Sweden

Counts of births and infant deaths in Sweden by county and year,
1995-2015

## Usage

``` r
swe_infant
```

## Format

A tibble with 441 rows and the following columns:

- `county`: A factor with 21 levels, where the levels are ordered by
  number of births, from `"Stockholm"` down to `"Gotland"`

- \`time: Calendar year

- `births`: Count of births

- `deaths`: Count of infant deaths

## Source

Database "Live births by region, mother's age and child's sex. Year
1968 - 2017" and database "Deaths by region, age (during the year) and
sex. Year 1968 - 2017" on the Statistics Sweden website. Downloaded on
13 July 2018.

## Details

Dataset used in Chapter 11 of the book *Bayesian Demographic Estimation
and Forecasting*.

## References

Bryant J and Zhang J. 2018. *Bayesian Demographic Estimation and
Forecasting*. CRC Press.
