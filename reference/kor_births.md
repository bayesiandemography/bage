# Births in South Korea

Births and mid-year population by age of mother, region, and calendar
year, 2011-2023, plus regional data on GDP per capita and population
density.

## Usage

``` r
kor_births
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 1,872 rows and the following columns:

- `age` Five-year age groups from `"10-14" to `"50-54"\`

- `region` Administrative region

- `time` Calendar year, 2011-2023

- `births` Counts of births

- `popn` Mid-year population

- `gdp_pc_2023` Regional GDP per capita in 2023

- `dens_2020` Regional population density (people per km-squared) in
  2020

## Source

Tables "Live Births by Age Group of Mother, Sex and Birth Order for
Provinces", and "Resident Population in Five-Year Age Groups", on the
Korean Statistical Information Service website. Data downloaded on 24
September 2024. Data on GDP per capita and population density from
Wikipedia https://w.wiki/DMFA, data downloaded on 8 March 2025, and
https://w.wiki/DMF9, data downloaded on 8 March 2025.
