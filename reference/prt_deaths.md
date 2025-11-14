# Deaths in Portugal

Deaths and exposure in Portugal, by age, sex, and year.

## Usage

``` r
prt_deaths
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 3,168 rows and the following columns:

- `age`: Age groups `"0"`, `"1-4"`, "5-9", ..., `"95-99"`, `"100+"`

- `sex`: `"Female"` or `"Male"`

- `time`: Calendar year

- `deaths`: Count of deaths

- `exposure`: Person-years lived by population

## Source

Human Mortality Database. University of California, Berkeley (USA), and
Max Planck Institute for Demographic Research (Germany). Available at
<https://www.mortality.org>. (data downloaded on 17 July 2018).

## Details

The data are from the Human Mortality Database. Deaths are rounded to
the nearest integer. More recent versions, and a comprehensive
description of the data, are available at the HMD website.
