# Data to Create Scaled SVD Object Based on World Marriage Database

A subset of the data needed to produce a scaled SVD object, derived from
data from the World Marriage Database. The data is formatted using
function `data_ssvd_wmd()` in package bssvd.

## Usage

``` r
data_wmd
```

## Format

A tibble with 6 rows and with columns `version`, `type`, `labels_age`,
`labels_sexgender`, `matrix`, and `offset`.

## Source

Derived from data from the *World Marriage Data 2019* database available
on the United Nations Population Division website, and assembled by the
UNPD from national census and survey data.

## See also

- [`ssvd()`](https://bayesiandemography.github.io/bage/reference/ssvd.md)
  Function to create scaled SVD objects

- [WMD_C](https://bayesiandemography.github.io/bage/reference/WMD_C.md)
  Scaled SVD object based on a full set of World Marriage Database data.

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage
