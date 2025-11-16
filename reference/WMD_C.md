# Scaled SVD Components from World Marriage Database

Object of class `"bage_ssvd"` holding scaled SVD components derived from
data from the census and survey data on marriage assembled by the United
Nations Population Division. `WMD_C` and `WMD_E` each hold 5 components.

## Usage

``` r
WMD_C

WMD_E
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2019"` (default) Data published in 2019

## Source

Derived from data from the [World Marriage Data
2019](https://www.un.org/development/desa/pd/data/world-marriage-data),
created by the United Nations Population Division from national census
and survey data. Code to create `WMD` is in folder `data-raw/ssvd_wmd`
in the source code for thet bage package.

## Details

- `WMD_C` is based on data on the proportion of the population that is
  currently married. It should be used for modelling the proportion of
  people whose marital status is "Currently Married"

- `WMD_E` is based on data on the proportion of the population that has
  ever been married. It should be used for modelling the proportion of
  people whose marital status is "Ever Married".

In both cases "marriage" includes de facto marriages and consensual
unions, in addition to legal marriages.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
