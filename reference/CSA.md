# Scaled SVD Components from Census School Attendance Data

An object of class `"bage_ssvd"` holding scaled SVD components derived
from census data on school attendance. The attendance data is assembed
by the United Nations Statistics Division. `CSA` holds 5 components.

## Usage

``` r
CSA
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2025"` (default). Data downloaded on 2025-11-05

## Source

Derived from data in the "Population 5 to 24 years of age by school
attendance, sex and urban/rural residence" table from the [Population
Censuses'
Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets)
database assembled by the United Nations Statistics Division. Code to
create `CSA` is in folder `data-raw/ssvd_csa` in the source code for the
bage package.

## Warning

Compared other age-sex patterns for other demographic processes such as
mortality, age-sex patterns for school attendance show substantial
variation across populations. More components may be needed to obtain
satisfactory models of age-sex patterns for school attendance than for
other processes.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
