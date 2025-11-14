# Scaled SVD Components from Census School Attendance Data

An object of class `"bage_ssvd"` holding scaled SVD components derived
from census data on school attendance. The attendance data is assembed
by by the United Nations Statistics Division, and presented in the
"Population 5 to 24 years of age by school attendance, sex and
urban/rural residence" table in from the [Population Censuses'
Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets).
`CSA` holds 5 components.

## Usage

``` r
CSA
```

## Format

Object of class `"bage_ssvd"`.

## Source

Derived from data in the "Population 5 to 24 years of age by school
attendance, sex and urban/rural residence" table in from the "Population
Censuses' Datasets" database assembled by the United Nations Statistics
Division. Code to create `CSA` is in folder 'data-raw/ssvd_csa' in the
source code for the bage package.

## Details

**Versions:**

- `"v2025"` (default) version of data downloadd on 5 November 2025

## Warning

Compared other demographic processes, such as mortality, age-sex
patterns in school attendance have more variation across populations.
More components may be needed to obtain satisfactory representations of
actual age-sex patterns for school attendance than for other processes,
and even then the accuracy may not be as high.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage
