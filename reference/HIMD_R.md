# Scaled SVD Components from Human Internal Migration Database

Objects of class `"bage_ssvd"` holding scaled SVD components derived
from data from the Human Internal Migration Database. `HIMD_P1`,
`HIMD_P5`, and `HIMD_R` each hold 5 components

## Usage

``` r
HIMD_R

HIMD_P1

HIMD_P5
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2024"` (default) Data published on 2024-10-23

## Source

Dyrting, S. (2024, October 23). Data from: [Estimating Complete
Migration Probabilities from Grouped Data](https://osf.io/vmrfk/).
Retrieved from osf.io/vmrfk on 1 September 2025. Code to create
`HIMD_R`, `HIMD_P1` and `HIMD_P5` is in folder `data-raw/ssvd_himd` in
the source code for the bage package.

## Details

- `HIMD_P1` is derived from data on 1-year migration probabilities, ie
  the probability that a person will migrate during a time interval of 1
  year.

- `HIMD_P5` is derived from data on 5-year migration probabilities, ie
  the probability that a person will migrate during a time interval of 5
  years.

- `HIMD_R` is derived from data on 1-year migration probabilities, using
  the formula \\r = -\log(1 - p)\\.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
