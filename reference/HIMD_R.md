# Scaled SVD Components from Human Internal Migration Database

Objects of class `"bage_ssvd"` holding scaled SVD components derived
from data from the [Human Internal Migration
Database](https://osf.io/vmrfk/). `HIMD_P1`, `HIMD_P5`, and `HIMD_R`
each object hold 5 components

## Usage

``` r
HIMD_R

HIMD_P1

HIMD_P5
```

## Format

An object of class bage_ssvd of length 1.

An object of class `bage_ssvd` of length 1.

An object of class `bage_ssvd` of length 1.

## Source

Dyrting, S. (2024, October 23). Data from: Estimating Complete Migration
Probabilities from Grouped Data. Retrieved from osf.io/vmrfk on 1
September 2025. Code to create `HIMD_R`, `HIMD_P1` and `HIMD_P5` is in
folder 'data-raw/ssvd_himd' in the source code for the bage package.

## Details

- `HIMD_P1` is derived from data on 1-year migration probabilities, ie
  the probability that a person will migrate during an interval of 1
  year.

- `HIMD_P5` is derived from data on 5-year migration probabilities, ie
  the probability that a person will migrate during an interval of 5
  years.

- `HIMD_R` is derived from data on 1-year migration probabilities, using
  the formula \\r = \log(1 - p)\\.

**Versions:**

- `"v2024"` Data from 23 October 2024 (the default)

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage
