# Scaled SVD Components from Human Fertility Database

An object of class `"bage_ssvd"` holding scaled SVD components derived
from data from the Human Fertility Database. `HFD` holds 5 components.

## Usage

``` r
HFD
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2025"` (default) Data published on 2025-07-24

- `"v2024"` Data published on October 2024-10-23

## Source

Derived from data from the [Human Fertility
Database](https://www.humanfertility.org). Max Planck Institute for
Demographic Research (Germany) and Vienna Institute of Demography
(Austria). Code to create `HFD` is in folder `data-raw/ssvd_hfd` in the
source code for the bage package.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
