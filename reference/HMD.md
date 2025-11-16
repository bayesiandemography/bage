# Scaled SVD Components from Human Mortality Database

An object of class `"bage_ssvd"` holding scaled SVD components derived
from data from the Human Mortality Database. `HMD` holds 5 components.

## Usage

``` r
HMD
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2025"` (default) Data published on 2025-09-25, all years

- `"v2025-50"` Data published on 2025-09-25, 1950 and later

- `"v2024"` Data published on 2024-02-26, all years

## Source

Derived from data from the [Human Mortality
Database](https://www.mortality.org). Max Planck Institute for
Demographic Research (Germany), University of California, Berkeley
(USA), and French Institute for Demographic Studies (France). Code to
create `HMD` is in folder `data-raw/ssvd_hmd` in the source code for the
bage package.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
