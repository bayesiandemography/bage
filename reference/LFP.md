# Scaled SVD Components from OECD Labor Force Participation Data

An object of class `"bage_ssvd"` holding scaled SVD components derived
from labor force participation data assembled by the OECD. `LFP` holds 5
components.

## Usage

``` r
LFP
```

## Format

Object of class `"bage_ssvd"`.

Versions:

- `"v2025"` Data downloaded on 2025-10-17

## Source

Derived from data in the "Labor Force Indicators" table of the [OECD
Data Explorer](https://data-explorer.oecd.org). Code to create `LFS` is
in folder `data-raw/ssvd_lfp` in the source code for the bage package.

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  A prior based on a scaled SVD
