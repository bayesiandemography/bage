# Scaled SVDs

Scaled SVDs contain information on typical age-patterns, or age-sex
patterns, for demographic processes, extracted from international
databases. The information is extracted using a singular value
decomposition (SVD), and then scaled to make it easier to formulate
priors.

Scaled SVDs can have multiple versions, based on data released at
different dates, or on subsets of the available data.

Some datasets, and hence some scaled SVDs, include information on age
but not on sex or gender.

## Details

|                                                                          |                                          |                                   |                                    |         |
|--------------------------------------------------------------------------|------------------------------------------|-----------------------------------|------------------------------------|---------|
| **Scaled SVD**                                                           | **Process**                              | **Source data**                   | **Versions**                       | **Sex** |
| [HFD](https://bayesiandemography.github.io/bage/reference/HFD.md)        | Fertility                                | Human Fertility Database          | `"v2025"`, `"v2024"`               | No      |
| [HIMD_R](https://bayesiandemography.github.io/bage/reference/HIMD_R.md)  | Internal migration: annual rates         | Human Internal Migration Database | `"v2024"`                          | No      |
| [HIMD_P1](https://bayesiandemography.github.io/bage/reference/HIMD_R.md) | Internal migration: 1-year probabilities | Human Internal Migration Database | `"v2024"`                          | No      |
| [HIMD_P5](https://bayesiandemography.github.io/bage/reference/HIMD_R.md) | Internal migration: 5-year probabilities | Human Internal Migration Database | `"v2024"`                          | No      |
| [HMD](https://bayesiandemography.github.io/bage/reference/HMD.md)        | Mortality                                | Human Mortality Database          | `"v2025"`, `"v2025_50"`, `"v2024"` | Yes     |
| [LFP](https://bayesiandemography.github.io/bage/reference/LFP.md)        | Labour Force Participation               | OECD                              | `"v2025"`                          | Yes     |
| [WMD_C](https://bayesiandemography.github.io/bage/reference/WMD_C.md)    | Currently married                        | World Migration Data              | `"v2019"`                          | Yes     |
| [WMD_E](https://bayesiandemography.github.io/bage/reference/WMD_C.md)    | Ever married                             | World Migration Data              | `"v2019"`                          | Yes     |
