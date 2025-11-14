# Data Models

The models for rates, probabilities, or means created with functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
can be extended by adding data models, also referred to as measurement
error models.

## Details

|                                                                                                             |                                                                               |             |              |            |
|-------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|-------------|--------------|------------|
| **Function**                                                                                                | **Assumptions about measurement error**                                       | **Poisson** | **Binomial** | **Normal** |
| [`set_datamod_miscount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_miscount.md)     | Reported outcome has undercount and overcount                                 | Yes         | No           | No         |
| [`set_datamod_undercount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_undercount.md) | Reported outcome has undercount                                               | Yes         | Yes          | No         |
| [`set_datamod_overcount()`](https://bayesiandemography.github.io/bage/reference/set_datamod_overcount.md)   | Reported outcome has overcount                                                | Yes         | No           | No         |
| [`set_datamod_noise()`](https://bayesiandemography.github.io/bage/reference/set_datamod_noise.md)           | Reported outcome unbiased, but with positive and negative measurement errors  | Yes\*       | No           | Yes        |
| [`set_datamod_exposure()`](https://bayesiandemography.github.io/bage/reference/set_datamod_exposure.md)     | Reported exposure unbiased, but with positive and negative measurement errors | Yes\*       | No           | No         |

\*Models with no dispersion term for rates.
