# Confidentialization

The models for rates, probabilities, or means created with functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
can be extended by adding descriptions of confidentalization procedures
applied to the outcome variable.

## Details

**Data models for outcome variable**

|                                                                                                         |                                           |          |           |          |
|---------------------------------------------------------------------------------------------------------|-------------------------------------------|----------|-----------|----------|
| **Function**                                                                                            | **Confidentialization procedure**         | **pois** | **binom** | **norm** |
| [`set_confidential_rr3()`](https://bayesiandemography.github.io/bage/reference/set_confidential_rr3.md) | Outcome randomly rounded to multiple of 3 | Yes      | Yes       | No       |
