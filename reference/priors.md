# Priors for Intercept, Main Effects, Interactions

The models created with functions
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
always include an intercept, and typically include main effects and
interactions formed from variables in input data. Most models, for
instance include an age effect, and many include an interaction between
age and sex/gender, or age and time.

The intercept, main effects, and interactions all have prior models that
capture the expected behavior of the term. Current choices for priors
summarised in the table below.

Priors where 'forecast' is yes can be used in forecasts for a
time-varying terms such as an age-time interactions.

Priors where 'along' is yes distinguish between 'along' and 'by'
dimensions.

## Details

|                                                                                     |                                                                                                                                                                        |                                                                                             |              |           |
|-------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|--------------|-----------|
| **Prior**                                                                           | **Description**                                                                                                                                                        | **Uses**                                                                                    | **Forecast** | **Along** |
| [`N()`](https://bayesiandemography.github.io/bage/reference/N.md)                   | Elements drawn from normal distribution                                                                                                                                | Term with no natural order                                                                  | Yes          | No        |
| [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md)             | [`N()`](https://bayesiandemography.github.io/bage/reference/N.md) with standard deviation fixed                                                                        | Term with few elements                                                                      | Yes          | No        |
| [`Known()`](https://bayesiandemography.github.io/bage/reference/Known.md)           | Values treated as known                                                                                                                                                | Simulations, prior knowledge                                                                | No           | No        |
| [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)                 | Random walk                                                                                                                                                            | Smoothing                                                                                   | Yes          | Yes       |
| [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)               | Second-order random walk                                                                                                                                               | Like [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md), but with trends   | Yes          | Yes       |
| [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md)               | Damped random walk                                                                                                                                                     | Smoothing, forecasting                                                                      | Yes          | Yes       |
| [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md)             | Damped second-order random walk                                                                                                                                        | Like [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md), but with trends | Yes          | Yes       |
| [`RW2_Infant()`](https://bayesiandemography.github.io/bage/reference/RW2_Infant.md) | [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md) with infant indicator                                                                            | Mortality age profiles                                                                      | No           | Yes       |
| [`RW_Seas()`](https://bayesiandemography.github.io/bage/reference/RW_Seas.md)       | [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md), with seasonal effect                                                                              | Terms involving time                                                                        | Yes          | Yes       |
| [`RW2_Seas()`](https://bayesiandemography.github.io/bage/reference/RW2_Seas.md)     | [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md), with seasonal effect                                                                            | Term involving time                                                                         | Yes          | Yes       |
| [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)                 | Auto-regressive prior of order *k*                                                                                                                                     | Mean reversion, forecasting                                                                 | Yes          | Yes       |
| [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)               | Special case of [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)                                                                                    | Mean reversion, forecasting                                                                 | Yes          | Yes       |
| [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)               | Linear trend, with independent errors                                                                                                                                  | Parsimonious model for time                                                                 | Yes          | Yes       |
| [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)         | Linear trend, with AR errors                                                                                                                                           | Term involving time, forecasting                                                            | Yes          | Yes       |
| [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)       | Linear trend, with AR1 errors                                                                                                                                          | Terms involving time, forecasting                                                           | Yes          | Yes       |
| [`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)                 | P-Spline (penalised spline)                                                                                                                                            | Smoothing, eg over age                                                                      | No           | Yes       |
| [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)               | Age-sex profile based on SVD                                                                                                                                           | Age or age-sex                                                                              | No           | No        |
| [`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)         | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)     | Age or age-sex and time                                                                     | Yes          | Yes       |
| [`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)   | Age or age-sex and time                                                                     | Yes          | Yes       |
| [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)         | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)     | Age or age-sex and time                                                                     | Yes          | Yes       |
| [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)   | Age or age-sex and time                                                                     | Yes          | Yes       |
| [`SVD_DRW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md)   | Age or age-sex and time                                                                     | Yes          | Yes       |
| [`SVD_DRW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)       | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md) | Age or age-sex and time                                                                     | Yes          | Yes       |

## Default prior

The rule for selecting a default prior for a term is:

- if term has less than 3 elements, use
  [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md);

- otherwise, if the term involves time, use
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
  with time as the \`along' dimension;

- otherwise, if the term involves age, use
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
  with age as the \`along' dimension;

- otherwise, use
  [`N()`](https://bayesiandemography.github.io/bage/reference/N.md).
