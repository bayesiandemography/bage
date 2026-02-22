# Priors for Intercept, Main Effects, Interactions

The models created with
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
and
[`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
include terms such as age effects and region-time interactions. Each of
these terms requires a prior distribution. Current options for these
priors are summarised in the table below.

## Details

|                                                                                     |                                                                                                                                                                        |                                                                                             |              |              |
|-------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|--------------|--------------|
| **Prior**                                                                           | **Description**                                                                                                                                                        | **Uses**                                                                                    | **Forecast** | **Along/By** |
| [`N()`](https://bayesiandemography.github.io/bage/reference/N.md)                   | Elements drawn from normal distribution                                                                                                                                | Term with no natural order                                                                  | Yes          | No           |
| [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md)             | [`N()`](https://bayesiandemography.github.io/bage/reference/N.md) with standard deviation fixed                                                                        | Term with few elements                                                                      | Yes          | No           |
| [`Known()`](https://bayesiandemography.github.io/bage/reference/Known.md)           | Values treated as known                                                                                                                                                | Simulations, prior knowledge                                                                | No           | No           |
| [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)                 | Random walk                                                                                                                                                            | Smoothing                                                                                   | Yes          | Yes          |
| [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)               | Second-order random walk                                                                                                                                               | Like [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md), but with trends   | Yes          | Yes          |
| [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md)               | Damped random walk                                                                                                                                                     | Smoothing, forecasting                                                                      | Yes          | Yes          |
| [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md)             | Damped second-order random walk                                                                                                                                        | Like [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md), but with trends | Yes          | Yes          |
| [`RW2_Infant()`](https://bayesiandemography.github.io/bage/reference/RW2_Infant.md) | [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md) with infant indicator                                                                            | Mortality age profiles                                                                      | No           | Yes          |
| [`RW_Seas()`](https://bayesiandemography.github.io/bage/reference/RW_Seas.md)       | [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md), with seasonal effect                                                                              | Terms involving time                                                                        | Yes          | Yes          |
| [`RW2_Seas()`](https://bayesiandemography.github.io/bage/reference/RW2_Seas.md)     | [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md), with seasonal effect                                                                            | Term involving time                                                                         | Yes          | Yes          |
| [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)                 | Auto-regressive prior of order *k*                                                                                                                                     | Mean reversion, forecasting                                                                 | Yes          | Yes          |
| [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)               | Special case of [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)                                                                                    | Mean reversion, forecasting                                                                 | Yes          | Yes          |
| [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)               | Linear trend, with independent errors                                                                                                                                  | Parsimonious model for time                                                                 | Yes          | Yes          |
| [`Lin_AR()`](https://bayesiandemography.github.io/bage/reference/Lin_AR.md)         | Linear trend, with AR errors                                                                                                                                           | Term involving time, forecasting                                                            | Yes          | Yes          |
| [`Lin_AR1()`](https://bayesiandemography.github.io/bage/reference/Lin_AR1.md)       | Linear trend, with AR1 errors                                                                                                                                          | Terms involving time, forecasting                                                           | Yes          | Yes          |
| [`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)                 | P-Spline (penalised spline)                                                                                                                                            | Smoothing, eg over age                                                                      | No           | Yes          |
| [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)               | Age-sex profile based on SVD                                                                                                                                           | Age or age-sex                                                                              | No           | No           |
| [`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)         | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md)     | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)   | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_Lin()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`Lin()`](https://bayesiandemography.github.io/bage/reference/Lin.md)   | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)         | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)     | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md)   | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_DRW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)        | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`DRW()`](https://bayesiandemography.github.io/bage/reference/DRW.md)   | Age or age-sex and time                                                                     | Yes          | Yes          |
| [`SVD_DRW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)       | [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md), but coefficients follow [`DRW2()`](https://bayesiandemography.github.io/bage/reference/DRW2.md) | Age or age-sex and time                                                                     | Yes          | Yes          |

## 'Along' and 'by' dimensions

Priors for interaction terms often consist of a time-series-style model
along one dimension, with a separate series for each combination of the
remaining dimensions. For instance, a prior for an age-sex-time
interaction might consist of a separate random walk along time for each
combination of age-group and sex. In bage the dimension with the
time-series-type model is referred to as the 'along' dimension, and the
remaining dimensions are referred to as the 'by' dimensions.

## Default prior

If no prior is specified for a term, then bage assigns the term a
default prior using the following algorithm:

- if the term has one or two elements, use
  [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md);

- otherwise, if the term involves time, use
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
  with time as the 'along' dimension;

- otherwise, if the term involves age, use
  [`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md),
  with age as the 'along' dimension;

- otherwise, use
  [`N()`](https://bayesiandemography.github.io/bage/reference/N.md).

## Forecasting

A model can only be used for forecasting if

- the model includes a time dimension, and

- the prior for the time dimension supports forecasting.

If necessary, the time dimension can be identified using
[`set_var_time()`](https://bayesiandemography.github.io/bage/reference/set_var_time.md).
The table above lists the priors that support forecasting.
