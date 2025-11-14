# Extract Components used by SVD Summary

Extract the matrix and offset used by a scaled SVD summary of a
demographic database.

## Usage

``` r
# S3 method for class 'bage_ssvd'
components(
  object,
  v = NULL,
  n_comp = NULL,
  indep = NULL,
  age_labels = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `"bage_ssvd"`.

- v:

  Version of scaled SVD components to use. If no value is suppled, the
  most recent version is used.

- n_comp:

  The number of components. The default is half the total number of
  components of `object`.

- indep:

  Whether to use independent or joint SVDs for each sex/gender, if the
  data contains a sex/gender variable. The default is to use independent
  SVDs. To obtain results for the total population when the data
  contains a sex/gender variable, set `indep` to `NA`.

- age_labels:

  Age labels for the desired age or age-sex profile. If no labels are
  supplied, the most detailed profile available is used.

- ...:

  Not currently used.

## Value

A tibble with the offset and components.

## Scaled SVDs of demographic databases in bage

- [`HMD`](https://bayesiandemography.github.io/bage/reference/HMD.md)
  Mortality rates from the [Human Mortality
  Database](https://www.mortality.org).

- [`HFD`](https://bayesiandemography.github.io/bage/reference/HFD.md)
  Fertility rates from the [Human Fertility
  Database](https://www.humanfertility.org).

- [`LFP`](https://bayesiandemography.github.io/bage/reference/LFP.md)
  Labor forcce participation rates from the
  [OECD](https://data-explorer.oecd.org).

## See also

- [generate()](https://generics.r-lib.org/reference/generate.html)
  Randomly generate age-profiles, or age-sex profiles, based on a scaled
  SVD summary.

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  SVD prior for terms involving age.

- [`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
  [`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
  Dynamic SVD priors for terms involving age and time.

- [`poputils::age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.html)
  Generate age labels.

## Examples

``` r
## females and males modeled independently
components(LFP, n_comp = 3)
#> # A tibble: 104 × 4
#>    component sex    age     value
#>    <fct>     <chr>  <chr>   <dbl>
#>  1 Offset    Female 15-19 -1.02  
#>  2 Offset    Female 20-24  0.470 
#>  3 Offset    Female 25-29  0.949 
#>  4 Offset    Female 30-34  0.924 
#>  5 Offset    Female 35-39  1.02  
#>  6 Offset    Female 40-44  1.12  
#>  7 Offset    Female 45-49  1.05  
#>  8 Offset    Female 50-54  0.729 
#>  9 Offset    Female 55-59  0.0967
#> 10 Offset    Female 60-64 -0.947 
#> # ℹ 94 more rows

## joint model for females and males
components(LFP, indep = FALSE, n_comp = 3)
#> # A tibble: 104 × 4
#>    component sex    age    value
#>    <fct>     <chr>  <chr>  <dbl>
#>  1 Offset    Female 15-19 -1.03 
#>  2 Offset    Female 20-24  0.496
#>  3 Offset    Female 25-29  0.936
#>  4 Offset    Female 30-34  0.895
#>  5 Offset    Female 35-39  1.00 
#>  6 Offset    Female 40-44  1.12 
#>  7 Offset    Female 45-49  1.06 
#>  8 Offset    Female 50-54  0.740
#>  9 Offset    Female 55-59  0.108
#> 10 Offset    Female 60-64 -0.949
#> # ℹ 94 more rows

## females and males combined
components(LFP, indep = NA, n_comp = 3)
#> # A tibble: 52 × 3
#>    component age    value
#>    <fct>     <chr>  <dbl>
#>  1 Offset    15-19 -0.839
#>  2 Offset    20-24  0.774
#>  3 Offset    25-29  1.48 
#>  4 Offset    30-34  1.58 
#>  5 Offset    35-39  1.66 
#>  6 Offset    40-44  1.70 
#>  7 Offset    45-49  1.59 
#>  8 Offset    50-54  1.26 
#>  9 Offset    55-59  0.632
#> 10 Offset    60-64 -0.434
#> # ℹ 42 more rows

## specify age groups
labels <- poputils::age_labels(type = "five", min = 15, max = 60)
components(LFP, age_labels = labels)
#> # A tibble: 72 × 4
#>    component sex    age    value
#>    <fct>     <chr>  <chr>  <dbl>
#>  1 Offset    Female 15-19 -1.16 
#>  2 Offset    Female 20-24  0.475
#>  3 Offset    Female 25-29  0.986
#>  4 Offset    Female 30-34  1.00 
#>  5 Offset    Female 35-39  1.10 
#>  6 Offset    Female 40-44  1.18 
#>  7 Offset    Female 45-49  1.09 
#>  8 Offset    Female 50-54  0.765
#>  9 Offset    Female 55-59  0.122
#> 10 Offset    Male   15-19 -0.888
#> # ℹ 62 more rows
```
