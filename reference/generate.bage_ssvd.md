# Generate Random Age or Age-Sex Profiles

Generate random age or age-sex profiles from an object of class
`"bage_ssvd"`. An object of class `"bage_ssvd"` holds results from an
[SVD](https://rdrr.io/r/base/svd.html) decomposition of demographic
data.

## Usage

``` r
# S3 method for class 'bage_ssvd'
generate(
  x,
  v = NULL,
  n_draw = 20,
  n_comp = NULL,
  indep = NULL,
  age_labels = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"bage_ssvd"`.

- v:

  Version of data to use.

- n_draw:

  Number of random draws to generate.

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

  Unused. Included for generic consistency only.

## Value

A tibble

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

- [components()](https://generics.r-lib.org/reference/components.html)
  Components used by SVD prior.

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  SVD prior for term involving age.

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
generate(HMD) 
#> # A tibble: 4,440 × 4
#>    draw   sex   age   value
#>    <fct>  <fct> <fct> <dbl>
#>  1 Draw 1 Male  0     -2.34
#>  2 Draw 1 Male  1     -4.14
#>  3 Draw 1 Male  2     -4.93
#>  4 Draw 1 Male  3     -5.40
#>  5 Draw 1 Male  4     -5.78
#>  6 Draw 1 Male  5     -6.07
#>  7 Draw 1 Male  6     -6.30
#>  8 Draw 1 Male  7     -6.53
#>  9 Draw 1 Male  8     -6.76
#> 10 Draw 1 Male  9     -6.94
#> # ℹ 4,430 more rows

## joint model for females and males
generate(HMD, indep = FALSE) 
#> # A tibble: 4,440 × 4
#>    draw   sex    age   value
#>    <fct>  <fct>  <fct> <dbl>
#>  1 Draw 1 Female 0     -3.02
#>  2 Draw 1 Female 1     -4.91
#>  3 Draw 1 Female 2     -5.75
#>  4 Draw 1 Female 3     -6.26
#>  5 Draw 1 Female 4     -6.64
#>  6 Draw 1 Female 5     -6.94
#>  7 Draw 1 Female 6     -7.22
#>  8 Draw 1 Female 7     -7.48
#>  9 Draw 1 Female 8     -7.69
#> 10 Draw 1 Female 9     -7.87
#> # ℹ 4,430 more rows

## SVD for females and males combined
generate(HMD, indep = NA)
#> # A tibble: 2,220 × 3
#>    draw   age   value
#>    <fct>  <fct> <dbl>
#>  1 Draw 1 0     -4.18
#>  2 Draw 1 1     -6.30
#>  3 Draw 1 2     -6.80
#>  4 Draw 1 3     -7.06
#>  5 Draw 1 4     -7.23
#>  6 Draw 1 5     -7.35
#>  7 Draw 1 6     -7.44
#>  8 Draw 1 7     -7.52
#>  9 Draw 1 8     -7.58
#> 10 Draw 1 9     -7.62
#> # ℹ 2,210 more rows

## specify age groups
labels <- poputils::age_labels(type = "lt", max = 60)
generate(HMD, age_labels = labels)
#> # A tibble: 560 × 4
#>    draw   sex    age   value
#>    <fct>  <fct>  <fct> <dbl>
#>  1 Draw 1 Female 0     -2.76
#>  2 Draw 1 Female 1-4   -4.55
#>  3 Draw 1 Female 5-9   -4.67
#>  4 Draw 1 Female 10-14 -4.57
#>  5 Draw 1 Female 15-19 -4.20
#>  6 Draw 1 Female 20-24 -4.13
#>  7 Draw 1 Female 25-29 -3.98
#>  8 Draw 1 Female 30-34 -3.71
#>  9 Draw 1 Female 35-39 -3.37
#> 10 Draw 1 Female 40-44 -3.02
#> # ℹ 550 more rows
```
