# Generate Values from Priors

Generate draws from priors for model terms.

## Usage

``` r
# S3 method for class 'bage_prior_ar'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_drwrandom'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_drwzero'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_drw2random'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_drw2zero'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_known'
generate(x, n_element = 20, n_draw = 25, ...)

# S3 method for class 'bage_prior_lin'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_linar'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_linex'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_norm'
generate(x, n_element = 20, n_draw = 25, ...)

# S3 method for class 'bage_prior_normfixed'
generate(x, n_element = 20, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwrandom'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwrandomseasfix'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwrandomseasvary'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwzero'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwzeroseasfix'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rwzeroseasvary'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2random'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2randomseasfix'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2randomseasvary'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2zero'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2zeroseasfix'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_rw2zeroseasvary'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_spline'
generate(x, n_along = 20, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd'
generate(x, n_element = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_ar'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_drwrandom'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_drwzero'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_drw2random'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_drw2zero'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_rwrandom'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_rwzero'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_rw2random'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)

# S3 method for class 'bage_prior_svd_rw2zero'
generate(x, n_along = 5, n_by = 1, n_draw = 25, ...)
```

## Arguments

- x:

  Object of class `"bage_prior"`

- n_along:

  Number of elements of 'along' dimension. Default is `20`.

- n_by:

  Number of combinations of 'by' variables. Default is `1`.

- n_draw:

  Number of draws. Default is `25`.

- ...:

  Unused. Included for generic consistency only.

- n_element:

  Number of elements in term, in priors that do not distinguish 'along'
  and 'by' dimensions. Default is `20`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Some priors distinguish between 'along' and 'by' dimensions, while
others do not: see
[priors](https://bayesiandemography.github.io/bage/reference/priors.md)
for a complete list. Arguments `n_along` and `n_by` are used with priors
that make the distinction, and argument `n_element` is used with priors
that do not.

## See also

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in **bage**

## Examples

``` r
## prior that distinguishes 'along' and 'by'
x <- RW()
generate(x, n_along = 10, n_by = 2)
#> # A tibble: 500 × 4
#>    draw   by    along   value
#>    <fct>  <fct> <int>   <dbl>
#>  1 Draw 1 By 1      1  0.784 
#>  2 Draw 1 By 1      2  0.761 
#>  3 Draw 1 By 1      3  1.33  
#>  4 Draw 1 By 1      4  0.776 
#>  5 Draw 1 By 1      5 -0.214 
#>  6 Draw 1 By 1      6  0.576 
#>  7 Draw 1 By 1      7  0.611 
#>  8 Draw 1 By 1      8 -0.0431
#>  9 Draw 1 By 1      9 -1.06  
#> 10 Draw 1 By 1     10 -1.14  
#> # ℹ 490 more rows

## prior that does not distinguish
x <- N()
generate(x, n_element = 20)
#> # A tibble: 500 × 3
#>    draw   element    value
#>    <fct>    <int>    <dbl>
#>  1 Draw 1       1 -0.00216
#>  2 Draw 1       2  0.0598 
#>  3 Draw 1       3  0.0528 
#>  4 Draw 1       4  0.0534 
#>  5 Draw 1       5  0.130  
#>  6 Draw 1       6 -0.0931 
#>  7 Draw 1       7 -0.0144 
#>  8 Draw 1       8  0.0263 
#>  9 Draw 1       9 -0.00547
#> 10 Draw 1      10  0.0246 
#> # ℹ 490 more rows

## SVD_AR(), SVD_RW(), and SVD_RW2()
## distinguish 'along' and 'by'
x <- SVD_AR(HFD)
generate(x, n_along = 5, n_by = 2)
#> # A tibble: 11,000 × 5
#>    draw   by    along age    value
#>    <fct>  <fct> <int> <fct>  <dbl>
#>  1 Draw 1 By 1      1 12    -10.1 
#>  2 Draw 1 By 1      1 13     -7.58
#>  3 Draw 1 By 1      1 14     -5.47
#>  4 Draw 1 By 1      1 15     -4.23
#>  5 Draw 1 By 1      1 16     -3.35
#>  6 Draw 1 By 1      1 17     -2.83
#>  7 Draw 1 By 1      1 18     -2.45
#>  8 Draw 1 By 1      1 19     -2.18
#>  9 Draw 1 By 1      1 20     -2.09
#> 10 Draw 1 By 1      1 21     -2.06
#> # ℹ 10,990 more rows

## SVD() does not
x <- SVD(HFD)
generate(x, n_element = 10)
#> # A tibble: 11,000 × 4
#>    draw   element age    value
#>    <fct>    <int> <fct>  <dbl>
#>  1 Draw 1       1 12    -12.5 
#>  2 Draw 1       1 13    -10.5 
#>  3 Draw 1       1 14     -8.67
#>  4 Draw 1       1 15     -6.90
#>  5 Draw 1       1 16     -5.40
#>  6 Draw 1       1 17     -4.29
#>  7 Draw 1       1 18     -3.52
#>  8 Draw 1       1 19     -2.97
#>  9 Draw 1       1 20     -2.62
#> 10 Draw 1       1 21     -2.39
#> # ℹ 10,990 more rows
```
