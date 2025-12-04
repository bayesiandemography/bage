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
#>    draw   by    along  value
#>    <fct>  <fct> <int>  <dbl>
#>  1 Draw 1 By 1      1  0.289
#>  2 Draw 1 By 1      2 -0.975
#>  3 Draw 1 By 1      3 -0.100
#>  4 Draw 1 By 1      4 -0.233
#>  5 Draw 1 By 1      5 -1.08 
#>  6 Draw 1 By 1      6 -3.44 
#>  7 Draw 1 By 1      7 -4.42 
#>  8 Draw 1 By 1      8 -5.23 
#>  9 Draw 1 By 1      9 -3.10 
#> 10 Draw 1 By 1     10 -0.714
#> # ℹ 490 more rows

## prior that does not distinguish
x <- N()
generate(x, n_element = 20)
#> # A tibble: 500 × 3
#>    draw   element   value
#>    <fct>    <int>   <dbl>
#>  1 Draw 1       1 -0.922 
#>  2 Draw 1       2 -0.0736
#>  3 Draw 1       3  0.0539
#>  4 Draw 1       4 -1.05  
#>  5 Draw 1       5  0.774 
#>  6 Draw 1       6 -1.12  
#>  7 Draw 1       7 -0.368 
#>  8 Draw 1       8 -1.01  
#>  9 Draw 1       9 -0.141 
#> 10 Draw 1      10  0.172 
#> # ℹ 490 more rows

## SVD_AR(), SVD_RW(), and SVD_RW2()
## distinguish 'along' and 'by'
x <- SVD_AR(HFD)
generate(x, n_along = 5, n_by = 2)
#> # A tibble: 11,000 × 5
#>    draw   by    along age    value
#>    <fct>  <fct> <int> <fct>  <dbl>
#>  1 Draw 1 By 1      1 12    -12.2 
#>  2 Draw 1 By 1      1 13    -10.1 
#>  3 Draw 1 By 1      1 14     -8.24
#>  4 Draw 1 By 1      1 15     -6.61
#>  5 Draw 1 By 1      1 16     -5.23
#>  6 Draw 1 By 1      1 17     -4.25
#>  7 Draw 1 By 1      1 18     -3.55
#>  8 Draw 1 By 1      1 19     -3.03
#>  9 Draw 1 By 1      1 20     -2.71
#> 10 Draw 1 By 1      1 21     -2.51
#> # ℹ 10,990 more rows

## SVD() does not
x <- SVD(HFD)
generate(x, n_element = 10)
#> # A tibble: 11,000 × 4
#>    draw   element age    value
#>    <fct>    <int> <fct>  <dbl>
#>  1 Draw 1       1 12    -12.7 
#>  2 Draw 1       1 13    -10.5 
#>  3 Draw 1       1 14     -8.11
#>  4 Draw 1       1 15     -6.16
#>  5 Draw 1       1 16     -4.63
#>  6 Draw 1       1 17     -3.51
#>  7 Draw 1       1 18     -2.77
#>  8 Draw 1       1 19     -2.32
#>  9 Draw 1       1 20     -2.06
#> 10 Draw 1       1 21     -1.92
#> # ℹ 10,990 more rows
```
