# Create Object to Hold Data from a Scaled SVD

Create an object of class `"bage_ssvd"` to hold results from a scaled
[Singular Value Decomposition](https://rdrr.io/r/base/svd.html) (SVD)
with `n_comp` components.

## Usage

``` r
ssvd(data)
```

## Arguments

- data:

  A data frame. See Details for description.

## Value

An object of class `"bage_ssvd"`.

## Details

`data` has the following columns:

- `version` Vintage of data

- `type` Type of decomposition. Choices are "total", "joint", and
  "indep".

- `labels_age` Age labels for individual rows of matrices within
  `matrix` and individual elements of vectors within `offset`.

- `labels_sexgender` Sex/gender labels for individual rows of matrices
  within `matrix` and individual elements of vectors within `offset`, or
  `NULL`. `NULL` when `sexgender` is `"total"`, since in this case
  results average across sexes/genders.

- `matrix` List column of sparse matrices. Must have rownames. Must not
  have NAs. When `type` is `"total"` or `"joint"`, each matrix has
  `n_comp` columns. When `"type"` is `"indep"`, each matrix has
  `2 * n_comp` columns.

- `offset` List column of vectors. Must have names, which are identical
  to the rownames of the corresponding element of `matrix`.

`data` would normally be constructed using functions in package
[bssvd](https://bayesiandemography.github.io/bssvd/).

## See also

- [Scaled
  SVDs](https://bayesiandemography.github.io/bage/reference/svds.md)
  Overview of scaled SVDs implemented in bage

- [`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
  Prior based on scaled SVD

## Examples

``` r
ssvd(data_wmd)
#> 
#> ------------------------------------------------------------ 
#> <Object of class "bage_ssvd">
#> 
#> versions:
#>      v2019 
#> 
#> sex/gender labels:
#>      Female 
#>      Male 
#> 
#> age labels:
#>      15-19, 20-24, 25-29, ..., 40-44, 45-49 
#>      15-19, 20-24, 25-29, ..., 45-49, 50-54 
#>      15-19, 20-24, 25-29, ..., 50-54, 55-59 
#> ------------------------------------------------------------ 
```
