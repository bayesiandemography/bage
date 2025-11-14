# Summarize Terms from a Fitted Model

Summarize the intercept, main effects, and interactions from a fitted
model.

## Usage

``` r
# S3 method for class 'bage_mod'
tidy(x, ...)
```

## Arguments

- x:

  Object of class `"bage_mod"`, typically created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

- ...:

  Unused. Included for generic consistency only.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Details

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
contains the following columns:

- `term` Name of the intercept, main effect, or interaction

- `prior` Specification for prior

- `n_par` Number of parameters

- `n_par_free` Number of free parameters

- `std_dev` Standard deviation for point estimates.

With some priors, the number of free parameters is less than the number
of parameters for that term. For instance, an
[`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
prior might use three vectors to represent 101 age groups so that the
number of parameters is 101, but the number of free parameters is 3.

`std_dev` is the standard deviation across elements of a term, based on
point estimates of those elements. For instance, if the point estimates
for a term with three elements are 0.3, 0.5, and 0.1, then the value for
`std_dev` is

    sd(c(0.3, 0.5, 0.1))

`std_dev` is a measure of the contribution of a term to variation in the
outcome variable.

## References

`std_dev` is modified from Gelman et al. (2014) *Bayesian Data Analysis.
Third Edition*. pp396–397.

## See also

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  Extract values for rates, probabilities, or means, together with
  original data

- [`components()`](https://generics.r-lib.org/reference/components.html)
  Extract values for hyper-parameters

- [`dispersion()`](https://bayesiandemography.github.io/bage/reference/dispersion.md)
  Extract values for dispersion

## Examples

``` r
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)
mod <- fit(mod)
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
tidy(mod)
#> # A tibble: 4 × 6
#>   term        prior  along n_par n_par_free std_dev
#>   <chr>       <chr>  <chr> <int>      <int>   <dbl>
#> 1 (Intercept) NFix() NA        1          1 NA     
#> 2 age         RW()   age      12         12  0.763 
#> 3 sex         NFix() NA        2          2  0.714 
#> 4 year        RW()   year     19         19  0.0935
```
