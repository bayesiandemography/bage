# Known Prior

Treat an intercept, a main effect, or an interaction as fixed and known.

## Usage

``` r
Known(values)
```

## Arguments

- values:

  A numeric vector

## Value

An object of class `"bage_prior_known"`.

## See also

- [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md)
  Prior where level unknown, but variability known.

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
Known(-2.3)
#> Known(-2.3) 
Known(c(0.1, 2, -0.11))
#> Known(c(0.1,2,-0.11)) 
```
