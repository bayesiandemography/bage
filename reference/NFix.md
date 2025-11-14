# Normal Prior with Fixed Variance

Normal prior where, in contrast to
[`N()`](https://bayesiandemography.github.io/bage/reference/N.md), the
variance is treated as fixed and known. Typically used for main effects
or interactions where there are too few elements to reliably estimate
variance from the available data.

## Usage

``` r
NFix(sd = 1)
```

## Arguments

- sd:

  Standard deviation. Default is `1`.

## Value

An object of class `"bage_prior_normfixed"`.

## Details

`NFix()` is the default prior for the intercept.

## Mathematical details

\$\$\beta_j \sim \text{N}(0, \tau^2)\$\$

where \\\beta\\ is the main effect or interaction, and a value for `sd`
is supplied by the user.

## See also

- [`N()`](https://bayesiandemography.github.io/bage/reference/N.md)
  Similar to `NFix()`, but standard deviation parameter is estimated
  from the data rather than being fixed in advance

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
NFix()
#>   NFix() 
#>         sd: 1
NFix(sd = 10)
#>   NFix(sd=10) 
#>         sd: 10
```
