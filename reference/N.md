# Normal Prior

Use independent draws from a normal distribution to model a main effect
or interaction. Typically used with variables other than age or time,
such as region or ethnicity, where there is no natural ordering.

## Usage

``` r
N(s = 1)
```

## Arguments

- s:

  Scale for the standard deviation. Default is `1`.

## Value

An object of class `"bage_prior_norm"`.

## Details

Argument `s` controls the size of errors. Smaller values for `s` tend to
give more tightly clustered estimates.

## Mathematical details

\$\$\beta_j \sim \text{N}(0, \tau^2)\$\$

where \\\beta\\ is the main effect or interaction.

Parameter \\\tau\\ has a half-normal prior \$\$\tau \sim \text{N}^+(0,
\mathtt{s}^2),\$\$ where `s` is provided by the user.

## See also

- [`NFix()`](https://bayesiandemography.github.io/bage/reference/NFix.md)
  Similar to `N()` but standard deviation parameter is supplied rather
  than estimated from data

- [priors](https://bayesiandemography.github.io/bage/reference/priors.md)
  Overview of priors implemented in bage

- [`set_prior()`](https://bayesiandemography.github.io/bage/reference/set_prior.md)
  Specify prior for intercept, main effect, or interaction

- [Mathematical
  Details](https://bayesiandemography.github.io/bage/articles/vig02_math.html)
  vignette

## Examples

``` r
N()
#>   N() 
#>          s: 1
N(s = 0.5)
#>   N(s=0.5) 
#>          s: 0.5
```
