# Specify RR3 Confidentialization

Specify a confidentialization procedure where the outcome variable is
randomly rounded to a multiple of 3.

## Usage

``` r
set_confidential_rr3(mod)
```

## Arguments

- mod:

  An object of class `"bage_mod"`, created with
  [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  or
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md).

## Value

A revised version of `mod`.

## Details

`set_confidential_rr3()` can only be used with Poisson and binomial
models (created with
[`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md)
and
[`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md).)

Random rounding to base 3 (RR3) is a confidentialization technique that
is sometimes applied by statistical agencies. The procedure for
randomly-rounding an integer value \\n\\ is as follows:

- If \\n\\ is divisible by 3, leave it unchanged

- If dividing \\n\\ by 3 leaves a remainder of 1, then round down
  (subtract 1) with probability 2/3, and round up (add 2) with
  probability 1/3.

- If dividing \\n\\ by 3 leaves a remainder of 1, then round down
  (subtract 2) with probability 1/3, and round up (add 1) with
  probability 2/3.

If `set_confidential_rr3()` is applied to a fitted model,
`set_confidential_rr3()`
[unfits](https://bayesiandemography.github.io/bage/reference/unfit.md)
the model, deleting existing estimates.

## See also

- [confidential](https://bayesiandemography.github.io/bage/reference/confidential.md)
  Overview of confidentialization procedures currently modeled in
  **bage**

- [`mod_pois()`](https://bayesiandemography.github.io/bage/reference/mod_pois.md),
  [`mod_binom()`](https://bayesiandemography.github.io/bage/reference/mod_binom.md),
  [`mod_norm()`](https://bayesiandemography.github.io/bage/reference/mod_norm.md)
  Specify a model for rates, probabilities, or means

## Examples

``` r
## 'injuries' variable in 'nzl_injuries' dataset
## has been randomly rounded to base 3
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = nzl_injuries,
                exposure = popn) |>
  set_confidential_rr3() |>
  fit()
#> Building log-posterior function...
#> Finding maximum...
#> Drawing values for hyper-parameters...
```
