
# Submission, 19 November 2025, version 0.10.2

* This is a re-submission, responding to a problem picked up by one of
  Professor Ripley's checks on 17 November. Unit tests in version
  0.10.1 were leaving behind a file in the cache directory for the
  package. I have reproduced this problem under CRAN settings, and
  fixed it.
* For some reason, the error is not showing up in today's check results
  (https://cran.r-project.org/web/checks/check_results_bage.html) but
  the problem was definitely there, and required fixing.

# Submission, 17 November 2025, version 0.10.1

* In response to version 0.10.0 exceeding the 10 minute limit, I have
  substantially reduced the time required for tests and vignettes.

* My previous submission, version v.10.1 failed the CRAN
  pre-checks. If I have understood the logs correctly, this is because
  the checktime exceeded the 10 minute limit:
  
  the Flavor: r-devel-windows-x86_64
  Check: Overall checktime, Result: NOTE
  Overall checktime 12 min > 10 min
  
* I have added `skip_on_cran()` calls to all tests that take more than
  0.5 second on my machine, and also moved 9 of the 10 vignettes that
  come with the packages into an 'articles' folder, so that now only
  one vignette is built on CRAN. After these changes the running time
  for tests has decreased by 50% on my machine, and the compilation
  time for vignettes has reduced by 80%. Hopefully this will be enough
  to bring CRAN checktime below 10 minutes.


# Submission, 16 November 2025, version 0.10.0

* Current CRAN checks have notes for the three r-oldrel systems. All
  three notes refer to library size.


# Submission, 16 October 2025, version 0.9.8

- This submission is a response to an error detected in a test of the
  package on R Under development (unstable) (2025-10-13 r88916), using
  using platform: aarch64-apple-darwin25.0.0.
  
  The error message was as follows:

```
    ── Failure ('test-bage_mod-methods.R:3310:5'): 'replicate_data' works with mod_pois, rr3 confidential ──
  mean(ans_fit$deaths) (`actual`) not equal to mean(ans$deaths) (`expected`).
```
  
- Professor Ripley indicated that error is likely to have been
  triggered by the updating of TMB to version 1.9.18. 
  
- The failing test involves random draws. I have made the test
  more robust be increasing the number of draws, which should reduce
  its sensitivity to events such as package updates. (I have also done
  the same in similar replicate data tests.) The test is now passing
  on TMB 1.9.18, including with R 4.6.0.
  
- The output from the tests included a deprecation warning from
  Matrix, which I have addressed. Testing and generation of vignettes was
  also slightly exceeding the 10-minute maximum. I have modified the
  tests and vignettes in order to reduce the time required.
  


# Submission, 7 October 2025, version 0.9.7

- This submission is a response to an error detected in a test of the
  package under noLD:
  https://cran.r-project.org/web/checks/check_results_bage.html
  
  The error message was as follows:

Quitting from vig07_simulation.Rmd:57-61 [unnamed-chunk-4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<error/rlang_error>
Error in `expand1()`:
! D[i,i] is negative, i=25
---
Backtrace:
     ▆
  1. └─bage::report_sim(mod_est = mod)
  2.   └─base::lapply(s_sim, report_sim_inner)
  3.     └─bage (local) FUN(X[[i]], ...)
  4.       ├─generics::fit(mod_est, method = method, vars_inner = vars_inner)
  5.       └─bage:::fit.bage_mod(mod_est, method = method, vars_inner = vars_inner)
  6.         └─bage:::fit_default(...)
  7.           └─bage:::draw_vals_and_record(...)
  8.             └─bage:::make_stored_draws(mod = mod, est = est, prec = prec, map = map)
  9.               └─bage:::make_draws_post(est = est, prec = prec, map = map, n_draw = n_draw)
 10.                 └─sparseMVN::rmvn.sparse(n = n_draw, mu = mean, CH = CH, prec = TRUE)
 11.                   ├─Matrix::expand(CH)
 12.                   └─Matrix::expand(CH)
 13.                     ├─Matrix::expand1(x, "L")
 14.                     └─Matrix::expand1(x, "L")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Error: processing vignette 'vig07_simulation.Rmd' failed with diagnostics:
D[i,i] is negative, i=25
--- failed re-building ‘vig07_simulation.Rmd’

- The process for making multivariate normal draws was failing during
  a simulation in a vignette. I have taken two steps to avoid this
  happening again:
  
1. I have made the process for generating draws more robust, by
adding fallbacks if the generation fails. (I have also also
added fallbacks to the Cholesky factorization that proceeds the
multivariate draws.)
  
2. I have modified the simulation vignette. The vignette previously
used weakly-informative priors to generate synthentic
datasets. This could lead to extreme values and hence numerical
problems. The vignette now uses more strongly informative priors,
which gives better-behaved datasets.
   
The package runs on `rhub::rhub_check(platforms = "nold")` without
problems.

Because these changes are non-trivial, I have bumped the version
number to 0.9.7.

Checktime in r-devel-windows-x86_64 for the previous version of the
package was 13 minutes. I have added `testthat::skip_on_cran()` calls
to computationally intensive tests, which will hopefully bring the
checktime below 10 minutes.


# Submission, 1 October 2025, version 0.9.6

* Current CRAN checks have notes for the three r-oldrel systems. All
  three notes refer to library size.

# Submission, 20 July 2025, version 0.9.4

* Current CRAN checks have notes for the three r-oldrel systems. All
  three notes refer to library size, and I think are not something I
  can fix?


# Submission, 8 January 2025, version 0.9.0

* `devtools::check(remote = TRUE, manual = TRUE)` and
  `devtools::check_win_devel()` passing
* Some current CRAN checks have notes, but these refer to the compiled
  library size


# Revised submission, 23 August 2024, version 0.7.4

```
\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \dontrun with \donttest or let us know why you think
\dontrun{} is needed in your case.
-> ssvd.Rd
```

* Thank you, this is a good point! I had wrapped the example for
  function `ssvd()` in a `\dontrun{}` because it involved a call to
  another package that is still under development. On reflection, I
  think it is better to not expose function `ssvd()` to users yet. It
  is a function aimed at developers, rather than data analysts, and
  isn't needed elsewhere in the package. I will exposure function
  `ssvd()` to users again once the other package is more mature. In
  the meantime, I have changed the `@export` tag to `@noRd`, and removed
  all cross references to `ssvd()`. I have also bumped the version
  number to 0.7.4.



# Initial submission, 19 August 2024, version 0.7.3

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* There are currently no references describing the methods in the
  package. We plan to write papers on the package eventually. 
