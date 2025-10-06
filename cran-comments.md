
# Submission, 6 October 2025, version 0.9.7

- This submission is a response to an error detected by Prof. Ripley
  when testing `bage` under noLD:
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

- It appears that the Cholesky factorization was failing without long
  doubles. I'm grateful to have this vulnerability pointed out, and
  have fixed it by wrapping the call to 



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
