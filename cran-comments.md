
# Submission, 9 July 2025, version 0.9.4

* Current CRAN checks have notes for the three r-oldrel flavours. All
  three notes refer to library size, and are not something I can
  change.
* When running `rhub::rub_check(platforms = "valgrind")` on `bage`, I
  obtained the following errors for the first example in
  'man/augment.bage_mod.Rd':

==3168==
==3168==    by 0x491B21E: coerceToReal (coerce.c:583)
==3168==    by 0x491B21E: Rf_coerceVector (coerce.c:1289)
==3168==    by 0x2901FE5A: random1 (random.c:118)
==3168==    by 0x4957CF9: R_doDotCall (dotcode.c:757)
==3168==    by 0x499377D: bcEval_loop (eval.c:8668)
==3168==    by 0x49A4842: bcEval (eval.c:7501)
==3168==    by 0x49A4842: bcEval (eval.c:7486)
==3168==    by 0x49A4BFA: Rf_eval (eval.c:1167)
==3168==    by 0x49A6D4E: R_execClosure (eval.c:2393)
==3168==    by 0x49A7AE6: applyClosure_core (eval.c:2306)
==3168==    by 0x49A8548: Rf_applyClosure (eval.c:2328)
==3168==    by 0x49F2F52: dispatchMethod (objects.c:473)
==3168==    by 0x49F3648: Rf_usemethod (objects.c:513)
==3168==    by 0x49F389A: do_usemethod (objects.c:579)
==3168==  Uninitialised value was created by a client request
==3168==    at 0x49EC970: Rf_allocVector3 (memory.c:2980)
==3168==    by 0x2901FC41: random1 (random.c:86)
==3168==    by 0x4957CF9: R_doDotCall (dotcode.c:757)
==3168==    by 0x499377D: bcEval_loop (eval.c:8668)
==3168==    by 0x49A4842: bcEval (eval.c:7501)
==3168==    by 0x49A4842: bcEval (eval.c:7486)
==3168==    by 0x49A4BFA: Rf_eval (eval.c:1167)
==3168==    by 0x49A6D4E: R_execClosure (eval.c:2393)
==3168==    by 0x49A7AE6: applyClosure_core (eval.c:2306)
==3168==    by 0x49A8548: Rf_applyClosure (eval.c:2328)
==3168==    by 0x49F2F52: dispatchMethod (objects.c:473)
==3168==    by 0x49F3648: Rf_usemethod (objects.c:513)
==3168==    by 0x49F389A: do_usemethod (objects.c:579)
==3168== 

The preceding function calls are pure R code with no calls to
TMB. I have not been able to replicate the errors outside rhub,
including running the code on a version of R 4.5.0 built from source
with valgrind instrumentation. It seems that (i) the errors are
outside my control, and (ii) possibly due to some quirk of the 'rhub'
environment (though I would welcome alternative explanations or
suggestions on what to do!)


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
