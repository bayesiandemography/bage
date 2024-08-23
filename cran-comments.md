
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
