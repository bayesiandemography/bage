## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* There are currently no references describing the methods in the
  package. We plan to write papers on the package eventually.
* When using 'win_builder_release', all unit tests pass. When using
  'win_builder_devel', however, all unit tests that involve calls to TMB
  fail. This appears to be due to an incompatibility between the TMB and
  Matrix packages. 'win_builder_release' and 'win_builder_devel' both
  use TMB v1.9.14. However, 'win_builder_release' uses Matrix v1.7.0,
  while 'win_builder_devel' uses Matrix v1.8.0. Similar problems with
  incompatible versions of TMB and Matrix are
  discussed, for instance, here:
  https://stat.ethz.ch/pipermail/r-package-devel/2024q1/010590.html
  I'm not aware of any way of avoiding these problems when running
  checks on, for instance, win_builder. Hopefully the problems are only
  transitory, and will disappear once TMB and Matrix versions come
  back into alignment.
  
