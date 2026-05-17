# Test Structure

The `tests` directory contains four types of tests.

## `tests/testthat`

These are the standard package tests run by:

- `devtools::test()`
- `R CMD check`
- the ordinary CI workflow

These tests are intended to provide routine package QA.

## `tests/simulation`

These are longer-running simulation-based tests used for statistical validation.

These tests are *not* run automatically by `R CMD check` or ordinary CI.

Run manually with:

```R
testthat::test_dir("tests/simulation")
```


## `tests/plot`

These are plots of model outputs. The aim is to detect any sort of anomolous behaviour.

The plots should be created and manually insepected by a human from time to time.



## `tests/valgrind`

These are small targeted tests intended for memory debugging under Valgrind.

The tests in this directory should:

- use very small datasets
- run quickly
- exercise important C++/TMB code paths
- avoid large simulations
- avoid long optimization loops where possible

Typical uses include:

- checking for memory leaks
- checking for invalid reads/writes
- checking behaviour of `MakeADFun()`
- checking optimization and `sdreport()`
- checking edge-case inputs

These tests are not run by ordinary `R CMD check`.

On macOS, run these tests remotely using the manual GitHub Actions workflow:

1. Commit and push the changes to GitHub.
2. Go to the package repository on GitHub.
3. Open the **Actions** tab.
4. Select the **valgrind** workflow.
5. Click **Run workflow**.
6. Choose the branch to test.
7. Click **Run workflow** again.
8. Inspect the workflow logs for Valgrind errors.

The workflow runs the tests in `tests/valgrind` on Ubuntu, where Valgrind is available through the system package manager.