
## 'print' --------------------------------------------------------------------

test_that("'print' works with mod_pois", {
  set.seed(0)
  ssvd <- sim_ssvd()
  expect_snapshot(print(ssvd))
})
