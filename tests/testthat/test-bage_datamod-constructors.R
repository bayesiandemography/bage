
## 'bage_datamod_outcome_rr3' -------------------------------------------------

test_that("'new_bage_datamod_outcome_rr3' works", {
  x <- new_bage_datamod_outcome_rr3()
  expect_s3_class(x, "bage_datamod_outcome_rr3")
  expect_s3_class(x, "bage_datamod_outcome")
  expect_identical(x$nm, "rr3")
})
  
