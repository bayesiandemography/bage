
## 'bage_confidential_rr3' -------------------------------------------------

test_that("'new_bage_confidential_rr3' works", {
  x <- new_bage_confidential_rr3()
  expect_s3_class(x, "bage_confidential_rr3")
  expect_s3_class(x, "bage_confidential")
  expect_identical(x$nm, "rr3")
})
  
