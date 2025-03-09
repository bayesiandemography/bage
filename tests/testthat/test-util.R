
## 'make_str_key' -------------------------------------------------------------

test_that("'make_str_key' works with valid input", {
    row <- data.frame(a = 1, b = "x")
    row$c <- list(1:3)
    row$d <- rvec::rvec(matrix(1:3, 1))
    ans <- make_str_key(row)
    expect_true(is.character(ans))
    expect_identical(length(ans), 1L)
})
    
    
