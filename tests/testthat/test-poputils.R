
## 'find_age' -----------------------------------------------------------------

test_that("'find_age' returns age with valid input", {
    expect_identical(find_age(c("age", "x", "agex", NA)),
                     "age")
    expect_identical(find_age(c(22, "Age.Years", "x", "agex", NA)),
                     "Age.Years")
    expect_identical(find_age("age_group"),
                     "age_group")
})

test_that("'find_age' returns NULL with no valid values", {
    expect_identical(find_age(c("sage", "x", "agex", NA)),
                     NULL)
    expect_identical(find_age(c(22, "Age.Days", "x", "agex", NA)),
                     NULL)
})

test_that("'find_age' returns NULL with two or more valid values", {
    expect_identical(find_age(c("age", "x", "ages", "age.group", NA)),
                     NULL)
})


## 'find_female' -----------------------------------------------------------------

test_that("'find_female' returns female with valid input", {
    expect_identical(find_female(c("female", "x", "male", NA)),
                     "female")
    expect_identical(find_female(c(22, "F", "M", "agex", NA)),
                     "F")
    expect_identical(find_female("WOMEN"),
                     "WOMEN")
})

test_that("'find_female' returns NULL with no valid values", {
    expect_identical(find_female(c("ff", "x", "mm", NA)),
                     NULL)
})

test_that("'find_female' returns NULL with two or more valid values", {
    expect_identical(find_female(c("f", "fem")),
                     NULL)
})


## 'find_male' -----------------------------------------------------------------

test_that("'find_male' returns male with valid input", {
    expect_identical(find_male(c("female", "x", "male", NA)),
                     "male")
    expect_identical(find_male(c(22, "F", "M", "agex", NA)),
                     "M")
    expect_identical(find_male("MEN"),
                     "MEN")
})

test_that("'find_male' returns NULL with no valid values", {
    expect_identical(find_male(c("ff", "x", "mm", NA)),
                     NULL)
})

test_that("'find_male' returns NULL with two or more valid values", {
    expect_identical(find_male(c("m", "male")),
                     NULL)
})


## 'find_sexgender' ----------------------------------------------------------------

test_that("'find_sexgender' returns sexgender with valid input", {
    expect_identical(find_sexgender(c("sexgender", "x", "sexgenderx", NA)),
                     "sexgender")
    expect_identical(find_sexgender(c(22, "gender", "x", "sexgenderx", NA)),
                     "gender")
    expect_identical(find_sexgender("Sexes"),
                     "Sexes")
})

test_that("'find_sexgender' returns NULL with no valid values", {
    expect_identical(find_sexgender(c("ssexgender", "x", "xxx", NA)),
                     NULL)
    expect_identical(find_sexgender(c(22, "Time", "x", "sexgenderx", NA)),
                     NULL)
})

test_that("'find_sexgender' returns NULL with two or more valid values", {
    expect_identical(find_sexgender(c("sexgender", "x", "sexes", "month__year", NA)),
                     NULL)
})


## 'find_time' ----------------------------------------------------------------

test_that("'find_time' returns time with valid input", {
    expect_identical(find_time(c("time", "x", "timex", NA)),
                     "time")
    expect_identical(find_time(c(22, "quarters", "x", "timex", NA)),
                     "quarters")
    expect_identical(find_time("month_year"),
                     "month_year")
})

test_that("'find_time' returns NULL with no valid values", {
    expect_identical(find_time(c("stime", "x", "timex", NA)),
                     NULL)
    expect_identical(find_time(c(22, "Time.Days", "x", "timex", NA)),
                     NULL)
})

test_that("'find_time' returns NULL with two or more valid values", {
    expect_identical(find_time(c("time", "x", "times", "month__year", NA)),
                     NULL)
})


## 'find_inner' ----------------------------------------------------------------

test_that("'find_inner' returns element of 'nms' with valid input", {
    expect_identical(find_inner(c("time", "x", "blaa", NA),
                                p_valid = "time"),
                     "time")
})

test_that("'find_inner' returns NULL with no valid values", {
    expect_identical(find_inner(c("timey", "x", "timex", NA),
                                p_valid = "time"),
                     NULL)
})

test_that("'find_inner' returns NULL with two valid values", {
    expect_identical(find_inner(c("time", "x", "time", NA),
                                p_valid = "time"),
                     NULL)
})


## 'to_matrix' ----------------------------------------------------------------

test_that("'to_matrix' works with valid inputs", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    ans <- to_matrix(df, rows = a, cols = c(c, b), measure = z)
    expect_true(is.matrix(ans))
    ans_quoted <- to_matrix(df, measure = z, rows = "a", cols = c("c", "b"))
    expect_identical(ans, ans_quoted)
    ans_t <- to_matrix(df, measure = z, rows = c(c, b), cols = a)
    expect_identical(ans, t(ans_t))
})


test_that("'to_matrix' works with missing levels inputs", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df <- df[-(58:60), ]
    df$a <- factor(df$a, levels = letters[1:4])
    df$z <- seq_len(nrow(df))
    ans <- to_matrix(df, rows = a, cols = c(c, b), measure = z)
    expect_identical(nrow(ans), 3L)
    expect_identical(ncol(ans), nrow(unique(df[c("b", "c")])))
    expect_identical(sum(is.na(ans)), 0L)
    expect_true(is.matrix(ans))
    ans_quoted <- to_matrix(df, measure = z, rows = "a", cols = c("c", "b"))
    expect_identical(ans, ans_quoted)
    ans_t <- to_matrix(df, measure = z, rows = c(c, b), cols = a)
    expect_identical(ans, t(ans_t))
})

test_that("'to_matrix' raises correct error with no measure var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b)),
                 "no measure variable supplied")
})

test_that("'to_matrix' raises correct error with two measure vars", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    df$z2 <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = c(z, z2)),
                 "attempt to select 2 measure variables")
})

test_that("'to_matrix' raises correct error with no rows var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, cols = c(c, b), measure = z),
                 "no value supplied for 'rows'")
})

test_that("'to_matrix' raises correct error with measure and rows overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = a),
                 "same variable \\['a'\\] selected by 'measure' and 'rows'")
})

test_that("'to_matrix' raises correct error with no cols var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = c(c, b), measure = z),
                 "no value supplied for 'cols'")
})

test_that("'to_matrix' raises correct error with measure and cols overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = c),
                 "same variable \\['c'\\] selected by 'measure' and 'cols'")
})

test_that("'to_matrix' raises correct error when rows and cols overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b, a), measure = z),
                 "same variable \\['a'\\] selected by 'rows' and 'cols'")
})

test_that("'to_matrix' raises correct error with duplicate classif vars", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    df <- rbind(df, df[1, ])
    expect_error(to_matrix(df, rows = a, cols = c(b, c), measure = z),
                 "'x' has two rows with values c\\(a=\"a\", b=\"A\", c=\"a\"\\)")
})


