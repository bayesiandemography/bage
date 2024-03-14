
## WARNING - All these functions will be moved
## out of 'bage' into their own package

## HAS_TESTS
#' Create components needed by a scaled_svd object
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values.
#'
#' `scaled_svd_comp() typically proceeds as follows:
#' - transform values in matrix `x` (eg take logs)
#' - carry out a SVD on the transformed version of `x`
#' - centre and scale the results from the SVD to produce
#' matrix `matrix` and vector `offset`.
#'
#' If 
#' - \eqn{X} is a the matrix of transformed values
#' of `x`
#' - \eqn{F} is the matrix called `matrix`,
#' - \eqn{g} is the vector called `offset`, and
#' - \eqn{\alpha} is a vector of standard normal variates,
#'
#' and
#'
#' \deqn{\beta = F \alpha + g}
#'
#' then \eqn{\beta} should look like a randomly-selected
#' column from \eqn{X}.
#'
#' Matrix `x` typically has age along the rows,
#' and some combination of classification variables,
#' such as country and time, along the columns.
#' One exception is when the SVD is used to capture
#' the relationship between female and male rates,
#' in which case rows are formed by interacting
#' sex and age. See below for an example.
#'
#' The number of components used by the SVD
#' (and hence the number of columns of the
#' `tranform` matrix) is governed by
#' argument `n`. The default is 10.
#'
#' When `scale` is `"log"` or `"logit"`,
#' `scaled_svd_comp()` converts any `0`s in
#' `x` to values just above `0` before
#' applying the log or logit function.
#' When `scale` is `"logit"`,
#' `scaled_svd_comp() also converts any
#' `1`s to values just below `1`.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param n Number of components of the SVD to use.
#' Defaults to 10.
#' @param transform `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#'
#' @returns A named list with two elements:
#' - `matrix`, a numeric matrix
#' - `offset`, a numeric vector
#'
#' @examples
#' x <- matrix(rgamma(n = 50, shape = 1), nrow = 5, ncol = 10)
#' x
#' scaled_svd_comp(x, n = 3)
#' scaled_svd_comp(x, n = 3, transform = "none")
#' @noRd
scaled_svd_comp <- function(x,
                            n = 10,
                            transform = c("log", "logit", "none")) {
  ## check 'n'
  check_n(n = n,
          nm_n = "n",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  n <- as.integer(n)
  ## check 'x'
  check_is_matrix(x, nm_x = "x")
  check_numeric(x, nm_x = "x")
  if (ncol(x) < n)
    cli::cli_abort(c("{.arg n} less than number of columns of {.arg x}",
                     i = "n: {.val {n}}.",
                     i = "ncol(x): {.val {ncol(x)}}."))
  ## check 'transform'
  transform <- match.arg(transform)
  if (transform %in% c("log", "logit")) {
    n_neg <- sum(x < 0)
    if (n_neg > 0)
      cli::cli_abort(paste("{.arg transform} is {.val {transform}} but {.arg x} has",
                           "{n_neg} negative value{?s}."))
  }
  if (transform  == "logit") {
    n_gt_1 <- sum(x > 1)
    if (n_gt_1 > 0L)
      cli::cli_abort(paste("{.arg transform} is {.val {transform}} but {.arg x} has",
                           "{n_gt_1} value{?s} greater than 1."))
  }
  ## transform to log or logit scale if necessary
  if (transform == "log") {
    x <- replace_zeros(x)
    x <- log(x)
  }
  if (transform == "logit") {
    x <- replace_zeros_ones(x)
    x <- log(1 / (1 - x))
  }
  ## apply svd
  svd <- svd(x = x,
             nu = n,
             nv = n)
  U <- svd$u
  D <- diag(svd$d[seq_len(n)])
  V <- svd$v
  ## standardise
  m <- colMeans(V)
  S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
  matrix <- U %*% D %*% S
  offset <- as.numeric(U %*% D %*% m)
  ## add names
  dn <- dimnames(x)
  if (!is.null(dn[[1L]])) {
    dimnames(matrix) <- c(dn[1L], list(component = seq_len(n)))
    names(offset) <- dn[[1L]]
  }
  ## convert matrix to sparse matrix
  matrix <- Matrix::sparseMatrix(i = row(matrix),
                                 j = col(matrix),
                                 x = as.double(matrix),
                                 dimnames = dimnames(matrix))
  ## return
  list(matrix = matrix,
       offset = offset)
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Replace zeros in a matrix of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace zeros in `x`, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that `x` is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no infinite values.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros <- function(x) {
    is_zero <- x == 0
    if (any(is_zero)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
    }
    x
}


## HAS_TESTS
#' Replace zeros and ones in a matrix 'x' of
#' estimated probabilities
#'
#' Based on a simple main effects model,
#' replace zeros and ones in \code{x},
#' a matrix of estimated probabilities.
#' The replacement values should typically be
#' near 0 or 1.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no values abouve one.
#'
#' @param x A matrix of probabilities.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros_ones <- function(x) {
    is_zero <- x == 0
    is_one <- x == 1
    if (any(is_zero) || any(is_one)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
        x[is_one] <- 0.5 + 0.5 * predicted[is_one]
    }
    x
}


## HAS_TESTS
#' Convert a Zipped HMD into a Combined Data Frame
#'
#' Zipped HMD file obtained from
#' https://www.mortality.org/Data/ZippedDataFiles
#' 
#' @param file Name of zipped HMD file
#'
#' @returns A tibble
#'
#' @noRd
hmd_unzip <- function(file) {
  dirs <- c("lt_both/bltper_1x1",
            "lt_both/bltper_5x1",
            "lt_female/fltper_1x1",
            "lt_female/fltper_5x1",
            "lt_male/mltper_1x1",
            "lt_male/mltper_5x1")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  utils::unzip(file, exdir = tmp_dir)
  get_data <- function(dir) {
    country_code <- sub("([A-Z]+)\\..*", "\\1", list.files(dir))
    sex <- sub(".*lt_([a-z]+).*", "\\1", dir)
    type_age <- sub(".*ltper_([1x5]+).*", "\\1", dir)
    files <- list.files(dir, full.names = TRUE)
    ans <- lapply(files,
                  read.table,
                  header = TRUE,
                  colClasses = c("integer",
                                 "character",
                                 "double",
                                 rep("NULL", 4L),
                                 "integer",
                                 rep("NULL", 2L)),
                  na.strings = ".",
                  skip = 1)
    add_nm <- function(x, nm) {
      x[["country"]] <- nm
      x <- x[c(length(x), seq_len(length(x) - 1L))]
      x
    }
    ans <- .mapply(add_nm,
                   dots = list(x = ans,
                               nm = country_code),
                   MoreArgs = list())
    ans <- do.call(rbind, ans)
    ans$sex <- sex
    ans$type_age <- type_age
    ans
  }
  dirs_unpacked <- file.path(tmp_dir,
                             sub("\\.zip$", "", basename(file)),
                             dirs)
  ans <- lapply(dirs_unpacked, get_data)
  ans <- do.call(rbind, ans)
  ans
}


## HAS_TESTS
#' Tidy Raw HMD data
#'
#' Steps:
#' - rename "Age" to "age"
#' - rename "Year" to "time"
#' - change levels for 'sex' variable to "Total", "Female", "Male"
#' - exclude any country-time combinations that have
#'   NAs for 'mx' or 'Lx'
#'
#' @param data A data fame with raw HMD data
#'
#' @returns A cleaned data frame
#'
#' @noRd
hmd_tidy_data <- function(data) {
  names(data)[[match("Age", names(data))]] <- "age"
  names(data)[[match("Year", names(data))]] <- "time"
  data$sex <- factor(data$sex,
                     levels = c("both", "female", "male"),
                     labels = c("Total", "Female", "Male"))
  data$type_age <- factor(data$type_age,
                          levels = c("1x1", "five", "5x1"), ## "5x1" splits 0-4
                          labels = c("single", "five", "lt"))
  has_na <- aggregate(x = data.frame(has_na = is.na(data$mx) | is.na(data$Lx)),
                      by = data[c("country", "time", "type_age")],
                      FUN = any)
  data <- merge(x = data,
                y = has_na,
                by = c("country", "time", "type_age"),
                sort = FALSE)
  data <- data[!data$has_na, ]
  data <- data[-match("has_na", names(data))]
  data
}



#' Create Age Group "five" and Add to 'data'
#'
#' Create age group "five" by combining
#' age groups 0 and 1-4 in "lt"
#'
#' @param data A data frame
#'
#' @returns Modifed version of 'data'
#'
#' @noRd
hmd_add_age_five <- function(data) {
  five <- data[data$type_age == "lt", ]
  five$type_age <- "five"
  five_04 <- five[five$age %in% c("0", "1-4"), ]
  five_04 <- aggregate_mx_Lx(five_04)
  five_04$dx <- five_04$mx * five_04$Lx
  five_04 <- aggregate(five_04[c("Lx", "dx")],
                       five[c("sex", "country", "time", "type_age")]
                       sum)
  five_04$mx <- five_04$dx / five_04$Lx
  five_04 <- five_04[-match("dx", names(five_04))]
  five_04$age <- "0-4"
  five_5pl <- five[!(five$age %in% c("0", "1-4")), ]
  five <- rbind(five_04, five_5pl)
  data <- rbind(data, five)
}


#' Take Weighted Mean of 'mx' and Add Up 'Lx'
#'
#' Aggregation happens within categories
#' defined by non-mx, non-Lx variables.
#'
#' Case where Lx all 0 or all NA dealt with.
#'
#' @param data A data frame that includes
#' columns 'mx' and 'Lx'
#'
#' @returns A data frame
#'
#' @noRd
aggregate_mx_Lx <- function(data) {
  nms <- names(data)
  nms_x <- c("mx", "Lx")
  nms_by <- setdiff(nms, c("mx", "Lx"))
  data <- vctrs::vec_split(x = data[nms_x], by = data[nms_f])
  fill_Lx <- function(x) {
    if (sum(x$Lx, na.rm = TRUE) == 0)
      x$Lx <- 1
    x
  }
  val <- lapply(data$val, fill_Lx)
  sum_or_mean <- function(x)
    tibble::tibble(mx = weighted.mean(x = x$mx, w = x$Lx),
                   Lx = sum(x$Lx))
  val <- lapply(val, sum_or_mean)
  val <- vctrs::vec_rbind(!!!val, .name_repair = "universal_quiet")
  vctrs::vec_rbind(data$key, val)
}

  
  
