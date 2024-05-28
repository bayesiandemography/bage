
## WARNING - All these functions will be moved
## out of 'bage' into package 'bssvd'

## HAS_TESTS
#' Create components needed by a ssvd object
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values.
#'
#' `ssvd_comp() typically proceeds as follows:
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
#' When `scale` is `"log"` or `"logit"`,
#' `ssvd_comp()` converts any `0`s in
#' `x` to values just above `0` before
#' applying the log or logit function.
#' When `scale` is `"logit"`,
#' `ssvd_comp() also converts any
#' `1`s to values just below `1`.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param transform `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param n_comp Number of components.
#'
#' @returns A named list with two elements:
#' - `matrix`, a numeric matrix
#' - `offset`, a numeric vector
#'
#' @examples
#' x <- matrix(rgamma(n = 150, shape = 1),
#'             nrow = 10,
#'             ncol = 15)
#' x
#' ssvd_comp(x)
#' ssvd_comp(x, transform = "none")
#' ssvd_comp(x, n_comp = 2)
#' @export
ssvd_comp <- function(x, transform = c("log", "logit", "none"), n_comp = 10) {
  ## check 'n_comp'
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  ## check 'x'
  check_is_matrix(x, nm_x = "x")
  check_numeric(x, nm_x = "x")
  if (nrow(x) < n_comp)
    cli::cli_abort(c("{.code nrow(x)} less than {.arg n_comp}.",
                     i = "{.code nrow(x)}: {.val {nrow(x)}}.",
                     i = "{.arg n_comp}: {.val {n_comp}}."))
  if (ncol(x) < n_comp)
    cli::cli_abort(c("{.code ncol(x)} less than {.arg n_comp}X.",
                     i = "{.code ncol(x)}: {.val {ncol(x)}}.",
                     i = "{.arg n_comp}: {.val {n_comp}}."))
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
    x <- log(x / (1 - x))
  }
  ## apply svd
  svd <- svd(x = x,
             nu = n_comp,
             nv = n_comp)
  U <- svd$u
  D <- diag(svd$d[seq_len(n_comp)])
  V <- svd$v
  ## standardise
  m <- colMeans(V)
  S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
  matrix <- U %*% D %*% S
  offset <- as.numeric(U %*% D %*% m)
  ## add names
  dn <- dimnames(x)
  if (!is.null(dn[[1L]])) {
    dimnames(matrix) <- c(dn[1L], list(component = seq_len(n_comp)))
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


## 'hmd' ----------------------------------------------------------------------

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
hmd_unzip <- function(zipfile) {
  dirs <- c("lt_both/bltper_1x1",
            "lt_both/bltper_5x1",
            "lt_female/fltper_1x1",
            "lt_female/fltper_5x1",
            "lt_male/mltper_1x1",
            "lt_male/mltper_5x1")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  utils::unzip(zipfile, exdir = tmp_dir)
  get_data <- function(dir) {
    country_code <- sub("([A-Z]+)\\..*", "\\1", list.files(dir))
    sex <- sub(".*lt_([a-z]+).*", "\\1", dir)
    type_age <- sub(".*ltper_([1x5]+).*", "\\1", dir)
    files <- list.files(dir, full.names = TRUE)
    ans <- lapply(files,
                  utils::read.table,
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
  base_zipfile <- sub("\\.zip$", "", basename(zipfile))
  if (base_zipfile %in% dir(tmp_dir)) {
    dirs_unpacked <- file.path(tmp_dir, base_zipfile, dirs)
  }
  else if (all(c("lt_both", "lt_female", "lt_male") %in% dir(tmp_dir))) {
    dirs_unpacked <- file.path(tmp_dir, dirs)
  }
  else {
    cli::cli_abort(c("Did not get expected files when unzipped {.arg zipfile}.",
                     i = "Contents of temporary directory after unzipping: {.val {dir(tmp_dir)}}.",
                     i = "Value of {.arg zipfile}: {.file {zipfile}}."))
  }
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
  has_na <- stats::aggregate(x = data.frame(has_na = is.na(data$mx) | is.na(data$Lx)),
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
  is_04 <- five$age %in% c("0", "1-4")
  five_04 <- five[is_04, ]
  five_5pl <- five[!is_04, ]
  five_04$age <- "0-4"
  five_04 <- hmd_aggregate_mx_Lx(five_04)
  five <- vctrs::vec_rbind(five_04, five_5pl)
  vctrs::vec_rbind(data, five)
}


## HAS_TESTS
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
hmd_aggregate_mx_Lx <- function(data) {
  nms <- names(data)
  nms_x <- c("mx", "Lx")
  nms_by <- setdiff(nms, nms_x)
  data <- vctrs::vec_split(x = data[nms_x], by = data[nms_by])
  Lx <- vapply(data$val, function(x) sum(x$Lx), 0)
  mx <- double(length = length(Lx))
  is_pos <- Lx > 0
  mx[is_pos] <- vapply(data$val[is_pos],
                       function(x) stats::weighted.mean(x = x$mx, w = x$Lx), 0)
  mx[!is_pos] <- vapply(data$val[!is_pos], function(x) mean(x$mx), 0)
  vctrs::vec_cbind(data$key, mx = mx, Lx = Lx)
}


## HAS_TESTS
#' Create Multiple Versions of Data, Each with Different
#' Open Age Group
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_vary_age_open <- function(data) {
  data <- vctrs::vec_split(data, data["type_age"])
  ans <- lapply(data$val, hmd_vary_age_open_type_age)
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## HAS_TESTS
#' Create Multiple Versions of Data, Each with Different
#' Open Age Group - For a Single Value of 'type_age'
#'
#' @param x A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_vary_age_open_type_age <- function(x) {
  age_open <- seq.int(from = 60L, to = 110L, by = 5L)
  make_data_age_open <- function(age_open, X) {
    X$age <- as.character(X$age)
    X$age <- poputils::set_age_open(X$age, lower = age_open)
    X <- hmd_aggregate_mx_Lx(X)
    X$age_open <- age_open
    X
  }
  ans <- lapply(age_open, function(a) make_data_age_open(a, X = x))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## HAS_TESTS
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_total <- function(data, n) {
  data <- data[data$sex == "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "country", "time", "mx")],
                           by = data[c("type_age", "age_open")])
  format_age <- function(x) {
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$age), ]
    x
  }
  data$val <- lapply(data$val, format_age)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = "age",
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              ssvd_comp,
              transform = "log")
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  labels_age <- lapply(matrix, rownames)
  tibble::tibble(type = "total",
                 labels_age = labels_age,
                 labels_sexgender = list(NULL),
                 matrix = matrix,
                 offset = offset)
}


## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Modelled Jointly
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_joint <- function(data, n) {
  data <- data[data$sex != "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "sex", "country", "time", "mx")],
                           by = data[c("type_age", "age_open")])
  format_sexage <- function(x) {
    x$sex <- poputils::reformat_sex(x$sex)
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$sex, x$age), ]
    x
  }
  data$val <- lapply(data$val, format_sexage)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = c("sex", "age"),
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              ssvd_comp,
              transform = "log")
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  rn <- lapply(matrix, rownames)
  p_rn <- "^(.*)\\.(.*)$"
  labels_age <- lapply(rn, function(x) sub(p_rn, "\\2", x))
  labels_sexgender <- lapply(rn, function(x) sub(p_rn, "\\1", x))
  tibble::tibble(type = "joint",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
}


## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Modelled Separately
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_indep <- function(data, n) {
  data <- data[data$sex != "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "country", "time", "mx")],
                           by = data[c("type_age", "age_open", "sex")])
  format_age <- function(x) {
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$age), ]
    x
  }
  data$val <- lapply(data$val, format_age)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = "age",
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              ssvd_comp,
              transform = "log")
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  labels_sexgender <- .mapply(rep.int,
                              dots = list(x = as.character(data$key$sex),
                                          times = lengths(offset)),
                              MoreArgs = list())
  labels_age <- lapply(offset, names)
  f <- data$key[c("type_age", "age_open")]
  matrix <- split(matrix, f = f)
  offset <- split(offset, f = f)
  matrix <- lapply(matrix, Matrix::.bdiag)
  concat_list <- function(x) do.call(c, x)
  offset <- lapply(offset, concat_list)
  labels_sexgender <- split(labels_sexgender, f = f)
  labels_age <- split(labels_age, f = f)
  labels_sexgender <- lapply(labels_sexgender, concat_list)
  labels_age <- lapply(labels_age, concat_list)
  for (i in seq_along(offset)) {
    nms <- paste(labels_sexgender[[i]], labels_age[[i]], sep = ".")
    names(offset[[i]]) <- nms
    rownames(matrix[[i]]) <- nms
  }
  ans <- tibble::tibble(type = "indep",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
  ans[-1L] <- lapply(ans[-1L], unname)
  ans
}


## HAS_TESTS
#' Create a Scaled SVD Object from a Human Mortality Database File
#'
#' Create an object of class [`"bage_ssvd"`][ssvd()]
#' from a zipped file downloaded from the
#' [Human Mortality Database](https://www.mortality.org/Data/ZippedDataFiles).
#'
#' @section Warning:
#'
#' This function, an associated functions, will probably
#' be moved into a separate package in future.
#'
#' @param zipfile The name of a zipped file downloaded
#' from the Human Mortality Database.
#' Any path name that can be handled by [utils::unzip()].
#' Defaults to `"log"`.
#'
#' @returns An object of class [`"bage_ssvd"`][ssvd()]
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' Register or log in at the Human Mortality Database, and go to page
#' [Downloading the HMD in zipped data files](https://www.mortality.org/Data/ZippedDataFiles).
#' Go to the "Previous Versions" table at the bottom of the page, and
#' download a file from the "Statistics" column, eg file
#'
#' https://www.mortality.org/File/Download/hmd.v6/zip/all_hmd/hmd_statistics_20240226.zip
#'
#' To download a file, you will need to register with the Human Mortality Databse
#' and log in.
#'
#' **Step 2: Call function 'svd_hmd'**
#'
#' Put the file into the working directory, and supply the name of the file
#' to function `svd_hmd()`, eg
#' ```
#' MyHMD <- svd_hmd("hmd_statistcs_20240226")
#' ```
#'
#' **Step 3: Use object to create a prior**
#' 
#' ```
#' mod |>
#'   set_prior(age ~ SVD(MyHMD))
#' ```
#' @seealso
#' - [SVD()], [SVDS()]
#'   to use an object of class 
#'   [`"bage_ssvd"`][ssvd()] in a prior.
#' @export
ssvd_hmd <- function(zipfile) {
  cli::cli_progress_message("Unzipping file...")
  data <- hmd_unzip(zipfile)
  cli::cli_progress_message("Tidying data...")
  data <- hmd_tidy_data(data)
  cli::cli_progress_message("Creating five-year age groups...")
  data <- hmd_add_age_five(data)
  cli::cli_progress_message("Assembling datasets for alternative open age groups...")
  data <- hmd_vary_age_open(data)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  total <- hmd_total(data)
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- hmd_indep(data)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- hmd_joint(data)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(total, indep, joint)
  data <- tibble::as_tibble(data)
  ssvd(data)
}  
