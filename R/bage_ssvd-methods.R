
## 'generate

#' @importFrom generics generate
#' @export
generics::generate

#' Generate Random Age or Age-Sex Profiles
#'
#' Generate random age or age-sex profiles from
#' an object of class `"bage_ssvd"`. An object
#' of class `"bage_ssvd"` holds results from
#' an [SVD][base::svd()] decomposition of demographic
#' data.
#'
#' @inheritSection SVD Scaled SVDs currently implemented in bage
#'
#' @param x An object of class `"bage_ssvd"`.
#' @param n_draw Number of random draws to generate.
#' @param n_comp The number of components
#' to use when generating profiles. Default is
#' 5. Maximum value is 10.
#' @param joint Whether to use joint or
#' independent SVDs for each sex/gender. If
#' no value is supplied, an SVD for all sexes/genders
#' combined is used.
#' @param age_labels Age labels for the
#' desired profile. If no labels are supplied, the
#' most detailed profile available is used. 
#' @param ... Not currently used.
#'
#' @returns A tibble
#'
#' @seealso
#' - [SVD()] SVD prior for age main effect.
#' - [SVDS()] SVD prior for interaction between
#'   age and sex or gender.
#' - [poputils::age_labels()] Generate age labels.
#'
#' @examples
#' ## SVD for females and males combined
#' generate(HMD)
#'
#' ## separate SVDs for females and males
#' generate(HMD, joint = FALSE) 
#'
#' ## specify age groups
#' labels <- poputils::age_labels(type = "lt", max = 60)
#' generate(HMD, age_labels = labels)
#' @export
generate.bage_ssvd <- function(x,
                               n_draw = 20,
                               n_comp = 5,
                               joint = NULL,
                               age_labels = NULL,
                               ...) {
  check_n(n = n_draw,
          nm_n = "n_draw",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  has_joint <- !is.null(joint)
  if (has_joint) {
    check_flag(joint)
    type <- if (joint) "joint" else "indep"
  }
  else
    type <- "total"
  has_age <- !is.null(age_labels)
  if (has_age) {
    age_labels <- tryCatch(poputils::reformat_age(age_labels, factor = FALSE),
                           error = function(e) e)
    if (inherits(age_labels, "error"))
      cli::cli_abort(c("Problem with {.arg age_labels}.",
                       i = age_labels$message))
  }
  data <- x$data
  data <- data[data$type == type, , drop = FALSE]
  if (has_age) {
    is_matched <- vapply(data$labels_age, setequal, TRUE, y = age_labels)
    if (!any(is_matched))
      cli::cli_abort("Can't find labels from {.arg age_labels} in {.arg x}.")
    i_matched <- which(is_matched)
  }
  else {
    lengths_labels <- lengths(data$labels_age)
    i_matched <- which.max(lengths_labels)
  }
  levels_age <- data$labels_age[[i_matched]]
  levels_sexgender <- data$labels_sexgender[[i_matched]]
  levels_age <- unique(levels_age)
  levels_sexgender <- unique(levels_sexgender)
  agesex <- if (has_joint) "age:sex" else "age"
  matrix <- get_matrix_or_offset_svd(ssvd = x,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = joint,
                                     agesex = agesex,
                                     get_matrix = TRUE,
                                     n_comp = n_comp)
  offset <- get_matrix_or_offset_svd(ssvd = x,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = joint,
                                     agesex = agesex,
                                     get_matrix = FALSE,
                                     n_comp = n_comp)
  n_Z <- if (type == "indep") 2L * n_comp else n_comp
  Z <- stats::rnorm(n = n_Z * n_draw)
  Z <- matrix(Z, nrow = n_Z, ncol = n_draw)
  value <- matrix %*% Z + offset
  n_age <- length(levels_age)
  if (has_joint) {
    n_sex <- length(levels_sexgender)
    ans <- tibble::tibble(draw = rep(seq_len(n_draw), each = n_age * n_sex),
                          sexgender = rep(rep(levels_sexgender, each = n_age), times = n_draw),
                          age = rep(levels_age, times = n_sex * n_draw))
  }
  else {
    ans <- tibble::tibble(draw = rep(seq_len(n_draw), each = n_age),
                          age = rep(levels_age, times = n_draw))
  }
  ans[["value"]] <- as.double(value)
  ans
}


## 'print' --------------------------------------------------------------------

#' @export
print.bage_ssvd <- function(x, ...) {
    cat("<Object of class \"", class(x), "\">\n", sep = "")
    invisible(x)
}
