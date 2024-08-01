## 'components' ---------------------------------------------------------------

#' Extract Components used by SVD Summary
#'
#' Extract the matrix and offset used by a scaled SVD
#' summary of a demographic database.
#'
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @param object An object of class `"bage_ssvd"`.
#' @param n_comp The number of components.
#' The default is half the total number of
#' components of `object`.
#' @param indep Whether to use independent or
#' joint SVDs for each sex/gender. If
#' no value is supplied, an SVD with no
#' sex/gender dimension is used. Note that the
#' default is different from [SVD()].
#' @param age_labels Age labels for the
#' desired age or age-sex profile.
#' If no labels are supplied, the
#' most detailed profile available is used. 
#' @param ... Not currently used.
#'
#' @returns A tibble with the offset and components.
#'
#' @seealso
#' - [generate()][bage::generate()] Randomly generate
#'   age-profiles, or age-sex profiles, based on a
#'   scaled SVD summary.
#' - [SVD()] SVD prior for terms involving age.
#' - [SVD_AR1()], [SVD_AR()], [SVD_RW()], [SVD_RW2()]
#'   Dynamic SVD priors for terms involving age and time.
#' - [poputils::age_labels()] Generate age labels.
#'
#' @examples
#' ## females and males combined
#' components(LFP, n_comp = 3)
#'
#' ## females and males modelled independently
#' components(LFP, indep = TRUE, n_comp = 3)
#' 
#' ## joint model for females and males
#' components(LFP, indep = FALSE, n_comp = 3)
#'
#' ## specify age groups
#' labels <- poputils::age_labels(type = "five", min = 15, max = 60)
#' components(LFP, age_labels = labels)
#' @export
components.bage_ssvd <- function(object,
                                 n_comp = NULL,
                                 indep = NULL,
                                 age_labels = NULL,
                                 ...) {
  n_comp_obj <- get_n_comp(object)
  if (is.null(n_comp))
    n_comp <- ceiling(n_comp_obj / 2)
  else {
    check_n(n = n_comp,
            nm_n = "n_comp",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    if (n_comp > n_comp_obj)
      cli::cli_abort(c("{.arg n_comp} larger than number of components of {.arg object}.",
                       i = "{.arg n_comp}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_obj}}."))
  }
  has_indep <- !is.null(indep)
  if (has_indep) {
    check_flag(x = indep, nm_x = "indep")
    if (!has_sexgender(object))
      cli::cli_abort(paste("Value supplied for {.arg indep}, but {.arg object}",
                           "does not have a sex/gender dimension."))
    type <- if (indep) "indep" else "joint"
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
  data <- object$data
  data <- data[data$type == type, ]
  if (has_age) {
    is_matched <- vapply(data$labels_age, setequal, TRUE, y = age_labels)
    if (!any(is_matched))
      cli::cli_abort("Can't find labels from {.arg age_labels} in {.arg object}.")
    i_matched <- which(is_matched)
  }
  else {
    lengths_labels <- lengths(data$labels_age)
    i_matched <- which.max(lengths_labels)
  }
  labels_age <- data$labels_age[[i_matched]]
  labels_sexgender <- data$labels_sexgender[[i_matched]]
  levels_age <- unique(labels_age)
  levels_sexgender <- unique(labels_sexgender)
  agesex <- if (has_indep) "age:sex" else "age"
  matrix <- get_matrix_or_offset_svd(ssvd = object,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = TRUE,
                                     n_comp = n_comp)
  offset <- get_matrix_or_offset_svd(ssvd = object,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = FALSE,
                                     n_comp = n_comp)
  n_age <- length(levels_age)
  n_sex <- length(unique(levels_sexgender))
  keep <- seq_len(n_comp)
  matrix <- Matrix::as.matrix(matrix)
  if (type == "indep") {
    matrix <- lapply(seq_len(n_sex),
                     function(i)
                       matrix[seq_len(n_age) + (i - 1L) * n_age,
                              seq_len(n_comp) + (i - 1L) * n_comp,
                              drop = FALSE])
    matrix <- do.call(rbind, matrix)
  }
  offset <- tibble::tibble(component = "Offset",
                           sex = labels_sexgender,
                           age = labels_age,
                           value = as.numeric(offset))
  component <- paste("Component", rep(seq_len(n_comp), each = length(labels_age)))
  components <- tibble::tibble(component = component,
                               sex = rep(labels_sexgender, times = n_comp),
                               age = rep(labels_age, times = n_comp),
                               value = as.numeric(matrix))
  ans <- rbind(offset, components)
  ans$component <- factor(ans$component, levels = unique(ans$component))
  ans
}


## 'generate' -----------------------------------------------------------------

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
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @inheritParams components.bage_ssvd
#' @param x An object of class `"bage_ssvd"`.
#' @param n_draw Number of random draws to generate.
#'
#' @returns A tibble
#'
#' @seealso
#' - [components()][bage::components()] Components
#'   used by SVD prior.
#' - [SVD()] SVD prior for term involving age.
#' - [SVD_AR1()], [SVD_AR()], [SVD_RW()], [SVD_RW2()]
#'   Dynamic SVD priors for terms involving age and time.
#' - [poputils::age_labels()] Generate age labels.
#'
#' @examples
#' ## SVD for females and males combined
#' generate(HMD)
#'
#' ## separate SVDs for females and males
#' generate(HMD, indep = TRUE) 
#'
#' ## specify age groups
#' labels <- poputils::age_labels(type = "lt", max = 60)
#' generate(HMD, age_labels = labels)
#' @export
generate.bage_ssvd <- function(x,
                               n_draw = 20,
                               n_comp = NULL,
                               indep = NULL,
                               age_labels = NULL,
                               ...) {
  check_n(n = n_draw,
          nm_n = "n_draw",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  n_comp_x <- get_n_comp(x)
  if (is.null(n_comp))
    n_comp <- ceiling(n_comp_x / 2)
  else {
    check_n(n = n_comp,
            nm_n = "n_comp",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    if (n_comp > n_comp_x)
      cli::cli_abort(c("{.arg n_comp} larger than number of components of {.arg x}.",
                       i = "{.arg n_comp}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_x}}."))
  }
  n_comp <- as.integer(n_comp)
  has_indep <- !is.null(indep)
  if (has_indep) {
    check_flag(x = indep, nm_x = "indep")
    if (!has_sexgender(x))
      cli::cli_abort(paste("Value supplied for {.arg indep}, but {.arg x}",
                           "does not have a sex/gender dimension."))
    type <- if (indep) "indep" else "joint"
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
  n_sexgender <- length(levels_sexgender)
  agesex <- if (has_indep) "age:sex" else "age"
  matrix <- get_matrix_or_offset_svd(ssvd = x,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = TRUE,
                                     n_comp = n_comp)
  offset <- get_matrix_or_offset_svd(ssvd = x,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = !indep,
                                     agesex = agesex,
                                     get_matrix = FALSE,
                                     n_comp = n_comp)
  n_Z <- if (type == "indep") n_sexgender * n_comp else n_comp
  Z <- stats::rnorm(n = n_Z * n_draw)
  Z <- matrix(Z, nrow = n_Z, ncol = n_draw)
  value <- matrix %*% Z + offset
  n_age <- length(levels_age)
  if (has_indep) {
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
  cat("\n")
  cat(paste(rep("-", times = 60), collapse = ""), "\n")
  cat("<Object of class \"", class(x), "\">\n\n", sep = "")
  data <- x$data
  type <- data$type
  sexgender <- data$labels_sexgender
  age <- data$labels_age
  has_sexgender <- "joint" %in% type
  if (has_sexgender) {
    levels_sexgender <- unique(sexgender[type == "joint"][[1L]])
    cat("sex/gender labels:\n")
    for (level in levels_sexgender)
      cat("    ", level, "\n")
    cat("\n")
  }
  cat("age labels:\n")
  labels_age <- age[data$type == "total"]
  for (labels in labels_age) {
    n <- length(labels)
    if (n >= 9L)
      labels <- c(labels[1:4], "...", labels[c(n - 2L, n - 1L, n)])
    labels <- paste(labels, collapse = ", ")
    cat("    ", labels, "\n")
  }
  cat(paste(rep("-", times = 60), collapse = ""), "\n")
}

