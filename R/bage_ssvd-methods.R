## 'components' ---------------------------------------------------------------

#' Extract Components used by SVD Summary
#'
#' Extract the matrix and offset used by a scaled SVD
#' summary of a demographic database.
#'
#' @inheritSection SVD Scaled SVDs of demographic databases in bage
#'
#' @param object An object of class `"bage_ssvd"`.
#' @param v Version of scaled SVD components
#' to use. If no value is suppled, the most
#' recent version is used.
#' @param n_comp The number of components.
#' The default is half the total number of
#' components of `object`.
#' @param indep Whether to use independent or
#' joint SVDs for each sex/gender, if the
#' data contains a sex/gender variable.
#' The default is to use independent SVDs.
#' To obtain results for the total population
#' when the data contains a sex/gender variable,
#' set `indep` to `NA`.
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
#' ## females and males modeled independently
#' components(LFP, n_comp = 3)
#'
#' ## joint model for females and males
#' components(LFP, indep = FALSE, n_comp = 3)
#'
#' ## females and males combined
#' components(LFP, indep = NA, n_comp = 3)
#'
#' ## specify age groups
#' labels <- poputils::age_labels(type = "five", min = 15, max = 60)
#' components(LFP, age_labels = labels)
#' @export
components.bage_ssvd <- function(object,
                                 v = NULL,
                                 n_comp = NULL,
                                 indep = NULL,
                                 age_labels = NULL,
                                 ...) {
  nm_ssvd <- deparse1(substitute(object))
  data <- object$data
  version <- data$version
  versions <- unique(version)
  n_comp_obj <- get_n_comp(object)
  if (is.null(n_comp))
    n_comp <- ceiling(n_comp_obj / 2)
  else {
    poputils::check_n(n = n_comp,
                      nm_n = "n_comp",
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    if (n_comp > n_comp_obj)
      cli::cli_abort(c("{.arg n_comp} larger than number of components of {.arg object}.",
                       i = "{.arg n_comp}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_obj}}."))
  }
  has_indep_arg <- !is.null(indep)
  if (has_indep_arg) {
    if (identical(length(indep), 1L) && is.na(indep)) {
      has_total <- "total" %in% data$type
      if (has_total)
        type <- "total"
      else
        cli::cli_abort(paste("{.arg indep} is {.val {NA}} but {.arg object}",
                             "does not have results for total population."))
    }
    else {
      check_flag(x = indep, nm_x = "indep")
      if (!has_sexgender(object))
        cli::cli_abort(paste("Value supplied for {.arg indep}, but",
                             "{.arg object} does not have a sex/gender",
                             "dimension."))
      type <- if (indep) "indep" else "joint"
    }
  }
  else {
    has_indep <- "indep" %in% data$type
    if (has_indep)
      type <- "indep"
    else
      type <- "total"
  }
  has_age <- !is.null(age_labels)
  if (has_age) {
    age_labels <- tryCatch(poputils::reformat_age(age_labels, factor = FALSE),
                           error = function(e) e)
    if (inherits(age_labels, "error"))
      cli::cli_abort(c("Problem with {.arg age_labels}.",
                       i = age_labels$message))
  }
  if (is.null(v)) {
    v <- versions[[1L]]
  }
  else {
    if (!(v %in% versions)) {
      n_version <- length(versions)
      if (n_version > 1L)
        msg_valid <- "Valid values for {.var v} with {.arg {nm_ssvd}} are: {.val {versions}}."
      else
        msg_valid <- "Only valid value for {.var v} with {.arg {nm_ssvd}} is {.val {versions}}."
      cli::cli_abort(c("Invalid value for version parameter {.var v}.",
                       i = msg_valid))
    }
  }
  is_version <- data$version == v
  is_type <- data$type == type
  data_version_type <- data[is_version & is_type, , drop = FALSE]
  if (has_age) {
    is_matched <- vapply(data_version_type$labels_age, setequal, TRUE, y = age_labels)
    if (!any(is_matched))
      cli::cli_abort("Can't find labels from {.arg age_labels} in {.arg object}.")
    i_matched <- which(is_matched)
  }
  else {
    lengths_labels <- lengths(data_version_type$labels_age)
    i_matched <- which.max(lengths_labels)
  }
  labels_age <- data_version_type$labels_age[[i_matched]]
  labels_sexgender <- data_version_type$labels_sexgender[[i_matched]]
  levels_age <- unique(labels_age)
  levels_sexgender <- unique(labels_sexgender)
  agesex <- if (type == "total") "age" else "age:sex"
  joint <- type == "joint"
  matrix <- get_matrix_or_offset_svd(ssvd = object,
                                     v = v,
                                     nm_ssvd = nm_ssvd,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = joint,
                                     agesex = agesex,
                                     get_matrix = TRUE,
                                     n_comp = n_comp)
  offset <- get_matrix_or_offset_svd(ssvd = object,
                                     v = v,
                                     nm_ssvd = nm_ssvd,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     joint = joint,
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
#' @param v Version of data to use.
#' @param n_draw Number of random draws to generate.
#' @param ... Unused. Included for generic consistency only.
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
#' ## females and males modeled independently
#' generate(HMD) 
#'
#' ## joint model for females and males
#' generate(HMD, indep = FALSE) 
#' 
#' ## SVD for females and males combined
#' generate(HMD, indep = NA)
#'
#' ## specify age groups
#' labels <- poputils::age_labels(type = "lt", max = 60)
#' generate(HMD, age_labels = labels)
#' @export
generate.bage_ssvd <- function(x,
                               v = NULL,
                               n_draw = 20,
                               n_comp = NULL,
                               indep = NULL,
                               age_labels = NULL,
                               ...) {
  nm_ssvd <- deparse1(substitute(x))
  check_has_no_dots(...)
  l <- generate_ssvd_helper(ssvd = x,
                            v = v,
                            nm_ssvd = nm_ssvd,
                            n_element = 1L,
                            n_draw = n_draw,
                            n_comp = n_comp,
                            indep = indep,
                            age_labels = age_labels)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  sd <- rep(1, times = n_draw)
  labels <- seq_len(ncol(matrix))
  alpha <- draw_vals_norm(sd = sd, labels = labels)
  value <- matrix %*% alpha + offset
  ans <- ans[-match("element", names(ans))]
  ans$value <- as.double(value)
  ans
}


## 'print' --------------------------------------------------------------------

#' @export
print.bage_ssvd <- function(x, ...) {
  cat("\n")
  cat(paste(rep("-", times = 60), collapse = ""), "\n")
  cat("<Object of class \"", class(x), "\">\n\n", sep = "")
  data <- x$data
  version <- data$version
  versions <- unique(version)
  type <- data$type
  types <- unique(type)
  sexgender <- data$labels_sexgender
  age <- data$labels_age
  cat("versions:\n")
  for (v in versions)
    cat("    ", v, "\n")
  cat("\n")
  has_sexgender <- "joint" %in% type
  if (has_sexgender) {
    levels_sexgender <- unique(sexgender[type == "joint"][[1L]])
    cat("sex/gender labels:\n")
    for (level in levels_sexgender)
      cat("    ", level, "\n")
    cat("\n")
  }
  cat("age labels:\n")
  is_first <- version == versions[[1L]] & type == types[[1L]]
  labels_age <- age[is_first]
  for (labels in labels_age) {
    n <- length(labels)
    if (n >= 9L)
      labels <- c(labels[1:3], "...", labels[c(n - 1L, n)])
    labels <- paste(labels, collapse = ", ")
    cat("    ", labels, "\n")
  }
  cat(paste(rep("-", times = 60), collapse = ""), "\n")
}

