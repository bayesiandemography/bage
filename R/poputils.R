
## functions to eventually move into package 'poputils'

## HAS_TESTS
#' Identify an age variable
#'
#' Find the element of `nms` that looks like an age variable.
#' If no elements look like an age variable, or if
#' two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_time()], [find_sexgender()]
#' 
#' @examples
#' find_age(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_age(c("Sex", "Year"))                 ## none valid
#' find_age(c("age", "age.years"))            ## two valid
#' @export
find_age <- function(nms) {
    p_valid <- paste0("^age$|^agegroup$|^agegp$|^ageyear$|^ageinterval$|",
                      "^ages$|^agegroups$|^agegps$|^ageyears$|^ageintervals$")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify female sex or gender labels
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to females.
#' If no elements look like a label for females,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_male()], [find_sexgender()]
#' 
#' @examples
#' find_female(c("Female", "Male")) ## one valid
#' find_female(c("0-4", "5-9"))     ## none valid
#' find_female(c("F", "Fem"))       ## two valid
#' @export
find_female <- function(nms) {
    p_valid <- paste0("^female$|^f$|^fem$|^women$|^girl$|",
                      "^females$|^girls")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify male sex or gender labels
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to males.
#' If no elements look like a label for males,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_female()], [find_sexgender()]
#' 
#' @examples
#' find_male(c("Female", "Male")) ## one valid
#' find_male(c("0-4", "5-9"))     ## none valid
#' find_male(c("male", "m"))      ## two valid
#' @export
find_male <- function(nms) {
    p_valid <- paste0("^male$|^m$|^men$|^boy$|",
                      "^males$|^boys")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify a sex or gender variable
#'
#' Find the element of `nms` that looks like
#' a sex or gender variable.
#' If no elements look like a sex or gender variable,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_age()], [find_time()], [find_female()],
#' [find_male()]
#' 
#' @examples
#' find_sexgender(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_sexgender(c("Age", "Region"))               ## none valid
#' find_sexgender(c("sexgender", "sexes"))          ## two valid
#' @export
find_sexgender <- function(nms) {
    p_valid <- paste0("^sex$|^gender$|^sexgender$|",
                      "^sexes$|^genders$")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify a time variable
#'
#' Find the element of `nms` that looks like an time variable.
#' If no elements look like a time variable, or if
#' two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_age()], [find_sexgender()]
#' 
#' @examples
#' find_time(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_time(c("Sex", "Region"))               ## none valid
#' find_time(c("time", "year"))                ## two valid
#' @export
find_time <- function(nms) {
    p_valid <- paste0("^time$|^period$|^year$|^month$|^quarter$|",
                      "^times$|^periods$|^years$|^months$|^quarters$|",
                      "^yearmonth$|^monthyear$|^yearquarter$|^quarteryear$")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Helper function for find_* functions
#'
#' @param nms A character vector
#' @param p_valid A regular expression used
#' to identify valid members of 'nms'
#'
#' @returns An element of 'nms' or NULL.
#'
#' @noRd
find_inner <- function(nms, p_valid) {
    nms_cleaned <- tolower(nms)
    nms_cleaned <- gsub("[^a-z]", "", nms_cleaned)
    i <- grep(p_valid, nms_cleaned)
    if (identical(length(i), 1L))
        nms[[i]]
    else
        NULL
}


## HAS_TESTS
#' Build a matrix from measure and classification variables
#'
#' Build a matrix where the elements are values of
#' a measure variable, and the rows and columns
#' are formed by interactions between classification
#' variables. The classification variables picked
#' out by `rows` and `cols` must uniquely identify
#' cells. `to_matrix()`, unlike `stats::xtabs()`,
#' does not sum across multiple combinations of
#' classification variables.
#'
#' @param x A data frame.
#' @param rows The classification variable(s)
#' used to distinguish rows in the matrix.
#' @param cols The classification variable(s)
#' used to distinguish columns in the matrix.
#' @param measure The measure variable, eg
#' some rates or counts.
#'
#' @returns A matrix
#'
#' @examples
#' m1 <- to_matrix(injuries,
#'                 rows = c(age, sex),
#'                 cols = c(ethnicity, year),
#'                 measure = injuries)
#' m1[1:5, 1:5]
#' dimnames(m1)
#'
#' m2 <- to_matrix(injuries,
#'                 rows = c(age, sex, ethnicity),
#'                 cols = year,
#'                 measure = injuries)
#' m2[1:5, 1:5]
#' dimnames(m2)
#'
#' ## cells not uniquely identified
#' try(
#' to_matrix(injuries,
#'           rows = age,
#'           cols = sex,
#'           measure = injuries)
#' )
#' @export
to_matrix <- function(x, rows, cols, measure) {
    ## check 'x'
    checkmate::assert_data_frame(x,
                                 any.missing = FALSE,
                                 min.cols = 3L)
    ## make 'i_measure'
    measure <- rlang::enquo(measure)
    i_measure <- tidyselect::eval_select(measure, data = x)
    n_measure <- length(i_measure)
    if (n_measure == 0L)
        stop(gettext("no measure variable supplied"),
             call. = FALSE)
    if (n_measure > 1L)
        stop(gettextf("attempt to select %d measure variables",
                      n_measure),
             call. = FALSE)
    ## make 'i_rows'
    rows <- rlang::enquo(rows)
    i_rows <- tidyselect::eval_select(rows, data = x)
    if (length(i_rows) == 0L)
        stop(gettextf("no value supplied for '%s'",
                      "rows"),
             call. = FALSE)
    if (i_measure %in% i_rows)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(i_measure),
                      "measure",
                      "rows"),
             call. = FALSE)
    ## make 'i_cols'
    cols <- rlang::enquo(cols)
    i_cols <- tidyselect::eval_select(cols, data = x)
    if (length(i_cols) == 0L)
        stop(gettextf("no value supplied for '%s'",
                      "cols"),
             call. = FALSE)
    if (i_measure %in% i_cols)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(i_measure),
                      "measure",
                      "cols"),
             call. = FALSE)
    i_rowcol <- intersect(i_rows, i_cols)
    if (length(i_rowcol) > 0L)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(x)[[i_rowcol[1L]]],
                      "rows",
                      "cols"),
             call. = FALSE)
    ## check for duplicate combinations of classification variables
    i_classif <- c(i_rows, i_cols)
    classif_vars <- x[i_classif]
    is_dup <- duplicated(classif_vars)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L) {
        nms_classif <- names(classif_vars)
        vals_dup <- classif_vars[i_dup, ]
        vals_dup <- vapply(vals_dup, as.character, "")
        vals_dup <- sprintf("\"%s\"", vals_dup)
        nms_vals <- paste(nms_classif, vals_dup, sep = "=")
        nms_vals <- paste(nms_vals, collapse = ", ")
        stop(gettextf("'%s' has two rows with values c(%s)",
                      "x",
                      nms_vals),
             call. = FALSE)
    }
    ## form interactions
    var_rows <- interaction(x[i_rows], lex.order = TRUE)
    var_cols <- interaction(x[i_cols], lex.order = TRUE)
    levels_rows <- levels(var_rows)
    levels_cols <- levels(var_cols)
    ## construct matrix and return
    ans <- matrix(NA,
                  nrow = length(levels_rows),
                  ncol = length(levels_cols),
                  dimnames = list(levels_rows, levels_cols))
    i <- match(var_rows, levels_rows)
    j <- match(var_cols, levels_cols)
    ans[cbind(i, j)] <- x[[i_measure]]
    ans
}
