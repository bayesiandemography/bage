
## get_matrix_svd <- function(val, nm_term, var_age, var_sexgender, ...) {
##     svd_type <- get_svd_type(nm_term = nm_term,
##                              var_age = var_age,
##                              var_sexgender = var_sexgender)
##     if (svd_type == "age") {

##     }
##     else if (svd_type == "sexage") {

##     }
##     else if (svd_type == "agesex") {

##     }
##     else
##         cli::cli_abort("Internal error: unexpected value for {.val svd_type}.")
## }



## get_offset_svd <- function(val, ...) {

## }


                          

## interact <- function(x, y) {
##     ans <- outer(x, y, paste, sep = ".")
##     as.character(ans)
## }


## #' If 'levels_sex' contains exactly two distinct values, then attempt to reformat it.
## #' If the reformatting fails, then leave it alone.
## reformat_agesex <- function(levels_agesex,
##                             levels_age,
##                             levels_sex,
##                             var_age,
##                             var_sex,
##                             age_first) {
##     levels_age_new <- tryCatch(reformat(levels_age, factor = FALSE),
##                                error = function(e) e)
##     if (inherits(levels_age_new, "error"))
##         cli::cli_abort(c("Problem with levels for age variable ({.var {var_age}}).",
##                          error$message))
##     two_sex_labels <- length(unique(level_sex)) == 2L
##     if (two_sex_labels) {
##         levels_sex_new <- tryCatch(reformat(levels_sex, factor = FALSE),
##                                    error = function(e) e)
##         if (inherits(levels_sex_new, "error"))
##             levels_sex_new <- levels_sex
##     }
##     else
##         levels_sex_new <- levels_sex
##     if (age_first) {
##         levels_agesex_implied <- interact(levels_age, levels_sex)
##         levels_agesex_new <- interact(levels_age_new, levels_sex_new)
##     }
##     else {
##         levels_agesex_implied <- interact(levels_sex, levels_age)
##         levels_agesex_new <- interact(levels_sex_new, levels_age_new)
##     }
##     i <- match(levels_agesex, levels_agesex_implied, nomatch = 0L)
##     i_unmatched <- match(0L, i, nomatch = 0L)
##     if (i_unmatched > 0L)
##         cli::cli_abort(paste("Internal error: value {.val {levels_agesex[[i_unmatched]]}} not",
##                              "composed of values from age and sex/gender variables."))
##     levels_agesex_new[i]
## }
    
    


## get_index_svd <- function(nm_prior,
##                           var_age,
##                           var, 
##                           levels_par,
##                           nm_prior,
##                           var_age,
##                           var_sexgender,
##                           ...) {
##     val_svd <- prior$val
##     indep <- prior$indep
##     if (is.null(var_age)) {
##         cli::cli_abort(c("Can't use SVD prior when age variable unknown.",
##                          i = "Need to use function {.fun set_var_age} to specify age variable?"))
##     }
##     nms_term <- str_split(nm_prior)[[1L]]
##     n_dim <- length(nms_term)
##     if (n_dim == 1L) {
##         if (nms_term != var_age)
##             cli::cli_abort(c("If SVD used with main effect, must be used with age variable."
##                              i = "SVD prior used with variable {.val {nms_dims}}.",
##                              i = "{.var var_age} currently set to {.val {var_age}}."))
##         levels_age <- poputils::reformat_age(levels_par, factor = FALSE)
##         ans <- make_matrix_svd_age(val = val,
##                                    levels_age = levels_age)
##     }
##     else if (n_dim == 2L) {
##         if (is.null(var_sexgender))
##             cli::cli_abort(c("Can't use SVD prior with interaction when sex/gender variable unknown.",
##                              i = paste("Need to use function {.fun set_var_sexgender} to",
##                                        "specify sex/gender variable?")))
##         if (!setequal(nms_term, c(var_age, var_sexgender)))
##             cli::cli_abort(c("If SVD used with interaction, must be used with age and sex/gender variables."
##                              i = "SVD prior used with variables {.val {nms_dims}}.",
##                              i = "{.var var_age} currently set to {.val {var_age}}.",
##                              i = "{.var var_sexgender} currently set to {.val {var_sexgender}}."))
##         levels_agesex <- reformat_agesex(levels_par)
##         ans <- make_matrix_svd_agesex(val = val,
##                                       levels_agesex = levels_agesex,
##                                       indep = indep)
##     }
##     else
##         cli::cli_abort(c("SVD prior used for interaction between {nms_terms}.",
##                          i = paste("SVD prior can only be used with age main effects,",
##                                    "or with interactions between age and sex.")))
##     ans
## }





## make_matrix_parfree_par.bage_val_svd <- function(x, labels_dims, nms_dims) {
##     indep <- x$indep 
##     if (indep)
##         make_matrix_svd_indep(x = x,
##                               labels_dims = labels_dims,
##                               nms_dims = nms_dims)
##     else
##         make_matrix_svd_dep(x = x,
##                             labels_dims = labels_dims,
##                             nms_dims = nms_dims)
## }

## make_offset_par.bage_val_svd <- function(x, labels_dims, nms_dims) {
##     indep <- x$indep 
##     if (indep)
##         make_offset_svd_indep(x = x,
##                               labels_dims = labels_dims,
##                               nms_dims = nms_dims)
##     else
##         make_offset_svd_dep(x = x,
##                             labels_dims = labels_dims,
##                             nms_dims = nms_dims)
## }



## make_matrix_svd_dep <- function(x, labels_dims, nms_dims) {
##     arrays <- x$arrays
##     index <- get_index_within_val_svd(x = x,
##                                       labels_dims = labels_dims,
##                                       names_dims = names_dims)
##     array <- arrays[[index]]
##     n_comp <- dim(array)[length(dim(array))]
##     ans <- matrix(array, ncol = n_comp)
##     labels_dims_vec <- make_labels_vec(labels_dims, ignore_last = FALSE)
##     labels_array <- dimnames(array)
##     labels_ans_vec <- make_labels_vec(labels_array, ignore_last = TRUE)
##     i <- match(labels_vec_dims, labels_vec_ans)
##     ans <- ans[i, , drop = FALSE]
##     ans
## }


## make_offset_svd_dep <- function(x, labels_dims, nms_dims) {
##     offsets <- x$offsets
##     index <- get_index_within_val_svd(x = x,
##                                       labels_dims = labels_dims,
##                                       names_dims = names_dims)
##     ans <- offsets[[index]]
##     labels_dims_vec <- make_labels_vec(labels_dims, ignore_last = FALSE)
##     labels_ans <- dimnames(ans)
##     labels_ans_vec <- make_labels_vec(labels_ans, ignore_last = FALSE)
##     i <- match(labels_vec_dims, labels_vec_ans)
##     ans <- ans[i]
##     ans
## }


## make_offset_svd_indep <- function(x, labels_dims, nms_dims) {
##     offsets <- x$offsets
##     indices <- get_indices_within_val_svd(x = x,
##                                           labels_dims = labels_dims,
##                                           names_dims = names_dims)
##     ans <- offsets[indices]
##     ans <- Reduce(outer, offsets)
##     labels_dims_vec <- make_labels_vec(labels_dims, ignore_last = FALSE)
##     labels_ans <- dimnames(ans)
##     labels_ans_vec <- make_labels_vec(labels_ans, ignore_last = FALSE)
##     i <- match(labels_vec_dims, labels_vec_ans)
##     ans <- ans[i]
##     ans
## }



## ## assume that 'labels_dim' for age and sexgender dimensions have already
## ## been reformatted to standard format
## get_index_within_val_svd <- function(x, labels_dims, nms_dims) {
##     nm_val_svd <- x$nm_val_svd
##     dimnames_val <- lapply(x$offsets, dimnames)
##     dimnames_data <- stats::setNames(labels_dims, nms_dims)
##     i_val <- NULL
##     for (i in seq_along(dimnames_val)) {
##         dimnames_val_i <- dimnames_val[[i]]
##         if (list_equal(dimnames_val_i, dimnames_data)) {
##             i_val <- i
##             break
##         }
##     }
##     if (is.null(i_val))
##         cli::cli_abort(c("Could not find transform and offset with same levels as {.val {nm_var}}.",
##                          i = "Looking in {.val {nm_val}}.",
##                          i = "Levels for {.val {nm_var}} variable: {labels_par}."))
##     i_val
## }


## list_equal <- function(x, y) {
##     check_valid_list_equal_list(x)
##     check_valid_list_equal_list(y)
##     if (length(x) != length(y))
##         return(FALSE)
##     if (!setequal(names(x), names(y)))
##         return(FALSE)
##     nms_x <- names(x)
##     ans <- FALSE
##     for (i in seq_along(x)) {
##         el_x <- x[[i]]
##         nm <- nms_x[[i]]
##         el_y <- y[[nm]]
##         if (!setequal(el_x, el_y)) {
##             ans <- TRUE
##             break
##         }
##     }
##     ans
## }
    

