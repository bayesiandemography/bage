
logit <- function(p) {
    log(p) - log(1 - p)
}

invlogit <- function(x) {
    is_x_pos <- x > 0
    ans <- double(length = x)
    exp_neg_x <- exp(-1 * x[is_x_pos])
    exp_x <- exp(x[!is_x_pos])
    ans[is_x_pos] <- 1 / (1 + exp_neg_x)
    ans[!is_x_pos] <- exp_x / (1 + exp_x)
    ans
}
               
    
