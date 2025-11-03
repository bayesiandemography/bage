
## Checks that involve inspection by a human, and so are not run by testthat

library(bage)
library(dplyr)
library(poputils)
library(ggplot2)

## SVD tests ------------------------------------------------------------------

nms_svd <- c("HFD",
             "HIMD_R", "HIMD_P1", "HIMD_P5",
             "HMD",
             "LFP",
             "WMD_C", "WMD_E")

transforms <- list(exp,
                   exp, invlogit, invlogit,
                   identity,
                   invlogit,
                   invlogit, invlogit)

has_sex_var <- function(x) "indep" %in% x$data$type

## components

graphics.off()
pdf(file = "manual-checks-svd-components.pdf",
    w = 8,
    h = 8,
    onefile = TRUE)

for (nm in nms_svd) {
  obj <- get(nm)
  has_sex <- has_sex_var(obj)
  indep <- if (has_sex) FALSE else NULL
  comp <- components(obj, n_comp = 5, indep = indep)
  if (has_sex) {
    p <- ggplot(comp, aes(x = age_mid(age), y = value, color = sex))
  } else {
    p <- ggplot(comp, aes(x = age_mid(age), y = value))
  }
  p <- p +
    facet_wrap(vars(component)) +
    geom_line() +
    ggtitle(nm)
  plot(p)
}

dev.off()


## generate

set.seed(0)
graphics.off()
pdf(file = "manual-checks-svd-generate.pdf",
    w = 12,
    h = 12,
    onefile = TRUE)

for (i in seq_along(nms_svd)) {
  nm <- nms_svd[[i]]
  transform <- transforms[[i]]
  obj <- get(nm)
  has_sex <- has_sex_var(obj)
  indep <- if (has_sex) FALSE else NULL
  gen <- generate(obj, indep = indep, n_comp = 5)
  gen$value <- transform(gen$value)
  if (has_sex) {
    p <- ggplot(gen, aes(x = age_mid(age), y = value, color = sex))
  } else {
    p <- ggplot(gen, aes(x = age_mid(age), y = value))
  }
  p <- p +
    facet_wrap(vars(draw)) +
    geom_line() +
    ggtitle(nm)
  plot(p)
}

dev.off()

file.remove("manual-checks-svd-components.pdf")
file.remove("manual-checks-svd-generate.pdf")

