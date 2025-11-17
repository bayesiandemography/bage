
## Checks that involve inspection by a human, and so are not run by testthat

library(bage)
library(dplyr)
library(poputils)
library(ggplot2)

## Datasets -------------------------------------------------------------------

p_isl_deaths <- ggplot(isl_deaths, aes(x = age_mid(age), y = deaths / popn, col = sex)) +
  facet_wrap(vars(time)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("isl_deaths - rates")

p_kor_births <- ggplot(kor_births, aes(x = time, y = births / popn)) +
  facet_grid(vars(age), vars(region)) +
  geom_line() +
  ggtitle("kor_births - rates")

p_nld_expenditure <- ggplot(nld_expenditure,
                            aes(x = age_mid(age), y = value)) +
  facet_grid(vars(diag), vars(year), scale = "free_y") +
  geom_line() +
  ggtitle("nld_expenditure - expenditures")

p_nzl_divorces <- ggplot(nzl_divorces,
                         aes(x = age_mid(age), y = divorces / population, color = sex)) +
  facet_wrap(vars(time)) +
  geom_line() +
  ggtitle("nzl_divorces - rates")

p_nzl_households <- ggplot(nzl_households,
                           aes(x = age_mid(age),
                               y = oneperson / total,
                               color = factor(year))) +
  facet_wrap(vars(region)) +
  geom_line() +
  ggtitle("nzl_households - rates one-person")

p_nzl_injuries <- ggplot(nzl_injuries,
                         aes(x = age_mid(age),
                             y = injuries / popn,
                             linetype = sex,
                             color = ethnicity)) +
  facet_wrap(vars(year)) +
  geom_line() +
  ggtitle("nzl_injuries - rates")

p_prt_deaths <- ggplot(prt_deaths,
                         aes(x = age_mid(age),
                             y = deaths / exposure,
                             color = sex)) +
  facet_wrap(vars(time)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("prt_deaths - rates")

p_swe_infant <- ggplot(swe_infant,
                       aes(x = time,
                           y = deaths / births)) +
  facet_wrap(vars(county)) +
  geom_line() +
  ggtitle("swe_infant - rates")


p_usa_deaths <- ggplot(usa_deaths,
         aes(x = as.Date(paste0(month, "-01"), format = "%Y-%b-%d"),
             y = deaths)) +
  geom_line() +
  ggtitle("usa_deaths - counts")


graphics.off()
pdf(file = "manual-checks-datasets.pdf",
    w = 10,
    h = 10,
    onefile = TRUE)
plot(p_isl_deaths)
plot(p_kor_births)
plot(p_nld_expenditure)
plot(p_nzl_divorces)
plot(p_nzl_households)
plot(p_nzl_injuries)
plot(p_prt_deaths)
plot(p_swe_infant)
plot(p_usa_deaths)
dev.off()

file.remove("manual-checks-datasets.pdf")


## Scaled SVDs ----------------------------------------------------------------

nms_svd <- c("CSA",
             "HFD",
             "HIMD_R", "HIMD_P1", "HIMD_P5",
             "HMD",
             "LFP",
             "WMD_C", "WMD_E")

transforms <- list(invlogit,
                   exp,
                   exp, invlogit, invlogit,
                   identity,
                   invlogit,
                   invlogit, invlogit)

has_sex_var <- function(x) "indep" %in% x$data$type

n_comp <- 5

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
  comp <- components(obj, n_comp = n_comp, indep = indep)
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
  gen <- generate(obj, indep = indep, n_comp = n_comp)
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

