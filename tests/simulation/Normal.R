
library(bage)
set.seed(0)

data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                    sex = c("Female", "Male"),
                    time = 2001:2005)
data$income <- NA
mod_est <- mod_norm(income ~ age + sex + time,
                    data = data)
mod_est <- set_prior(mod_est, age ~ SVD(LFP))
mod_est <- set_prior(mod_est, time ~ AR1())
mod_est <- set_disp(mod_est, mean = 0.05)
rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
print(rep)
