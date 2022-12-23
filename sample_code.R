
model <- mod_pois(deaths ~ age:sex + age:time + sex:time,
                  exposure = population) %>%
    set_data(mydata) %>%
    set_datamod_rr(...) %>%
    set_prior(sex:age ~ RW2()) %>%
    set_prior(age:time ~ LinAR()) %>%                  
    set_age_transform("hmd") %>%    
    fit() %>%
    predict(n = 10)


account <- mvt_account() %>%
    set_mod_pois(xxxx) %>%
    xxx

