# 'print' works with mod_pois

    Code
      print(mod)
    Output
      
          ------ Unfitted Poisson model ------
      
         deaths ~ age + sex + time
      
        exposure = popn
      
         data model for outcome: rr3()
      
              term  prior along n_par n_par_free
       (Intercept) NFix()     -     1          1
               age   RW()   age    10         10
               sex NFix()     -     2          2
              time   RW()  time     6          6
      
       covariates: ~income
      
       disp: mean = 1
      
       n_draw var_time var_age var_sexgender
         1000     time     age           sex
      

# 'print' works with mod_pois - inner-outer fitting method

    Code
      print(mod)
    Output
      
          ------ Unfitted Poisson model ------
      
         deaths ~ age + sex + time
      
        exposure = popn
      
              term  prior along n_par n_par_free
       (Intercept) NFix()     -     1          1
               age   RW()   age    10         10
               sex NFix()     -     2          2
              time   RW()  time     6          6
      
       disp: mean = 1
      
       n_draw var_time var_age var_sexgender
         1000     time     age           sex
      

