# 'print' works with mod_pois

    Code
      print(mod)
    Output
      
          ------ Unfitted Poisson model ------
      
      
         deaths ~ age + sex + time
      
      
          data model for outcome: rr3()
      
      
              term  prior along zero_sum n_par n_par_free
       (Intercept) NFix()     -        -     1          1
               age   RW()   age        -    10          9
               sex NFix()     -        -     2          2
              time   RW()  time        -     6          5
      
      
                dispersion: mean=1
                  exposure: popn
                   var_age: age
             var_sexgender: sex
                  var_time: time
                    n_draw: 1000

