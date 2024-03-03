# 'print' works with mod_pois

    Code
      print(mod)
    Output
      -- Unfitted Poisson model --
      
           deaths ~ age + sex + time
      
      (Intercept) ~ NFix(sd=10)
              age ~ RW()
              sex ~ N()
             time ~ RW()
      
           dispersion: s=1
             exposure: popn
              var_age: age
        var_sexgender: sex
             var_time: time
               n_draw: 1000

---

    Code
      print(mod)
    Output
      -- Fitted Poisson model --
      
           deaths ~ age + sex + time
      
      (Intercept) ~ NFix(sd=10)
              age ~ RW()
              sex ~ N()
             time ~ RW()
      
           dispersion: s=1
             exposure: popn
              var_age: age
        var_sexgender: sex
             var_time: time
               n_draw: 1000

