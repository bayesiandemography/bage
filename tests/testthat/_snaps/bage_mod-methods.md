# 'print' works with mod_pois

    Code
      print(mod)
    Output
      -- Unfitted Poisson model --
      
           deaths ~ age + sex + time
      
      (Intercept) ~ NFix()
              age ~ RW()
              sex ~ NFix()
             time ~ RW()
      
      data model for outcome: rr3()
      
       term along n_par n_par_free
        age   age    10          9
        sex     -     2          2
       time  time     6          5
      
           dispersion: mean=1
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
      
      (Intercept) ~ NFix()
              age ~ RW()
              sex ~ NFix()
             time ~ RW()
      
      data model for outcome: rr3()
      
       term along n_par n_par_free std_dev
        age   age    10          9  0.0471
        sex     -     2          2  0.0028
       time  time     6          5  0.0142
      
           dispersion: mean=1
             exposure: popn
              var_age: age
        var_sexgender: sex
             var_time: time
               n_draw: 1000

