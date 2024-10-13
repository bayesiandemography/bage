# 'print_prior' works

    Code
      print_prior(RW(), nms = c("s", "along"), slots = c("scale", "along"))
    Output
        RW() 
               s: 1
           along: NULL

# 'print_prior_header' works

    Code
      print_prior_header(AR())
    Output
        AR() 

# 'print_prior_slot' works

    Code
      print_prior_slot(AR(), nm = "n_coef", slot = "n_coef")
    Output
          n_coef: 2

