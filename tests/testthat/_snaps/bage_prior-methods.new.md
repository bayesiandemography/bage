# 'print' works

    Code
      print(AR())
    Output
        AR() 
          n_coef: 2
             min: -1
             max: 1
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(AR1(min = 0.2))
    Output
        AR1(min=0.2) 
             min: 0.2
             max: 0.98
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(Known(c(0.2, -0.2)))
    Output
      Known(c(0.2,-0.2)) 

---

    Code
      print(Lin())
    Output
        Lin() 
               s: 1
      mean_slope: 0
        sd_slope: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(Lin(s = 0))
    Output
        Lin(s=0) 
               s: 0
      mean_slope: 0
        sd_slope: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(Lin_AR())
    Output
        Lin_AR() 
          n_coef: 2
               s: 1
      mean_slope: 0
        sd_slope: 1
             min: -1
             max: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(N())
    Output
        N() 
               s: 1

---

    Code
      print(NFix())
    Output
        NFix() 
              sd: 1

---

    Code
      print(RW())
    Output
      $i_prior
      [1] 19
      
      $const
      scale    sd 
          1     1 
      
      $specific
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rwrandom" "bage_prior"         

---

    Code
      print(RW_Seas(n_seas = 2))
    Output
      $i_prior
      [1] 20
      
      $const
       n_seas sd_seas   scale      sd 
            2       1       1       1 
      
      $specific
      $specific$n_seas
      [1] 2
      
      $specific$sd_seas
      [1] 1
      
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rwrandomseasfix" "bage_prior"                

---

    Code
      print(RW_Seas(n_seas = 2, s_seas = 1))
    Output
      $i_prior
      [1] 21
      
      $const
          n_seas scale_seas    sd_seas      scale         sd 
               2          1          1          1          1 
      
      $specific
      $specific$n_seas
      [1] 2
      
      $specific$scale_seas
      [1] 1
      
      $specific$sd_seas
      [1] 1
      
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rwrandomseasvary" "bage_prior"                 

---

    Code
      print(RW2())
    Output
      $i_prior
      [1] 22
      
      $const
         scale       sd sd_slope 
             1        1        1 
      
      $specific
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$sd_slope
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rw2random" "bage_prior"          

---

    Code
      print(RW2_Infant())
    Output
        RW2_Infant() 
               s: 1
        sd_slope: 1
        zero_sum: FALSE

---

    Code
      print(RW2_Seas(n_seas = 2))
    Output
      $i_prior
      [1] 23
      
      $const
        n_seas  sd_seas    scale       sd sd_slope 
             2        1        1        1        1 
      
      $specific
      $specific$n_seas
      [1] 2
      
      $specific$sd_seas
      [1] 1
      
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$sd_slope
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rw2randomseasfix" "bage_prior"                 

---

    Code
      print(RW2_Seas(n_seas = 2, s_seas = 1))
    Output
      $i_prior
      [1] 24
      
      $const
          n_seas scale_seas    sd_seas      scale         sd   sd_slope 
               2          1          1          1          1          1 
      
      $specific
      $specific$n_seas
      [1] 2
      
      $specific$scale_seas
      [1] 1
      
      $specific$sd_seas
      [1] 1
      
      $specific$scale
      [1] 1
      
      $specific$sd
      [1] 1
      
      $specific$sd_slope
      [1] 1
      
      $specific$along
      NULL
      
      $specific$zero_sum
      [1] FALSE
      
      
      attr(,"class")
      [1] "bage_prior_rw2randomseasvary" "bage_prior"                  

---

    Code
      print(Sp())
    Output
        Sp() 
          n_comp: NULL
               s: 1
        sd_slope: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD(HMD))
    Output
        SVD(HMD) 
            ssvd: HMD
          n_comp: 3

---

    Code
      print(SVD(HMD, indep = FALSE))
    Output
        SVD(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 3
           indep: FALSE

---

    Code
      print(SVD_AR(HMD))
    Output
        SVD_AR(HMD) 
            ssvd: HMD
          n_comp: 3
          n_coef: 2
             min: -1
             max: 1
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_AR(HMD, indep = FALSE))
    Output
        SVD_AR(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 3
           indep: FALSE
          n_coef: 2
             min: -1
             max: 1
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_AR1(HMD))
    Output
        SVD_AR1(HMD) 
            ssvd: HMD
          n_comp: 3
             min: 0.8
             max: 0.98
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_AR1(HMD, indep = FALSE))
    Output
        SVD_AR1(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 3
           indep: FALSE
             min: 0.8
             max: 0.98
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_RW(HMD))
    Output
        SVD_RW(HMD) 
            ssvd: HMD
          n_comp: 3
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_RW(HMD, indep = FALSE))
    Output
        SVD_RW(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 3
           indep: FALSE
               s: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_RW2(HMD))
    Output
        SVD_RW2(HMD) 
            ssvd: HMD
          n_comp: 3
               s: 1
        sd_slope: 1
           along: NULL
        zero_sum: FALSE

---

    Code
      print(SVD_RW2(HMD, indep = FALSE))
    Output
        SVD_RW2(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 3
           indep: FALSE
               s: 1
        sd_slope: 1
           along: NULL
        zero_sum: FALSE

