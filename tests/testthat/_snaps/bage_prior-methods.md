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
             con: none

---

    Code
      print(AR1(min = 0.2))
    Output
        AR1(min=0.2) 
             min: 0.2
             max: 0.98
               s: 1
           along: NULL
             con: none

---

    Code
      print(DRW())
    Output
        DRW() 
               s: 1
              sd: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
           along: NULL
             con: none

---

    Code
      print(DRW(sd = 0))
    Output
        DRW(sd=0) 
               s: 1
              sd: 0
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
           along: NULL
             con: none

---

    Code
      print(DRW2())
    Output
        DRW2() 
               s: 1
              sd: 1
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
           along: NULL
             con: none

---

    Code
      print(DRW2(sd = 0))
    Output
        DRW2(sd=0) 
               s: 1
              sd: 0
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
           along: NULL
             con: none

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
             con: none

---

    Code
      print(Lin(s = 0))
    Output
        Lin(s=0) 
               s: 0
      mean_slope: 0
        sd_slope: 1
           along: NULL
             con: none

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
             con: none

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
        RW() 
               s: 1
              sd: 1
           along: NULL
             con: none

---

    Code
      print(RW_Seas(n_seas = 2))
    Output
        RW_Seas(n_seas=2) 
          n_seas: 2
               s: 1
              sd: 1
          s_seas: 0
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW_Seas(n_seas = 2, s_seas = 1))
    Output
        RW_Seas(n_seas=2,s_seas=1) 
               n: NULL
               s: 1
              sd: 1
          s_seas: 1
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW(sd = 0))
    Output
        RW(sd=0) 
               s: 1
              sd: 0
           along: NULL
             con: none

---

    Code
      print(RW_Seas(n_seas = 2, sd = 0))
    Output
        RW_Seas(n_seas=2,sd=0) 
          n_seas: 2
               s: 1
              sd: 0
          s_seas: 0
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW_Seas(n_seas = 2, s_seas = 1, sd = 0))
    Output
        RW_Seas(n_seas=2,sd=0,s_seas=1) 
          n_seas: 2
               s: 1
              sd: 0
          s_seas: 1
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW2_Infant())
    Output
        RW2_Infant() 
               s: 1
        sd_slope: 1
             con: none

---

    Code
      print(RW2())
    Output
        RW2() 
               s: 1
              sd: 1
        sd_slope: 1
           along: NULL
             con: none

---

    Code
      print(RW2_Seas(n_seas = 2))
    Output
        RW2_Seas(n_seas=2) 
          n_seas: 2
               s: 1
              sd: 1
        sd_slope: 1
          s_seas: 0
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW2_Seas(n_seas = 2, s_seas = 1))
    Output
        RW2_Seas(n_seas=2,s_seas=1) 
          n_seas: 2
               s: 1
              sd: 1
        sd_slope: 1
          s_seas: 1
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW2(sd = 0))
    Output
        RW2(sd=0) 
               s: 1
              sd: 0
        sd_slope: 1
           along: NULL
             con: none

---

    Code
      print(RW2_Seas(n_seas = 2, sd = 0))
    Output
        RW2_Seas(n_seas=2,sd=0) 
          n_seas: 2
               s: 1
              sd: 0
        sd_slope: 1
          s_seas: NULL
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(RW2_Seas(n_seas = 2, sd = 0, s_seas = 1))
    Output
        RW2_Seas(n_seas=2,sd=0,s_seas=1) 
          n_seas: 2
               s: 1
              sd: NULL
        sd_slope: 1
          s_seas: 1
         sd_seas: 1
           along: NULL
             con: none

---

    Code
      print(Sp())
    Output
        Sp() 
          n_comp: NULL
               s: 1
              sd: 1
        sd_slope: 1
           along: NULL
             con: none

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
          n_comp: 2
          n_coef: 2
             min: -1
             max: 1
               s: 1
             con: none

---

    Code
      print(SVD_AR(HMD, indep = FALSE))
    Output
        SVD_AR(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
          n_coef: 2
             min: -1
             max: 1
               s: 1
             con: none

---

    Code
      print(SVD_AR1(HMD))
    Output
        SVD_AR1(HMD) 
            ssvd: HMD
          n_comp: 2
             min: 0.8
             max: 0.98
               s: 1
             con: none

---

    Code
      print(SVD_AR1(HMD, indep = FALSE))
    Output
        SVD_AR1(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
             min: 0.8
             max: 0.98
               s: 1
             con: none

---

    Code
      print(SVD_DRW(HMD))
    Output
        SVD_DRW(HMD) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW(HMD, indep = FALSE))
    Output
        SVD_DRW(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW(HMD, sd = 0))
    Output
        SVD_DRW(HMD,sd=0) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 0
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW(HMD, sd = 0, indep = FALSE))
    Output
        SVD_DRW(HMD,indep=FALSE,sd=0) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 0
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW2(HMD))
    Output
        SVD_DRW2(HMD) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 1
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW2(HMD, indep = FALSE))
    Output
        SVD_DRW2(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 1
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW2(HMD, sd = 0))
    Output
        SVD_DRW2(HMD,sd=0) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 0
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_DRW2(HMD, sd = 0, indep = FALSE))
    Output
        SVD_DRW2(HMD,indep=FALSE,sd=0) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 0
        sd_slope: 1
          shape1: 5
          shape2: 5
             min: 0.8
             max: 0.98
             con: none

---

    Code
      print(SVD_Lin(HMD))
    Output
        SVD_Lin(HMD) 
            ssvd: HMD
          n_comp: 2
               s: 1
      mean_slope: 0
        sd_slope: 1
             con: none

---

    Code
      print(SVD_Lin(HMD, s = 0))
    Output
        SVD_Lin(HMD,s=0) 
            ssvd: HMD
          n_comp: 2
               s: 0
      mean_slope: 0
        sd_slope: 1
             con: none

---

    Code
      print(SVD_RW(HMD))
    Output
        SVD_RW(HMD) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 1
             con: none

---

    Code
      print(SVD_RW(HMD, indep = FALSE))
    Output
        SVD_RW(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 1
             con: none

---

    Code
      print(SVD_RW(HMD, sd = 0))
    Output
        SVD_RW(HMD,sd=0) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 0
             con: none

---

    Code
      print(SVD_RW(HMD, sd = 0, indep = FALSE))
    Output
        SVD_RW(HMD,indep=FALSE,sd=0) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 0
             con: none

---

    Code
      print(SVD_RW2(HMD))
    Output
        SVD_RW2(HMD) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 1
        sd_slope: 1
             con: none

---

    Code
      print(SVD_RW2(HMD, indep = FALSE))
    Output
        SVD_RW2(HMD,indep=FALSE) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 1
        sd_slope: 1
             con: none

---

    Code
      print(SVD_RW2(HMD, sd = 0))
    Output
        SVD_RW2(HMD,sd=0) 
            ssvd: HMD
          n_comp: 2
               s: 1
              sd: 0
        sd_slope: 1
             con: none

---

    Code
      print(SVD_RW2(HMD, sd = 0, indep = FALSE))
    Output
        SVD_RW2(HMD,indep=FALSE,sd=0) 
            ssvd: HMD
          n_comp: 2
           indep: FALSE
               s: 1
              sd: 0
        sd_slope: 1
             con: none

