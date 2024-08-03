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

---

    Code
      print(AR1(min = 0.2))
    Output
        AR1(min=0.2) 
           min: 0.2
           max: 0.98
             s: 1
         along: NULL

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
            sd: 1
         along: NULL

---

    Code
      print(Lin_AR())
    Output
        Lin_AR() 
        n_coef: 2
             s: 1
            sd: 1
           min: -1
           max: 1
         along: NULL

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
         along: NULL

---

    Code
      print(RW_Seas(n_seas = 2, s_seas = 0))
    Output
        RW_Seas(n_seas=2,s_seas=0) 
        n_seas: 2
             s: 1
        s_seas: 0
         along: NULL

---

    Code
      print(RW_Seas(n_seas = 2))
    Output
        RW_Seas(n_seas=2) 
             n: NULL
             s: 1
        s_seas: 1
         along: NULL

---

    Code
      print(RW2())
    Output
        RW2() 
             s: 1
         along: NULL

---

    Code
      print(RW2_Seas(n_seas = 2, s_seas = 0))
    Output
        RW2_Seas(n_seas=2,s_seas=0) 
        n_seas: 2
             s: 1
        s_seas: 0
         along: NULL

---

    Code
      print(RW2_Seas(n_seas = 2))
    Output
        RW2_Seas(n_seas=2) 
        n_seas: 2
             s: 1
        s_seas: 1
         along: NULL

---

    Code
      print(Sp())
    Output
        Sp() 
        n_comp: NULL
             s: 1
         along: NULL

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

---

    Code
      print(SVD_RW(HMD))
    Output
        SVD_RW(HMD) 
          ssvd: HMD
        n_comp: 3
             s: 1

---

    Code
      print(SVD_RW(HMD, indep = FALSE))
    Output
        SVD_RW(HMD,indep=FALSE) 
          ssvd: HMD
        n_comp: 3
         indep: FALSE
             s: 1

---

    Code
      print(SVD_RW2(HMD))
    Output
        SVD_RW2(HMD) 
          ssvd: HMD
        n_comp: 3
             s: 1

---

    Code
      print(SVD_RW2(HMD, indep = FALSE))
    Output
        SVD_RW2(HMD,indep=FALSE) 
          ssvd: HMD
        n_comp: 3
         indep: FALSE
             s: 1

