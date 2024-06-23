# 'print' works

    Code
      print(AR())
    Output
        AR(n=2) 
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
      print(LinAR())
    Output
        LinAR() 
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
      print(RWSeas(n = 2, s_seas = 0))
    Output
        RWSeas(n=2,s_seas=0) 
             n: 2
             s: 1
        s_seas: 0
         along: NULL

---

    Code
      print(RWSeas(n = 2))
    Output
        RWSeas(n=2) 
             n: 2
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
      print(RW2Seas(n = 2, s_seas = 0))
    Output
        RW2Seas(n=2,s_seas=0) 
             n: 2
             s: 1
        s_seas: 0
         along: NULL

---

    Code
      print(RW2Seas(n = 2))
    Output
        RW2Seas(n=2) 
             n: 2
             s: 1
        s_seas: 1
         along: NULL

---

    Code
      print(Sp())
    Output
        Sp() 
             n: NULL
             s: 1
         along: NULL

---

    Code
      print(SVD(HMD))
    Output
        SVD(HMD) 
          ssvd: HMD
             n: 5

---

    Code
      print(SVDS(HMD))
    Output
        SVDS(HMD) 
          ssvd: HMD
             n: 5
         joint: FALSE

