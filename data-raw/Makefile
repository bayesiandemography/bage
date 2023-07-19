

.PHONY: all
all: out/deaths.rds


../data/households.rda: households/households.R \
  households/TABLECODE8420_Data.csv.gz
	Rscript $<

../data/expenditure.rda: expenditure/expenditure.R \
  expenditure/EBDAG_24052016055802288.csv.gz \
  expenditure/POP_PROJ_04062016225548685.csv.gz
	Rscript $<

../data/HMD.rda: svd_hmd/HMD.R \
  svd_hmd/HMD_20221129.rds
	Rscript $^ $@

../data/deaths.rda: deaths/deaths.R \
  deaths/MAN02008_20230712-132130.csv.zip \
  deaths/MAN05221_20230712-131721.csv.zip
	Rscript $^ $@


.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"
