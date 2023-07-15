

.PHONY: all
all: ../data/deaths.rda \
     ../data/injuries.rda \
     ../data/households.rda \
     ../data/expenditure.rda \
     documentation

../data/injuries.rda: injuries/injuries.R \
  injuries/DPE403903_20230101_021709_80.csv.gz \
  injuries/DPE479901_20230101_021445_73.csv.gz \
  injuries/TABLECODE7935_Data.csv.gz
	Rscript $<

../data/households.rda: households/households.R \
  households/TABLECODE8420_Data.csv.gz
	Rscript $<

../data/expenditure.rda: expenditure/expenditure.R \
  expenditure/EBDAG_24052016055802288.csv.gz \
  expenditure/POP_PROJ_04062016225548685.csv.gz
	Rscript $<

../data/HMD.rda: svd_hmd/HMD.R \
  svd_hmd/hmd_20221129.rds
	Rscript $^ $@

../data/deaths.rda: deaths/deaths.R \
  deaths/MAN02008_20230712-132130.csv.zip \
  deaths/MAN05221_20230712-131721.csv.zip
	Rscript $^ $@


.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"
