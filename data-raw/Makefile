

.PHONY: all
all: ../data/deaths.rda \
     ../data/divorces.rda \
     ../data/expenditure.rda \
     ../data/households.rda \
     ../data/HMD.rda \
     documentation


../data/deaths.rda: deaths/deaths.R \
  deaths/MAN02008_20230712-132130.csv.zip \
  deaths/MAN05221_20230712-131721.csv.zip
	Rscript $^ $@

divorces/divorces_raw.rds: divorces/divorces_raw.R \
  divorces/VSM480501_20230322_102557_40.csv.gz
	Rscript $^ $@

divorces/popn_raw.rds: divorces/popn_raw.R \
  divorces/DPE403905_20230322_103055_56.csv.gz
	Rscript $^ $@

../data/divorces.rda: divorces/divorces.R \
  divorces/divorces_raw.rds \
  divorces/popn_raw.rds
	Rscript $^ $@
	rm divorces/divorces_raw.rds
	rm divorces/popn_raw.rds

../data/expenditure.rda: expenditure/expenditure.R \
  expenditure/EBDAG_24052016055802288.csv.gz \
  expenditure/POP_PROJ_04062016225548685.csv.gz
	Rscript $^ $@

../data/households.rda: households/households.R \
  households/TABLECODE8420_Data.csv.gz
	Rscript $^ $@


../data/HMD.rda: svd_hmd/HMD.R \
  svd_hmd/HMD_20221129.rds
	Rscript $^ $@

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
