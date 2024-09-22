

.PHONY: all
all: ../data/deaths.rda \
     ../data/divorces.rda \
     ../data/expenditure.rda \
     ../data/households.rda \
     ../data/injuries.rda \
     ../data/us_acc_deaths.rda \
     ../data/HFD.rda \
     ../data/HMD.rda \
     ../data/LFP.rda \
	documentation


## Data frames

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

.INTERMEDIATE: divorces/divorces_raw.rds
.INTERMEDIATE: divorces/popn_raw.rds
../data/divorces.rda: divorces/divorces.R \
  divorces/divorces_raw.rds \
  divorces/popn_raw.rds
	Rscript $^ $@

../data/expenditure.rda: expenditure/expenditure.R \
  expenditure/EBDAG_24052016055802288.csv.gz \
  expenditure/POP_PROJ_04062016225548685.csv.gz
	Rscript $^ $@

../data/households.rda: households/households.R \
  households/TABLECODE8420_Data.csv.gz
	Rscript $^ $@

../data/injuries.rda: injuries/injuries.R \
  injuries/TABLECODE7935_Data.csv.gz \
  injuries/DPE479901_20230101_021445_73.csv.gz \
  injuries/DPE403903_20230101_021709_80.csv.gz
	Rscript $^ $@

../data/us_acc_deaths.rda: us_acc_deaths/us_acc_deaths.R
	Rscript $^ $@


## 'bage_ssvd' objects

../data/HFD.rda: ssvd_hfd/HFD.R \
  ssvd_hfd/asfrRR.txt.zip
	Rscript $^ $@

../data/HMD.rda: ssvd_hmd/HMD.R \
  ssvd_hmd/hmd_statistics_20240226.zip
	Rscript $^ $@

../data/LFP.rda: ssvd_lfp/LFP.R
	Rscript $^ $@


## update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
