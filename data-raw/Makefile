
.PHONY: all
all: ../data/isl_deaths.rda \
     ../data/kor_births.rda \
     ../data/nzl_divorces.rda \
     ../data/nld_expenditure.rda \
     ../data/nzl_households.rda \
     ../data/nzl_injuries.rda \
     ../data/usa_deaths.rda \
     ../data/HFD.rda \
     ../data/HMD.rda \
     ../data/LFP.rda \
	documentation


## Data frames

../data/isl_deaths.rda: isl_deaths/isl_deaths.R \
  isl_deaths/MAN02008_20230712-132130.csv.zip \
  isl_deaths/MAN05221_20230712-131721.csv.zip
	Rscript $^ $@

../data/kor_births.rda: kor_births/kor_births.R \
  kor_births/101_DT_1B81A12_20240924103050.csv \
  kor_births/Resident_Population_in_FiveYear_Age_Groups__2011_._20240924104155.csv
	Rscript $^ $@

nzl_divorces/divorces_raw.rds: nzl_divorces/divorces_raw.R \
  nzl_divorces/VSM480501_20230322_102557_40.csv.gz
	Rscript $^ $@

nzl_divorces/popn_raw.rds: nzl_divorces/popn_raw.R \
  nzl_divorces/DPE403905_20230322_103055_56.csv.gz
	Rscript $^ $@

.INTERMEDIATE: nzl_divorces/divorces_raw.rds
.INTERMEDIATE: nzl_divorces/popn_raw.rds
../data/nzl_divorces.rda: nzl_divorces/nzl_divorces.R \
  nzl_divorces/divorces_raw.rds \
  nzl_divorces/popn_raw.rds
	Rscript $^ $@

../data/nld_expenditure.rda: nld_expenditure/nld_expenditure.R \
  nld_expenditure/EBDAG_24052016055802288.csv.gz \
  nld_expenditure/POP_PROJ_04062016225548685.csv.gz
	Rscript $^ $@

../data/nzl_households.rda: nzl_households/nzl_households.R \
  nzl_households/TABLECODE8420_Data.csv.gz
	Rscript $^ $@

../data/nzl_injuries.rda: nzl_injuries/nzl_injuries.R \
  nzl_injuries/TABLECODE7935_Data.csv.gz \
  nzl_injuries/DPE479901_20230101_021445_73.csv.gz \
  nzl_injuries/DPE403903_20230101_021709_80.csv.gz
	Rscript $^ $@

../data/swe_infant.rda: swe_infant/swe_infant.R \
   swe_infant/BE0101E2.csv \
   swe_infant/BE0101D9.csv
	Rscript $^ $@

../data/usa_deaths.rda: usa_deaths/usa_deaths.R
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
