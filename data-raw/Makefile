
.PHONY: all
all: ../data/isl_deaths.rda \
     ../data/kor_births.rda \
     ../data/nzl_divorces.rda \
     ../data/nld_expenditure.rda \
     ../data/nzl_households.rda \
     ../data/nzl_injuries.rda \
     ../data/prt_deaths.rda \
     ../data/usa_deaths.rda \
     ../data/CSA.rda \
     ../data/HFD.rda \
     ../data/HIMD_P1.rda \
     ../data/HIMD_P5.rda \
     ../data/HIMD_R.rda \
     ../data/HMD.rda \
     ../data/LFP.rda \
     ../data/WMD_C.rda \
     ../data/WMD_E.rda \
	documentation


## Data frames

../data/isl_deaths.rda: isl_deaths/isl_deaths.R \
  isl_deaths/MAN02008_20230712-132130.csv.zip \
  isl_deaths/MAN05221_20230712-131721.csv.zip
	Rscript $^ $@

../data/kor_births.rda: kor_births/kor_births.R \
  kor_births/101_DT_1B81A12_20240924103050.csv.gz \
  kor_births/Resident_Population_in_FiveYear_Age_Groups__2011_._20240924104155.csv.gz \
  kor_births/List_of_South_Korean_regions_by_GDP_2.csv.gz \
  kor_births/Administrative_divisions_of_South_Korea_2.csv.gz
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

../data/prt_deaths.rda: prt_deaths/prt_deaths.R \
  prt_deaths/Deaths_1x1.txt.gz \
  prt_deaths/PRT.Exposures_1x1.txt.gz
	Rscript $^ $@

../data/swe_infant.rda: swe_infant/swe_infant.R \
   swe_infant/BE0101E2.csv.gz \
   swe_infant/BE0101D9.csv.gz
	Rscript $^ $@

../data/usa_deaths.rda: usa_deaths/usa_deaths.R
	Rscript $^ $@


## 'bage_ssvd' objects

../data/CSA.rda: ssvd_csa/CSA.R \
  ssvd_csa/UNdata_Export_20251022_221132488.csv.zip \
  ssvd_csa/UNdata_Export_20251022_221508651.csv.zip
	Rscript $^ $@

../data/HFD.rda: ssvd_hfd/HFD.R \
  ssvd_hfd/asfrRR_20240523.txt.zip \
  ssvd_hfd/asfrRR_20250724.txt.zip
	Rscript $^ $@

../data/HIMD_P1.rda: ssvd_himd/HIMD_P1.R \
  ssvd_himd/himd_20241023.zip
	Rscript $^ $@

../data/HIMD_P5.rda: ssvd_himd/HIMD_P5.R \
  ssvd_himd/himd_20241023.zip
	Rscript $^ $@

../data/HIMD_R.rda: ssvd_himd/HIMD_R.R \
  ssvd_himd/himd_20241023.zip
	Rscript $^ $@

../data/HMD.rda: ssvd_hmd/HMD.R \
  ssvd_hmd/hmd_statistics_20250925.zip \
  ssvd_hmd/hmd_statistics_20240226.zip
	Rscript $^ $@

../data/LFP.rda: ssvd_lfp/LFP.R \
  ssvd_lfp/lfp_data_2025-10-17.zip
	Rscript $^ $@

../data/WMD_C.rda: ssvd_wmd/WMD_C.R \
  ssvd_wmd/undesa_pd_2019_wmd_marital_status.xlsx
	Rscript $^ $@

../data/WMD_E.rda: ssvd_wmd/WMD_E.R \
  ssvd_wmd/undesa_pd_2019_wmd_marital_status.xlsx
	Rscript $^ $@


## update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
