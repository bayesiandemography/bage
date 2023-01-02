

.PHONY: all
all: ../data/injuries.rda \
     documentation

../data/injuries.rda: injuries/injuries.R \
  injuries/DPE403903_20230101_021709_80.csv.gz \
  injuries/DPE479901_20230101_021445_73.csv.gz \
  injuries/TABLECODE7935_Data.csv.gz
	Rscript $<

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"
