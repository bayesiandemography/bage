

.PHONY: all
all: ../data/nz_divorces.rda \
     ../data/nz_population.rda

../data/nz_divorces.rda: nz_divorces.R \
  VSM480501_20230322_102557_40.csv
	Rscript $<

../data/nz_population.rda: nz_population.R \
  DPE403905_20230322_103055_56.csv
	Rscript $<


## Clean

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
