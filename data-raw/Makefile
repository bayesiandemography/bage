

.PHONY: all
all: ../data/mx_hmd.rda ../data/lfpr_oecd.rda

../data/mx_hmd.rda: mx_hmd.R \
  fltper_5x1 mltper_5x1
	Rscript $<

../data/lfpr_oecd.rda: lfpr_oecd.R \
  LFS_SEXAGE_I_R_12102022025744175.csv
	Rscript $<
