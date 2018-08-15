all: data cv analysis fig
data: data.Rout
cv: output/ffcv.Rout output/rfcv.Rout
analysis: output/main.Rout
fig: figures/varimp.Rout tables/perf.Rout figures/screen.Rout

## Make subdirectories
MKDIR_P := mkdir -p
OUT_DIR := figures output tables ## data
.PHONY: subdirs $(OUT_DIR)
$(OUT_DIR):
	${MKDIR_P} $(OUT_DIR)

## Created by make data
data_output = data/turnout.Rda
## Created by make analysis
main_output = output/turn.logit.Rda \
output/turn.cart.Rda \
output/turn.rf.Rda \
output/turn.ff.Rda \
output/turn.logit_robust.Rda \
output/turn.cart_robust.Rda \
output/turn.rf_robust.Rda \
output/turn.ff_robust.Rda

## Individual scripts
data.Rout: fuzzy_forest_utility.R
	Rscript data.R
output/rfcv.Rout: $(data_output)
	@mkdir -p $(@D)
	Rscript rfcv.R
output/ffcv.Rout: $(data_output)
	Rscript ffcv.R
output/main.Rout: $(data_output)
	Rscript main.R
figures/varimp.Rout: $(data_output) $(main_output)
	Rscript varimp.R
tables/perf.Rout: $(data_output) $(main_output)
	Rscript perf.R
figures/screen.Rout: $(data_output) $(outsample_output)
	Rscript screen.R

## If zipping of the data file or the Rda outputs desired
data.zip:
	zip $@ data/*.tab
rep.zip:
	zip $@ output/*.Rda

## Clean directory of R outputs
clean_all:
	find . | egrep ".*((\.(Rda|RData|Rout|Rhistory))|~)$$" | xargs rm
	rm -rf auto
