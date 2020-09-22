#What number should floats be multiplied by?
#You could discretize the weights by multiplying by some large constant,
#but you have to do this carefully, since the results will depend on your
#quantization, which will arbitrarily change the density of the graph...
#https://nabble.skewed.de/minimize-nested-blockmodel-dl-performance-td4025438.html#a4025439


#Try non-hierarchical with correct edge weight daily - not much is coming from hierarchical?

PROJDIR := /Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example
PYTHON_INTERPRETER = ~/anaconda3/bin/python $^ $@
R_INTERPRETER = /usr/local/bin/Rscript $^ $@

default: sbm plot

sbm: $(PROJDIR)/processed/sbm.csv

$(PROJDIR)/processed/sbm.csv: $(PROJDIR)/sbm_communities.py $(PROJDIR)/data/movement_2020_03_2020.csv
	$(PYTHON_INTERPRETER)

plot: $(PROJDIR)/output/sbm.png

$(PROJDIR)/output/sbm.png: $(PROJDIR)/plot.R $(PROJDIR)/processed/sbm.csv $(PROJDIR)/data/tiles_zoom_12.shp
	$(R_INTERPRETER)
