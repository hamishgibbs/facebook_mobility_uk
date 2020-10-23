$(PROJECT_DIR).PHONY: clean data lint requirements sync_data_to_s3 sync_data_from_s3

#################################################################################
# GLOBALS                                                                       #
#################################################################################

PROJECT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
BUCKET = [OPTIONAL] your-bucket-for-syncing-data (do not include 's3://')
PROFILE = default
PROJECT_NAME = facebook_mobility_uk
PYTHON_INTERPRETER = python3
R_INTERPRETER = /usr/local/bin/Rscript

ifeq (,$(shell which conda))
HAS_CONDA=False
else
HAS_CONDA=True
endif

#################################################################################
# COMMANDS                                                                      #
#################################################################################

## Install Python Dependencies
requirements: test_environment
	$(PYTHON_INTERPRETER) -m pip install -U pip setuptools wheel
	$(PYTHON_INTERPRETER) -m pip install -r requirements.txt

## Make Dataset
processTileMovement: TileMovement_hours TileMovement_days TileMovement_days_norm

TileMovement_hours: $(PROJECT_DIR)/data/interim/mobility_hours.csv

TileMovement_days: $(PROJECT_DIR)/data/interim/mobility_days.csv

TileMovement_days_norm: $(PROJECT_DIR)/data/processed/mobility_days_norm.csv

$(PROJECT_DIR)/data/interim/mobility_hours.csv: $(PROJECT_DIR)/src/data/TileMovement/TileMovement_hours.py $(PROJECT_DIR)/data/raw/Britain_TileMovement
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/interim/mobility_days.csv: $(PROJECT_DIR)/src/data/TileMovement/TileMovement_days.py $(PROJECT_DIR)/data/interim/mobility_hours.csv $(PROJECT_DIR)/data/raw/old_mobility/old_mobility.csv
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/processed/mobility_days_norm.csv: $(PROJECT_DIR)/src/data/TileMovement/fb_user_prop.py $(PROJECT_DIR)/data/interim/mobility_days.csv $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv
	$(PYTHON_INTERPRETER) $^ $@

processAdminMovement: AdminMovement_hours TileMovement_days

AdminMovement_hours: $(PROJECT_DIR)/data/interim/admin_mobility_hours.csv

TileMovement_days: $(PROJECT_DIR)/data/interim/admin_mobility_days.csv

$(PROJECT_DIR)/data/interim/admin_mobility_hours.csv: $(PROJECT_DIR)/src/data/AdminMovement/AdminMovement_hours.py $(PROJECT_DIR)/data/raw/Britain_AdminMovement
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/interim/admin_mobility_days.csv: $(PROJECT_DIR)/src/data/AdminMovement/AdminMovement_days.py $(PROJECT_DIR)/data/interim/admin_mobility_hours.csv
	$(PYTHON_INTERPRETER) $^ $@

processTilePopulation: TilePopulation_hours TilePopulation_days

TilePopulation_hours: $(PROJECT_DIR)/data/interim/population_hours.csv

TilePopulation_days: $(PROJECT_DIR)/data/interim/population_days.csv

$(PROJECT_DIR)/data/interim/population_hours.csv: $(PROJECT_DIR)/src/data/TilePopulation/TilePopulation_hours.py $(PROJECT_DIR)/data/raw/Britain_TilePopulation
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/interim/population_days.csv: $(PROJECT_DIR)/src/data/TilePopulation/TilePopulation_days.py $(PROJECT_DIR)/data/interim/population_hours.csv
	$(PYTHON_INTERPRETER) $^ $@

TileReference: tiles_zoom_12 tiles_zoom_13

tiles_zoom_12: $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp

tiles_zoom_13: $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_13.shp

$(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp: $(PROJECT_DIR)/src/data/TileMovement/TileMovement_reference.py $(PROJECT_DIR)/data/interim/mobility_days.csv
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_13.shp: $(PROJECT_DIR)/src/data/TilePopulation/TilePopulation_reference.py $(PROJECT_DIR)/data/interim/population_days.csv
	$(PYTHON_INTERPRETER) $^ $@

tile_Reference: $(PROJECT_DIR)/data/processed/oa_reference/oa_tile_reference.csv  \
	$(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv \
	$(PROJECT_DIR)/data/processed/la_reference/la_tile_reference.csv \
	$(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv

$(PROJECT_DIR)/data/processed/oa_reference/oa_tile_reference.csv: $(PROJECT_DIR)/src/data/OA_Reference/OA_to_tiles.py \
																																	$(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_13.shp \
																																	$(PROJECT_DIR)/data/raw/OA_Population/engwal_oa_bng.shp \
																																	$(PROJECT_DIR)/data/raw/OA_Population/OutputArea2011_PWC.shp \
																																	$(PROJECT_DIR)/data/raw/OA_Population/NI_SA_Centroids.shp
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/processed/la_reference/la_tile_reference.csv: $(PROJECT_DIR)/src/data/OA_Reference/LA_to_tiles.py \
																																	$(PROJECT_DIR)/data/raw/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp \
																																	$(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv: $(PROJECT_DIR)/src/data/OA_Reference/A3_to_tiles.py \
																																	$(PROJECT_DIR)/data/raw/gadm36_GBR_shp/gadm36_GBR_3.shp \
																																	$(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv: $(PROJECT_DIR)/src/data/OA_Reference/tile_oa_population.py \
																															 $(PROJECT_DIR)/data/processed/oa_reference/oa_tile_reference.csv \
																															 $(PROJECT_DIR)/data/raw/OA_Population/Eng_Wal_OA_Mid_Pop.csv \
																															 $(PROJECT_DIR)/data/raw/OA_Population/OA_to_DZ.csv \
																															 $(PROJECT_DIR)/data/raw/OA_Population/simd2020_withinds.csv \
																															 $(PROJECT_DIR)/data/raw/OA_Population/NI_Mid_Pop.csv
	$(PYTHON_INTERPRETER) $^ $@

params: $(PROJECT_DIR)/data/processed/params/date_range.pickle

$(PROJECT_DIR)/data/processed/params/date_range.pickle: $(PROJECT_DIR)/src/data/params/define_time_periods.py \
																									 $(PROJECT_DIR)/data/interim/mobility_days.csv
	$(PYTHON_INTERPRETER) $^ $@

communities: infomap leiden

infomap: $(PROJECT_DIR)/data/processed/infomap/infomap_full.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_full.csv: $(PROJECT_DIR)/src/analysis/infomap/infomap_communities.py \
																												$(PROJECT_DIR)/data/interim/mobility_days.csv \
																												$(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(PYTHON_INTERPRETER) $^ $@

leiden: $(PROJECT_DIR)/data/processed/infomap/leiden_full.csv

$(PROJECT_DIR)/data/processed/infomap/leiden_full.csv: $(PROJECT_DIR)/src/analysis/infomap/leiden_communities.py \
																												$(PROJECT_DIR)/data/interim/mobility_days.csv \
																												$(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(PYTHON_INTERPRETER) $^ $@

label_map: $(PROJECT_DIR)/data/interim/infomap/label_map_test.csv $(PROJECT_DIR)/data/interim/infomap/label_map_leiden_test.csv

$(PROJECT_DIR)/data/interim/infomap/label_map_test.csv: $(PROJECT_DIR)/src/analysis/cluster_comparison/apply_label_map.py \
																												$(PROJECT_DIR)/data/processed/infomap/infomap_full.csv \
																												$(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(PYTHON_INTERPRETER) $^ $@

$(PROJECT_DIR)/data/interim/infomap/label_map_leiden_test.csv: $(PROJECT_DIR)/src/analysis/cluster_comparison/apply_label_map.py \
																															 $(PROJECT_DIR)/data/processed/infomap/leiden_full.csv \
																															 $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(PYTHON_INTERPRETER) $^ $@

matrix_comparison: $(PROJECT_DIR)/data/interim/canberra_distance/c_dist_test.csv

$(PROJECT_DIR)/data/interim/canberra_distance/c_dist_test.csv: $(PROJECT_DIR)/src/analysis/matrix_comparison/matrix_comparison.py  \
																															 $(PROJECT_DIR)/data/interim/mobility_days.csv \
																															 $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv \
																															 $(PROJECT_DIR)/data/processed/infomap/infomap_full.csv
	$(PYTHON_INTERPRETER) $^ $@

# Get population size and area of communities
community_size: $(PROJECT_DIR)/data/processed/communities_descriptive/community_size.csv

$(PROJECT_DIR)/data/processed/communities_descriptive/community_size.csv: $(PROJECT_DIR)/src/analysis/infomap/community_size.R \
																																				 $(PROJECT_DIR)/data/interim/infomap/label_map_test.csv \
																																				 $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
																																				 $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv
	$(R_INTERPRETER) $^ $@

# Get betweneess of all network edges
betweeness: $(PROJECT_DIR)/data/processed/backbone/betweeness.csv

$(PROJECT_DIR)/data/processed/backbone/betweeness.csv: $(PROJECT_DIR)/src/analysis/matrix_comparison/compute_betweeness.R \
																											 $(PROJECT_DIR)/data/interim/mobility_days.csv
	$(R_INTERPRETER) $^ $@

community_sensitivity: $(PROJECT_DIR)/data/processed/communities_descriptive/lei_im_s_to_l.csv

$(PROJECT_DIR)/data/processed/communities_descriptive/lei_im_s_to_l.csv: $(PROJECT_DIR)/src/visualization/infomap/spatial_comparison_data.R \
																	$(PROJECT_DIR)/data/interim/infomap/label_map_test.csv \
										              $(PROJECT_DIR)/data/interim/infomap/label_map_leiden_test.csv \
										              $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv \
										              $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp
	$(R_INTERPRETER) $^ $@

analysis: crisis_baseline_comparison

crisis_baseline_comparison: $(PROJECT_DIR)/data/interim/crisis_baseline_comparison.csv

$(PROJECT_DIR)/data/interim/crisis_baseline_comparison.csv: $(PROJECT_DIR)/src/analysis/crisis_baseline_comparison/crisis_baseline_aggregation.py \
																														$(PROJECT_DIR)/data/processed/params/date_range.pickle \
																														$(PROJECT_DIR)/data/interim/mobility_days.csv
	$(PYTHON_INTERPRETER) $^ $@

data: requirements
	$(PYTHON_INTERPRETER) src/data/make_dataset.py data/raw data/processed

## Create figs
figs: figs_fb_pop figs_canberra_grid figs_comm_method_im

figs_fb_pop: $(PROJECT_DIR)/reports/figures/tile_oa_pop_comparison.png

$(PROJECT_DIR)/reports/figures/tile_oa_pop_comparison.png: $(PROJECT_DIR)/src/visualization/crisis_baseline_comparison/visualize_tile_oa_pop.R \
																													 $(PROJECT_DIR)/data/interim/mobility_hours.csv \
																													 $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv \
																													 $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
																													 $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(R_INTERPRETER) $^ $@

figs_canberra_grid: $(PROJECT_DIR)/reports/figures/matrix_comparison.rds

$(PROJECT_DIR)/reports/figures/matrix_comparison.rds: $(PROJECT_DIR)/src/visualization/matrix_comparison/m_compare_test.R $(PROJECT_DIR)/data/interim/canberra_distance/c_dist_test.csv
	$(R_INTERPRETER) $^ $@

figs_comm_method_im: $(PROJECT_DIR)/reports/figures/comm_method_comparison.png

$(PROJECT_DIR)/reports/figures/comm_method_comparison.png: $(PROJECT_DIR)/src/visualization/infomap/community_method_comparison.R \
																													 $(PROJECT_DIR)/data/interim/infomap/label_map_test.csv \
																													 $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
																													 $(PROJECT_DIR)/data/interim/mobility_days.csv \
																													 $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv \
																													 $(PROJECT_DIR)/data/processed/communities_descriptive/community_size.csv \
																													 $(PROJECT_DIR)/data/processed/communities_descriptive/community_pop.csv
	$(R_INTERPRETER) $^ $@

figs_local_area_multi: $(PROJECT_DIR)/reports/figures/manchester.png

$(PROJECT_DIR)/reports/figures/manchester.png: $(PROJECT_DIR)/src/visualization/local_lockdown/google_comparison_multi.R \
																							 $(PROJECT_DIR)/data/interim/infomap/label_map_test.csv \
																							 $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
																							 $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv \
																							 $(PROJECT_DIR)/data/interim/mobility_days.csv \
																							 /Users/hamishgibbs/Downloads/Local_Authority_Districts__December_2019__Boundaries_UK_BFC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFC.shp \
																							 /Users/hamishgibbs/Downloads/Region_Mobility_Report_CSVs/2020_GB_Region_Mobility_Report.csv
	$(R_INTERPRETER) $^ $@

figs_betweeness: $(PROJECT_DIR)/reports/figures/btw_comparison.png

$(PROJECT_DIR)/reports/figures/btw_comparison.png: $(PROJECT_DIR)/src/visualization/matrix_comparison/betweeness.R \
																									 $(PROJECT_DIR)/data/interim/mobility_days.csv \
																		               $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
																		               $(PROJECT_DIR)/data/interim/journey_lines/journey_lines.shp \
																		               $(PROJECT_DIR)/data/processed/backbone/betweeness.csv
	$(R_INTERPRETER) $^ $@

figs_label_map: $(PROJECT_DIR)/reports/figures/label_map.png

$(PROJECT_DIR)/reports/figures/label_map.png: $(PROJECT_DIR)/src/visualization/infomap/explore_label_map.R \
							$(PROJECT_DIR)/data/interim/infomap/label_map_test.csv \
              $(PROJECT_DIR)/data/interim/infomap/label_map_leiden_test.csv \
              $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
              $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
	$(R_INTERPRETER) $^ $@

figs_spatial_comparison: $(PROJECT_DIR)/reports/figures/spatial_comparison.png

$(PROJECT_DIR)/reports/figures/spatial_comparison.png: $(PROJECT_DIR)/src/visualization/infomap/spatial_comparison_network.R \
							$(PROJECT_DIR)/data/interim/infomap/label_map_test_norm.csv \
              $(PROJECT_DIR)/data/interim/infomap/label_map_leiden_test_norm.csv \
              $(PROJECT_DIR)/data/processed/communities_descriptive/lei_im_s_to_l.csv \
              $(PROJECT_DIR)/data/processed/communities_descriptive/lei_im_l_to_s.csv
	$(R_INTERPRETER) $^ $@

figs_population_change: $(PROJECT_DIR)/reports/figures/holiday_pop.png

$(PROJECT_DIR)/reports/figures/holiday_pop.png: $(PROJECT_DIR)/src/visualization/holiday_travel/holiday_internal_movement.R \
							$(PROJECT_DIR)/data/interim/population_days.csv \
              $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference_13.csv \
              $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv \
              $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv \
              /Users/hamishgibbs/Downloads/gadm36_GBR_shp/gadm36_GBR_3.shp
	$(R_INTERPRETER) $^ $@

figs_qk_imd: $(PROJECT_DIR)/reports/figures/imd_mob.png

$(PROJECT_DIR)/reports/figures/imd_mob.png: $(PROJECT_DIR)/src/visualization/imd/imd_mob.R \
																						$(PROJECT_DIR)/data/interim/mobility_days.csv \
															              $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp \
															              $(PROJECT_DIR)/data/processed/imd_reference/quadkey_imd.csv
	$(R_INTERPRETER) $^ $@

figs_n_tiles: $(PROJECT_DIR)/reports/figures/n_tiles.png

$(PROJECT_DIR)/reports/figures/n_tiles.png: $(PROJECT_DIR)/src/visualization/crisis_baseline_comparison/n_tiles.R \
																						$(PROJECT_DIR)/data/interim/mobility_days.csv \
															              $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv \
															              $(PROJECT_DIR)/reports/figures/tile_oa_pop_comparison.png
	$(R_INTERPRETER) $^ $@

## Delete all compiled Python files
clean:
	find . -type f -name "*.py[co]" -delete
	find . -type d -name "__pycache__" -delete

.PHONY: just
just: ;

## Lint using flake8
lint:
	flake8 src

## Upload Data to S3
sync_data_to_s3:
ifeq (default,$(PROFILE))
	aws s3 sync data/ s3://$(BUCKET)/data/
else
	aws s3 sync data/ s3://$(BUCKET)/data/ --profile $(PROFILE)
endif

## Download Data from S3
sync_data_from_s3:
ifeq (default,$(PROFILE))
	aws s3 sync s3://$(BUCKET)/data/ data/
else
	aws s3 sync s3://$(BUCKET)/data/ data/ --profile $(PROFILE)
endif

## Set up python interpreter environment
create_environment:
ifeq (True,$(HAS_CONDA))
		@echo ">>> Detected conda, creating conda environment."
ifeq (3,$(findstring 3,$(PYTHON_INTERPRETER)))
	conda create --name $(PROJECT_NAME) python=3
else
	conda create --name $(PROJECT_NAME) python=2.7
endif
		@echo ">>> New conda env created. Activate with:\nsource activate $(PROJECT_NAME)"
else
	$(PYTHON_INTERPRETER) -m pip install -q virtualenv virtualenvwrapper
	@echo ">>> Installing virtualenvwrapper if not already installed.\nMake sure the following lines are in shell startup file\n\
	export WORKON_HOME=$$HOME/.virtualenvs\nexport PROJECT_HOME=$$HOME/Devel\nsource /usr/local/bin/virtualenvwrapper.sh\n"
	@bash -c "source `which virtualenvwrapper.sh`;mkvirtualenv $(PROJECT_NAME) --python=$(PYTHON_INTERPRETER)"
	@echo ">>> New virtualenv created. Activate with:\nworkon $(PROJECT_NAME)"
endif

## Test python environment is setup correctly
test_environment:
	$(PYTHON_INTERPRETER) test_environment.py

#################################################################################
# PROJECT RULES                                                                 #
#################################################################################



#################################################################################
# Self Documenting Commands                                                     #
#################################################################################

.DEFAULT_GOAL := help

# Inspired by <http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html>
# sed script explained:
# /^##/:
# 	* save line in hold space
# 	* purge line
# 	* Loop:
# 		* append newline + line to hold space
# 		* go to next line
# 		* if line starts with doc comment, strip comment character off and loop
# 	* remove target prerequisites
# 	* append hold space (+ newline) to line
# 	* replace newline plus comments by `---`
# 	* print line
# Separate expressions are necessary because labels cannot be delimited by
# semicolon; see <http://stackoverflow.com/a/11799865/1968>
.PHONY: help
help:
	@echo "$$(tput bold)Available rules:$$(tput sgr0)"
	@echo
	@sed -n -e "/^## / { \
		h; \
		s/.*//; \
		:doc" \
		-e "H; \
		n; \
		s/^## //; \
		t doc" \
		-e "s/:.*//; \
		G; \
		s/\\n## /---/; \
		s/\\n/ /g; \
		p; \
	}" ${MAKEFILE_LIST} \
	| LC_ALL='C' sort --ignore-case \
	| awk -F '---' \
		-v ncol=$$(tput cols) \
		-v indent=19 \
		-v col_on="$$(tput setaf 6)" \
		-v col_off="$$(tput sgr0)" \
	'{ \
		printf "%s%*s%s ", col_on, -indent, $$1, col_off; \
		n = split($$2, words, " "); \
		line_length = ncol - indent; \
		for (i = 1; i <= n; i++) { \
			line_length -= length(words[i]) + 1; \
			if (line_length <= 0) { \
				line_length = ncol - indent - length(words[i]) - 1; \
				printf "\n%*s ", -indent, " "; \
			} \
			printf "%s ", words[i]; \
		} \
		printf "\n"; \
	}' \
	| more $(shell test $(shell uname) = Darwin && echo '--no-init --raw-control-chars')
