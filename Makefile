.PHONY: clean data lint requirements sync_data_to_s3 sync_data_from_s3

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

$(PROJECT_DIR)/data/interim/mobility_days.csv: $(PROJECT_DIR)/src/data/TileMovement/TileMovement_days.py $(PROJECT_DIR)/data/interim/mobility_hours.csv
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

analysis: crisis_baseline_comparison

crisis_baseline_comparison: $(PROJECT_DIR)/data/interim/crisis_baseline_comparison.csv

$(PROJECT_DIR)/data/interim/crisis_baseline_comparison.csv: $(PROJECT_DIR)/src/analysis/crisis_baseline_comparison/crisis_baseline_aggregation.py \
																														$(PROJECT_DIR)/data/processed/params/date_range.pickle \
																														$(PROJECT_DIR)/data/interim/mobility_days.csv
	$(PYTHON_INTERPRETER) $^ $@

infomap: $(PROJECT_DIR)/data/processed/infomap/infomap_full.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_full.csv: $(PROJECT_DIR)/src/analysis/infomap/infomap.py $(PROJECT_DIR)/data/interim/mobility_days.csv
	$(PYTHON_INTERPRETER) $^ $@

data: requirements
	$(PYTHON_INTERPRETER) src/data/make_dataset.py data/raw data/processed

## Create figs
figs: fb_pop

fb_pop: $(PROJECT_DIR)/reports/figures/tile_oa_pop_comparison.png

$(PROJECT_DIR)/reports/figures/tile_oa_pop_comparison.png: $(PROJECT_DIR)/src/visualization/crisis_baseline_comparison/visualize_tile_oa_pop.R $(PROJECT_DIR)/data/interim/mobility_hours.csv $(PROJECT_DIR)/data/processed/oa_reference/tile_12_oa_pop.csv $(PROJECT_DIR)/data/processed/tile_reference/tiles_zoom_12.shp $(PROJECT_DIR)/data/processed/la_reference/a3_tile_reference.csv
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
