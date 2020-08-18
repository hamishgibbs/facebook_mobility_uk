facebook_mobility_uk
==============================

Mobility analysis in the UK.

This project structure is based on the `cookiecutterdatascience` default data science project. Some changes have been made to the default directory structure.

## Contributions

When making contributions to a new component of the analysis, please create a new subdirectory in `src/analysis`, `src/visualization`, and `src/data`. A basic summary of the project structure is included below.

Feel free to contribute targets to the Makefile for routine data processing tasks.

## Project Organization

    ├── LICENSE
    ├── Makefile           <- Makefile with default data processing targets
    ├── README.md          
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final data sets.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── docs               <- A default Sphinx project; see sphinx-doc.org for details
    │
    ├── models             <- Trained models (not applicable for this study).
    │
    ├── notebooks          <- Jupyter notebooks.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    │
    ├── src                <- Source code for use in this project.
    │   ├── data           <- Scripts to download or generate data
    │   │
    │   ├── analysis       <- Scripts to conduct analysis, subdirectories for each component
    │   │
    │   ├── visualization  <- Scripts to generate data visualisations (output in reports/figures)
    │   │
    │   ├── utils          <- Basic dataset relevant utilties
    │   
    └── tox.ini            <- tox file with settings for running tox; see tox.readthedocs.io


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
