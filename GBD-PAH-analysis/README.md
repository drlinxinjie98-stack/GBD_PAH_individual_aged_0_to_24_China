# Pulmonary arterial hypertension burden analysis in China

This repository contains the R scripts and geographic support files used to analyze temporal and geographic patterns in pulmonary arterial hypertension burden in China using Global Burden of Disease data and related socioeconomic indicators.

## Repository structure

```text
GBD-PAH-analysis/
├── README.md
├── package_versions.tsv
├── assets/
│   ├── China.json
│   ├── Map_china.json
│   └── china_adj.graph
└── scripts/
    ├── 01_awr_eapc.R
    ├── 02_asr_eapc.R
    ├── 03_main_figures.R
    └── 04_explanatory_analysis.R
```

## Analysis scripts

| Script | Purpose |
|---|---|
| `01_awr_eapc.R` | Calculation and summarization of age-weighted rates and estimated annual percentage changes |
| `02_asr_eapc.R` | Calculation of age-standardized rates, case-fatality rates, EAPCs, and age-group sensitivity analyses |
| `03_main_figures.R` | Geographic maps, temporal trends, decomposition analyses, HDI-related analyses, and main figures |
| `04_explanatory_analysis.R` | Exploratory socioeconomic analyses, spatial adjacency construction, Bayesian spatiotemporal models, and HDI/CADI correlation analyses |

## Geographic assets

- `China.json` and `Map_china.json` contain geographic boundary information used by the mapping workflow.
- `china_adj.graph` contains the 33-area adjacency structure used by the spatial models.
- Chinese geographic names retained in the scripts are functional matching keys required to join the statistical and geographic data; they are not personal information.

Users should verify that their intended use and redistribution of geographic boundary files comply with the terms of the original data source.

## Input data

The input datasets are not distributed with this repository. Users must obtain the relevant GBD estimates, population data, HDI component data, and other covariates from their respective authorized sources. The scripts use project-relative paths under `data_2023/` and write derived results beneath `data_2023/` or `Figure/`.

Run the scripts from the repository root so that the relative paths resolve correctly. Required input file names and subdirectories are specified directly in each script.

## Software environment

The code was prepared under R 4.5.2. Versions of the directly used R packages are listed in `package_versions.tsv`. The INLA package is distributed through its own repository and may require installation instructions from the R-INLA project.

## Reproducibility and privacy

- Run the scripts in numerical order for the complete workflow.
- Review input and output paths before execution and create the required output directories.
- No passwords, access tokens, personal file-system paths, or individual-level participant data are included.
- Do not commit restricted input data, derived record-level data, credentials, local R workspaces, or unreviewed output files.

## Code availability

The analysis code supporting this study is provided in this repository. Access to the underlying datasets is governed separately by the relevant data custodians and is not provided through this repository.

