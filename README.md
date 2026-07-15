# Knowledge, attitudes and practices regarding vector-borne diseases

This repository reproduces the principal figures and the numbered descriptive tables for the associated scientific article.

**Publication details:** `[PLACEHOLDER: full citation, DOI, journal, and manuscript version]`

The pipeline is deliberately organized around one entry point:

```r
source("Main.R")
```

Running `Main.R` prepares the two survey datasets, applies the BeReady analysis-sample criteria, performs the model-based analyses, and writes publication outputs to `output/`.

## Repository structure

```text
.
├── Main.R
├── R/
│   ├── utils.R
│   ├── data.R
│   ├── analysis.R
│   ├── figures.R
│   └── tables.R
├── data/
│   ├── raw/
│   │   └── README.md
│   ├── external/
│   │   ├── README.md
│   │   ├── Gemeindestand.xlsx
│   │   ├── HCL_CH_ISCO_19_PROF_level_1.xlsx
│   │   └── Raumgliederungen.xlsx
│   └── processed/
│       └── README.md
├── output/
│   ├── README.md
│   ├── figures/
│   │   └── README.md
│   └── tables/
│       └── README.md
├── .gitignore
├── README.md
└── VBD_KAP_surveys.Rproj
```

## Script responsibilities

`Main.R` defines paths and analysis settings, validates inputs, and runs the complete pipeline.

`R/data.R` imports the raw exports and the three downloaded external workbooks, joins municipality, occupation, and topography information, applies recodes, derives knowledge variables, and creates the primary BeReady analysis sample.

`R/analysis.R` performs chained XGBoost imputation, fits the two-parameter logistic IRT model, fits the multivariable knowledge-score regression, and performs block-weighted Gower/PAM clustering.

`R/figures.R` constructs Figures 1–4. Plot functions return plot objects; file export is handled centrally.

`R/tables.R` constructs the main participant-characteristics table, Supplementary Tables S1.1–S1.14, and Supplementary Tables S2.1–S2.4 as long-format CSV files.

`R/utils.R` contains shared validation, import-cleaning, export, and reporting helpers.

## Software requirements

The scripts require R 4.2 or later and the following packages:

```r
install.packages(c(
  "broom", "cluster", "dplyr", "forcats", "ggplot2", "ggrepel",
  "mirt", "patchwork", "purrr", "readr", "readxl", "scales",
  "stringr", "tibble", "tidyr", "xgboost"
))
```

## Required input files

Raw and restricted data are intentionally excluded from version control. Place these files under `data/raw/` or edit their paths at the top of `Main.R`:

```text
1951BEreadyHauptstud-VectorborneDiseases_DATA_2026-03-05_1544.csv
NCCSimpactsStakehold_DATA_2025-06-26_0710.csv
NCCSimpactsStakehold_DATA_2025-06-26_0710_SUPP.csv
```

The pipeline also reads these downloaded workbooks directly from `data/external/`:

```text
Gemeindestand.xlsx
HCL_CH_ISCO_19_PROF_level_1.xlsx
Raumgliederungen.xlsx
```

They provide, respectively:

- municipality names and canton abbreviations by BFS municipality code;
- English labels for CH-ISCO 2019 level-1 occupation codes;
- the BFS 2020 urban/intermediate/rural municipality classification.

No manual conversion to CSV is required. Exact worksheet names, required columns, classification codes, and data-documentation placeholders are given in `data/external/README.md`.

### Data documentation placeholders

Complete the following before publishing the repository:

- `[PLACEHOLDER: provenance and version of the BeReady REDCap export]`
- `[PLACEHOLDER: dates covered by the BeReady analysis extract]`
- `[PLACEHOLDER: provenance and version of the stakeholder REDCap export]`
- `[PLACEHOLDER: explanation of the stakeholder supplementary file, including type and canton fields]`
- `[PLACEHOLDER: source URLs, versions, download dates, and licences for the three external workbooks]`
- `[PLACEHOLDER: justification for the municipal-status and spatial-classification reference dates]`
- `[PLACEHOLDER: data-access procedure and contact]`
- `[PLACEHOLDER: data-use restrictions and licence]`
- `[PLACEHOLDER: ethics approvals and consent statement]`
- `[PLACEHOLDER: link or location of the questionnaire and data dictionary]`
- `[PLACEHOLDER: de-identification procedure, especially for open-text responses]`

## Outputs

The pipeline creates four figures under `output/figures/`:

```text
figure1_questionnaire_responses.png
figure2_irt_results.png
figure3_knowledge_determinants.png
figure4_cluster_profiles.png
```

Set `save_pdf = TRUE` in `Main.R` to save a PDF copy of each figure as well.

The pipeline creates 19 numbered manuscript and supplementary CSV tables under `output/tables/`:

```text
table_1_participant_characteristics.csv
table_s1_01_sociodemography.csv
...
table_s1_14_risk_and_control_perception.csv
table_s2_01_involvement_and_resources.csv
...
table_s2_04_collaboration_and_coordination.csv
```

The CSV files use a long, analysis-friendly representation. Counts, denominators, percentages, missing counts, means, and standard deviations are stored in separate columns rather than embedded in formatted manuscript strings.

Generated intermediate datasets are written to `data/processed/`. A session-information file is written to `output/sessionInfo.txt`.

## Analysis sample

The primary BeReady analysis sample retains respondents who:

1. classified tick-borne encephalitis as a vector-borne disease after the introductory definition; and
2. showed no logical contradiction between vector-recognition and disease–vector matching responses.

All-respondent and primary-analysis datasets are both retained because the supplementary descriptive tables compare these samples.

## Reproducibility notes

The script does not install packages or download data. It reads the three external Excel workbooks in their downloaded layouts and stops with an explicit message when a required package, file, worksheet, or column is absent.

Open-text outputs contain aggregated verbatim responses. They must be reviewed for potentially identifying information before public release.

The statistical analyses are exploratory and reproduce the analysis framework of the associated manuscript. Any material change to recoding rules, imputation settings, IRT items, regression predictors, cluster blocks, or the fixed number of clusters should be documented in the repository history.
