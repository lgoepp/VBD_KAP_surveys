# Analysis code for the Swiss vector-borne disease KAP surveys

This repository contains the analysis code accompanying the manuscript **“Knowledge, attitudes, practices regarding vector-borne diseases among adults in Switzerland: a cross-sectional survey”** by Lilian Goepp, Eva Maria Hodel, Arlette Szelecsenyi, Nina Huber, Ana Maria Vicedo-Cabrera, Ioannis Magouras, and Julien Riou.

The repository is a transparent record of the data preparation and statistical methods used in the study. The participant-level survey data are governed by the BEready cohort and the Swiss National Centre for Climate Services (NCCS) and are not distributed through the public repository. Exact re-execution requires authorized copies of the original REDCap exports and the external Swiss reference files.

## Study components

The analysis combines two cross-sectional surveys conducted in 2025.

1. **General-population survey.** A vector-borne disease module was administered within the BEready cohort in the canton of Bern. It measured knowledge of vector-borne diseases and vectors, perceived disease relevance, self-reported tick and mosquito exposure, preventive practices, and views on vector-control measures. Of 1,847 adult respondents, 1,337 met the predefined validity criteria and formed the primary analysis sample.

2. **Stakeholder survey.** A national survey collected information on institutional engagement, activities, resources, coordination, and information needs from cantonal authorities in human health, animal health, and environmental services. The analysis includes 55 complete responses covering all 26 Swiss cantons and Liechtenstein.

The population survey constitutes the main analytical component of the study. The stakeholder survey provides complementary institutional context.

## Statistical analysis

### Analysis sample and descriptive summaries

The population-survey code first recodes the REDCap export and derives binary indicators for correct knowledge responses. Two predefined checks define the primary analysis sample:

- respondents must classify tick-borne encephalitis as vector-borne after being given the introductory definition; and
- responses must contain no logical contradiction between recognition of ticks or mosquitoes as vectors and subsequent disease-vector matching answers.

Descriptive tables and Figure 1 use observed responses. Missing values are retained explicitly where relevant. The supplementary population tables report both the full respondent set and the primary analysis sample where applicable.

### Completion of missing analytical variables

Variables required for the model-based analyses are completed after the validity filters have been applied. The implementation in `R/analysis.R` uses iterative chained prediction with gradient-boosted trees (`xgboost`):

- numeric variables are initialized with their median and modeled with squared-error regression;
- categorical variables are initialized with their mode and modeled with binary or multiclass logistic objectives;
- variables are updated sequentially for at most five iterations, using the fixed seed specified in `Main.R`.

This procedure produces one completed analytical dataset. It is not a multiple-imputation analysis with Rubin-rule pooling. Missingness indicators and a before/after missingness summary are retained in the returned analysis object.

### Latent knowledge model

Overall vector-borne disease knowledge is represented by a one-dimensional two-parameter logistic item response theory model fitted with `mirt`:

$$
\Pr(Y_{ij}=1\mid\theta_i)=\operatorname{logit}^{-1}\{a_j(\theta_i-b_j)\},
$$

where $a_j$ is item discrimination, $b_j$ is item difficulty, and $\theta_i$ is the latent knowledge level of respondent $i$.

The final model contains 15 binary items: seven disease-classification items and eight disease-vector association items. “Do not know” responses are treated as incorrect. The organism-recognition items are not included in the IRT score because the true-vector items were almost universally answered correctly and the distractor organisms showed little variation, leading to unstable or uninformative item parameters.

The code extracts expected-a-posteriori (EAP) respondent scores and their standard errors, then standardizes the scores to mean zero and standard deviation one for subsequent analyses. Figure 2 reports item discrimination and difficulty and model-implied probabilities for respondents at the 10th, 50th, and 90th percentiles of the latent-score distribution.

### Determinants of latent knowledge

The standardized IRT score is analyzed using an ordinary least squares multivariable regression. The model includes:

- age, centered at the sample mean and entered as linear and quadratic terms on a 10-year scale;
- sex;
- education, with compulsory and upper-secondary education combined as the reference category;
- residence history in Switzerland;
- self-reported tick-bite and summer mosquito-bite frequency;
- municipality urban/intermediate/rural classification; and
- indicators for having visited Europe, North America, Central America, South America, Africa, Asia, and Oceania.

Coefficients are differences in standard-deviation units of latent knowledge, conditional on the other variables in the model. The analysis is exploratory and associational; it is not intended to support causal interpretation. Figure 3 contains the adjusted coefficients with 95% confidence intervals, the fitted nonlinear age association, and unadjusted score distributions for selected covariates.

### Knowledge-attitude-practice profiles

The exploratory clustering analysis uses partitioning around medoids (PAM) with Gower dissimilarities. Active variables are organized into seven conceptual blocks:

1. knowledge;
2. tick preventive practices;
3. mosquito preventive practices;
4. perceived relevance of tick-borne diseases;
5. perceived relevance of mosquito-borne diseases;
6. perceived effectiveness of tick-control measures; and
7. perceived effectiveness of mosquito-control measures.

To prevent blocks with more questionnaire items from dominating the solution, each item in block $b$ receives weight $1/m_b$, where $m_b$ is the number of items in that block. The resulting Gower dissimilarity therefore gives equal total weight to each conceptual block. PAM is then fitted to the dissimilarity matrix with $k=6$. In the manuscript, this value was selected after considering internal validity, stability, minimum cluster size, and substantive interpretability; the current pipeline treats $k=6$ as fixed and does not rerun those selection diagnostics.

Sociodemographic characteristics, municipality classification, vector exposure, and travel history are not used to form the clusters. They are summarized only after cluster assignment. Figure 4 reports cluster sizes, block-level profiles, and selected post hoc composition variables.

### Stakeholder survey

The active pipeline imports the main and supplementary stakeholder exports, retains complete submitted questionnaires, joins department type and canton metadata, and produces descriptive tables overall and by department type. The structured summaries cover institutional involvement and resources, current activities, information needs, mosquito-control measures, and collaboration or climate-adaptation coordination.

The manuscript also reports a locally run large-language-model synthesis of multilingual stakeholder open-text responses. That narrative synthesis is not implemented by `Main.R` and is outside the reproducible scope of the active code in this repository.

## Code organization

`Main.R` is the analysis entry point and records the input paths, random seed, cluster count, imputation settings, figure resolution, and output locations. It sources the script files in the following order:

| File | Role |
|---|---|
| `R/utils.R` | Input validation, recoding helpers, file export, and session reporting. |
| `R/data.R` | Import of survey and reference data, recoding, derived knowledge indicators, validity checks, and construction of the full and primary samples. |
| `R/analysis.R` | XGBoost chained completion, 2PL IRT model, OLS regression, and block-weighted Gower/PAM clustering. |
| `R/figures.R` | Construction of manuscript Figures 1-4. |
| `R/tables.R` | Construction of the main participant table and supplementary population and stakeholder tables. |

The repository layout is:

```text
.
├── Main.R
├── R/                  # active analysis modules and retained auxiliary scripts
├── data/
│   ├── raw/            # restricted REDCap exports; not version-controlled
│   ├── external/       # Swiss municipality, occupation, and spatial lookups
│   └── processed/      # generated RDS analysis objects
├── output/
│   ├── figures/        # generated manuscript figures
│   ├── tables/         # generated manuscript and supplementary CSV tables
|   └── sessionInfo.txt
└── VBD_KAP_surveys.Rproj
```

## Data inputs and governance

The code expects three restricted survey exports: the BEready population survey, the main stakeholder survey, and a stakeholder supplement containing department and canton metadata.

The population data are enriched with three external workbooks:

- the Swiss municipality register, used to map municipality codes to names and cantons;
- the CH-ISCO 2019 level-1 nomenclature, used to group occupations; and
- the Federal Statistical Office urban/intermediate/rural municipality classification.

Access to BEready data can be requested through the BEready data access procedure using the [BEready data request form](https://www.beready.unibe.ch/pour_chercheureuse/index_fra.html). Data access is subject to review and approval according to BEready governance and applicable ethical and legal requirements.

Anonymised stakeholder survey data may be made available upon reasonable request to Julien Riou, subject to approval by the Swiss National Centre for Climate Services (NCCS).

The BEready cohort was approved under BASEC numbers 2023-00333 and 2023-02290 and is registered at ClinicalTrials.gov as NCT06739499.

## Outputs

Sourcing Main.R writes:

- four publication figures to `output/figures/`;
- the main participant-characteristics table, 14 population-survey supplementary tables, and four stakeholder-survey supplementary tables to `output/tables/`;
- prepared population and stakeholder data objects to `data/processed/`; and
- the R session record to `output/sessionInfo.txt`.

The CSV tables are in long format. Figure export is PNG by default; PDF export can be enabled through `save_pdf` in `Main.R`. Generated data products, figures, and tables are ignored by Git.

## Software environment

The recorded analysis run used R 4.2.0. Missing analytical values are imputed using gradient-boosted trees implemented in xgboost. The two-parameter logistic IRT model is fitted with `mirt`, the multivariable linear regression with base R’s `stats` package, and the Gower dissimilarities and PAM clustering with `cluster`. Data processing and output generation rely primarily on `dplyr`, `tidyr`, `readr`, `readxl`, `ggplot2`, `patchwork`, `broom`, and related packages. Direct package dependencies checked by `Main.R` are:

```text
broom, cluster, dplyr, forcats, ggplot2, ggrepel, mirt, patchwork,
purrr, readr, readxl, scales, stringr, tibble, tidyr, xgboost
```
