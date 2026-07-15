# Reproduce publication figures and CSV tables ----------------------------

if (!file.exists(file.path("R", "utils.R"))) {
  stop("Run Main.R from the repository root directory.", call. = FALSE)
}

source(file.path("R", "utils.R"))
source(file.path("R", "data.R"))
source(file.path("R", "analysis.R"))
source(file.path("R", "figures.R"))
source(file.path("R", "tables.R"))

available_cores <- parallel::detectCores(logical = TRUE)
if (is.na(available_cores)) available_cores <- 1L

config <- list(
  beready_file = file.path(
    "data", "raw",
    "1951BEreadyHauptstud-VectorborneDiseases_DATA_2026-03-05_1544.csv"
  ),
  stakeholder_file = file.path(
    "data", "raw",
    "NCCSimpactsStakehold_DATA_2025-06-26_0710.csv"
  ),
  stakeholder_supp_file = file.path(
    "data", "raw",
    "NCCSimpactsStakehold_DATA_2025-06-26_0710_SUPP.csv"
  ),
  municipality_file = file.path("data", "external", "Gemeindestand.xlsx"),
  profession_file = file.path(
    "data", "external", "HCL_CH_ISCO_19_PROF_level_1.xlsx"
  ),
  topography_file = file.path("data", "external", "Raumgliederungen.xlsx"),
  processed_dir = file.path("data", "processed"),
  figure_dir = file.path("output", "figures"),
  table_dir = file.path("output", "tables"),
  seed = 20260519L,
  pam_k = 6L,
  figure_dpi = 320L,
  save_pdf = FALSE,
  imputation = list(
    max_iter = 5L,
    nrounds = 120L,
    eta = 0.05,
    max_depth = 3L,
    min_child_weight = 5,
    subsample = 0.9,
    colsample_bytree = 0.9,
    nthread = max(1L, available_cores - 1L)
  )
)

required_packages <- c(
  "broom", "cluster", "dplyr", "forcats", "ggplot2", "ggrepel",
  "mirt", "patchwork", "purrr", "readr", "readxl", "scales",
  "stringr", "tibble", "tidyr", "xgboost"
)

check_packages(required_packages)
check_input_files(c(
  config$beready_file,
  config$stakeholder_file,
  config$stakeholder_supp_file,
  config$municipality_file,
  config$profession_file,
  config$topography_file
))
create_output_directories(config)
set.seed(config$seed)

message("Preparing BeReady survey data...")
beready <- prepare_beready_data(
  data_file = config$beready_file,
  municipality_file = config$municipality_file,
  profession_file = config$profession_file,
  topography_file = config$topography_file
)

message("Preparing stakeholder survey data...")
stakeholder <- prepare_stakeholder_data(
  data_file = config$stakeholder_file,
  supplement_file = config$stakeholder_supp_file
)

save_processed_data(
  beready = beready,
  stakeholder = stakeholder,
  directory = config$processed_dir
)

message(
  "BeReady respondents: ", nrow(beready$all),
  "; primary analysis sample: ", nrow(beready$analysis),
  "; stakeholder responses: ", nrow(stakeholder), "."
)

message("Running imputation, IRT, regression, and clustering...")
results <- run_figure_analyses(data = beready$analysis, config = config)

message("Creating and saving figures...")
figures <- make_all_figures(observed_data = beready$analysis, results = results)
save_all_figures(
  figures = figures,
  directory = config$figure_dir,
  dpi = config$figure_dpi,
  save_pdf = config$save_pdf
)

message("Creating and saving CSV tables...")
tables <- make_all_tables(
  beready_all = beready$all,
  beready_analysis = beready$analysis,
  stakeholder = stakeholder
)
write_named_csvs(tables, directory = config$table_dir)

write_session_info("output")
report_outputs(figures = figures, tables = tables, config = config)
