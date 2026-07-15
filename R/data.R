# Data import, recoding, and derived variables -----------------------------

knowledge_irt_labels <- c(
  kn_cls_westnile = "West Nile fever classified as vector-borne",
  kn_cls_dengue = "Dengue classified as vector-borne",
  kn_cls_zika = "Zika classified as vector-borne",
  kn_cls_chikun = "Chikungunya classified as vector-borne",
  kn_cls_lyme = "Lyme disease classified as vector-borne",
  kn_cls_influ = "Influenza classified as non-vector-borne",
  kn_cls_measles = "Measles classified as non-vector-borne",
  kn_mat_tbe = "TBE matched to ticks",
  kn_mat_wnf = "West Nile fever matched to mosquitoes",
  kn_mat_dengue = "Dengue matched to mosquitoes",
  kn_mat_zika = "Zika matched to mosquitoes",
  kn_mat_chikun = "Chikungunya matched to mosquitoes",
  kn_mat_lyme = "Lyme disease matched to ticks",
  kn_mat_influ = "Influenza matched to no vector",
  kn_mat_measles = "Measles matched to no vector"
)

knowledge_cluster_labels <- c(
  kn_cls_westnile = "West Nile fever classified correctly",
  kn_cls_dengue = "Dengue classified correctly",
  kn_cls_zika = "Zika classified correctly",
  kn_cls_chikun = "Chikungunya classified correctly",
  kn_cls_lyme = "Lyme disease classified correctly",
  kn_org_ticks = "Ticks identified as vectors",
  kn_org_mosq = "Mosquitoes identified as vectors",
  kn_mat_tbe = "TBE matched to ticks",
  kn_mat_wnf = "West Nile fever matched to mosquitoes",
  kn_mat_dengue = "Dengue matched to mosquitoes",
  kn_mat_zika = "Zika matched to mosquitoes",
  kn_mat_chikun = "Chikungunya matched to mosquitoes",
  kn_mat_lyme = "Lyme disease matched to ticks"
)

practice_tick_labels <- c(
  bl_vbd_meas_tick___1 = "Check for and remove ticks after outdoor activities",
  bl_vbd_meas_tick___2 = "Wear protective clothing against ticks",
  bl_vbd_meas_tick___3 = "Use repellent on skin or clothing against ticks",
  bl_vbd_meas_tick___4 = "Avoid wooded areas during tick activity",
  bl_vbd_meas_tick___0 = "No active measures against ticks"
)

practice_mosq_labels <- c(
  bl_vbd_meas_mosq___1 = "Use mosquito repellent",
  bl_vbd_meas_mosq___2 = "Wear protective clothing against mosquitoes",
  bl_vbd_meas_mosq___3 = "Burn mosquito coils",
  bl_vbd_meas_mosq___4 = "Stay in screened areas",
  bl_vbd_meas_mosq___5 = "Install window screens",
  bl_vbd_meas_mosq___6 = "Use fans",
  bl_vbd_meas_mosq___7 = "Use automatic insect spray",
  bl_vbd_meas_mosq___8 = "Install bed nets",
  bl_vbd_meas_mosq___9 = "Remove containers with standing water",
  bl_vbd_meas_mosq___0 = "No active measures against mosquitoes"
)

relevance_tick_labels <- c(
  bl_vbd_dis_problem___1 = "TBE is a current health problem in Switzerland",
  bl_vbd_dis_problem___6 = "Lyme disease is a current health problem in Switzerland"
)

relevance_mosq_labels <- c(
  bl_vbd_dis_problem___2 = "West Nile fever is a current health problem in Switzerland",
  bl_vbd_dis_problem___3 = "Dengue is a current health problem in Switzerland",
  bl_vbd_dis_problem___4 = "Zika is a current health problem in Switzerland",
  bl_vbd_dis_problem___5 = "Chikungunya is a current health problem in Switzerland"
)

efficacy_tick_labels <- c(
  bl_vbd_meas_tick_eff___1 = "Tick control: environmental pesticides",
  bl_vbd_meas_tick_eff___2 = "Tick control: biological control",
  bl_vbd_meas_tick_eff___3 = "Tick control: clear vegetation in woods",
  bl_vbd_meas_tick_eff___4 = "Tick control: protect deer from ticks",
  bl_vbd_meas_tick_eff___5 = "Tick control: control deer numbers",
  bl_vbd_meas_tick_eff___6 = "Tick control: block deer from public woods",
  bl_vbd_meas_tick_eff___7 = "Tick control: protect small rodents from ticks",
  bl_vbd_meas_tick_eff___8 = "Tick control: promote personal protection",
  bl_vbd_meas_tick_eff___0 = "Tick control: none of these effective"
)

efficacy_mosq_labels <- c(
  bl_vbd_meas_mosq_eff___1 = "Mosquito control: environmental pesticides",
  bl_vbd_meas_mosq_eff___2 = "Mosquito control: biological control",
  bl_vbd_meas_mosq_eff___3 = "Mosquito control: eliminate standing water",
  bl_vbd_meas_mosq_eff___4 = "Mosquito control: apply larvicides",
  bl_vbd_meas_mosq_eff___5 = "Mosquito control: install repellent sprayers",
  bl_vbd_meas_mosq_eff___6 = "Mosquito control: install mosquito traps",
  bl_vbd_meas_mosq_eff___7 = "Mosquito control: release irradiated/GM mosquitoes",
  bl_vbd_meas_mosq_eff___8 = "Mosquito control: promote personal protection",
  bl_vbd_meas_mosq_eff___0 = "Mosquito control: none of these effective"
)

control_need_labels <- c(control_need = "Need for vector-control measures in Switzerland")

active_blocks <- list(
  Knowledge = names(knowledge_cluster_labels),
  `Tick preventive practices` = names(practice_tick_labels),
  `Mosquito preventive practices` = names(practice_mosq_labels),
  `Tick-borne disease relevance` = names(relevance_tick_labels),
  `Mosquito-borne disease relevance` = names(relevance_mosq_labels),
  `Tick-control effectiveness` = names(efficacy_tick_labels),
  `Mosquito-control effectiveness` = names(efficacy_mosq_labels)
)

fig1_blocks <- c(active_blocks, list(`Need for vector control` = "control_need"))
fig1_label_map <- c(
  knowledge_cluster_labels,
  practice_tick_labels,
  practice_mosq_labels,
  relevance_tick_labels,
  relevance_mosq_labels,
  efficacy_tick_labels,
  efficacy_mosq_labels,
  control_need_labels
)

read_beready_data <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
  if ("Unnamed: 0" %in% names(data)) data <- dplyr::select(data, -`Unnamed: 0`)
  if (!"record_id" %in% names(data)) data$record_id <- seq_len(nrow(data))
  if (!"bl_sex" %in% names(data)) stop("The BeReady export must contain 'bl_sex'.", call. = FALSE)

  data |>
    dplyr::filter(!is.na(bl_sex)) |>
    dplyr::mutate(record_id = as.character(record_id))
}

check_lookup_columns <- function(data, required, filename) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      filename, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      ". See data/external/README.md.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_municipality_lookup <- function(path) {
  lookup <- readxl::read_excel(path, sheet = "Daten")
  required <- c("BFS Gde-nummer", "Gemeindename", "Kanton")
  check_lookup_columns(lookup, required, basename(path))

  lookup |>
    dplyr::transmute(
      municipality_code = suppressWarnings(
        as.integer(normalize_code(.data[["BFS Gde-nummer"]]))
      ),
      municipality_name = clean_chr(.data[["Gemeindename"]]),
      municipality_canton = clean_chr(.data[["Kanton"]])
    ) |>
    dplyr::filter(!is.na(municipality_code)) |>
    dplyr::distinct(municipality_code, .keep_all = TRUE)
}

read_profession_lookup <- function(path) {
  lookup <- readxl::read_excel(path, sheet = 1)
  required <- c("Code", "Name_en")
  check_lookup_columns(lookup, required, basename(path))

  lookup |>
    dplyr::transmute(
      occupation_code = normalize_code(.data[["Code"]]),
      occupation_group = clean_chr(.data[["Name_en"]])
    ) |>
    dplyr::filter(!is.na(occupation_code), !is.na(occupation_group)) |>
    dplyr::distinct(occupation_code, .keep_all = TRUE) |>
    dplyr::arrange(suppressWarnings(as.integer(occupation_code)))
}

read_topography_lookup <- function(path) {
  # The first row of the downloaded workbook is a title. Row 2 contains
  # the actual field names and row 3 contains classification identifiers.
  lookup <- readxl::read_excel(path, sheet = "Daten", skip = 1)
  required <- c("BFS Gde-nummer", "Stadt/Land-Typologie")
  check_lookup_columns(lookup, required, basename(path))

  topography_raw <- clean_chr(lookup[["Stadt/Land-Typologie"]])
  topography <- dplyr::case_when(
    normalize_code(topography_raw) == "1" ~ "Urban",
    normalize_code(topography_raw) == "2" ~ "Intermediate",
    normalize_code(topography_raw) == "3" ~ "Rural",
    stringr::str_to_lower(topography_raw) %in% c("urban", "städtisch", "urbain") ~ "Urban",
    stringr::str_to_lower(topography_raw) %in%
      c("intermediate", "intermediär", "intermediaire", "intermédiaire") ~ "Intermediate",
    stringr::str_to_lower(topography_raw) %in% c("rural", "ländlich") ~ "Rural",
    TRUE ~ NA_character_
  )

  tibble::tibble(
    municipality_code = suppressWarnings(
      as.integer(normalize_code(lookup[["BFS Gde-nummer"]]))
    ),
    land_topography = factor(
      topography,
      levels = c("Urban", "Intermediate", "Rural")
    )
  ) |>
    dplyr::filter(!is.na(municipality_code)) |>
    dplyr::distinct(municipality_code, .keep_all = TRUE)
}

extract_occupation_code <- function(data) {
  profession_cols <- names(data)[
    stringr::str_starts(names(data), "bl_prof_now_sql")
  ]
  if (!length(profession_cols)) return(rep(NA_character_, nrow(data)))

  profession_text <- apply(
    data[, profession_cols, drop = FALSE],
    1,
    function(x) {
      x <- clean_chr(x)
      x <- x[!is.na(x)]
      if (length(x)) x[[1]] else NA_character_
    }
  )

  # REDCap stores the detailed CH-ISCO code in brackets, for example
  # "Biostatistician [45304021]". The first digit is the level-1 code.
  stringr::str_match(profession_text, "\\[(\\d)")[, 2]
}

contradiction_flag <- function(transmitter, matches, expected_vector) {
  transmitter01 <- as_binary01(transmitter)
  match_checks <- lapply(matches, function(x) clean_chr(x) == expected_vector)
  matched_expected <- Reduce(`|`, match_checks)
  condition <- transmitter01 == 0L & matched_expected
  dplyr::case_when(is.na(condition) ~ NA_integer_, condition ~ 1L, TRUE ~ 0L)
}

recode_beready_data <- function(data, municipalities, professions, topography) {
  sex_birth_levels <- c("Female", "Male", "Intersex")
  gender_levels <- c("Female", "Male", "Other")
  education_levels <- c(
    "None", "Incomplete compulsory schooling", "Compulsory schooling",
    "One-year transitional program", "General education school",
    "Apprenticeship/vocational school", "Gymnasial maturity",
    "Professional maturity", "Federal diploma exam", "Higher vocational school",
    "Bachelor degree", "Master degree", "Doctorate/habilitation"
  )
  residence_levels <- c("Since birth", "Born in Switzerland but also lived abroad", "Born abroad")
  marital_levels <- c(
    "Married, living with spouse", "Married, permanently separated",
    "Registered partnership, living together", "Registered partnership, separated",
    "Single, stable partnership", "Single, living alone", "Divorced", "Widowed"
  )
  mobility_levels <- c(
    "I have no problems walking around", "I have slight problems walking around",
    "I have moderate problems walking around", "I have severe problems walking around",
    "I am unable to walk around"
  )
  match_levels <- c("None of these", "Mosquitoes", "Ticks", "Wasps", "Bed bugs", "I don't know")
  occupation_labels <- stats::setNames(
    professions$occupation_group,
    professions$occupation_code
  )
  occupation_levels <- professions$occupation_group
  occupation_code <- extract_occupation_code(data)

  checkbox_vars <- names(data)[stringr::str_detect(names(data), "___")]
  data[checkbox_vars] <- lapply(data[checkbox_vars], as_binary01)

  out <- data |>
    dplyr::mutate(
      municipality_code = suppressWarnings(as.integer(normalize_code(bl_commune))),
      sex_birth = decode_codes(bl_sex, c("1" = "Male", "2" = "Female", "3" = "Intersex"), sex_birth_levels),
      sex = factor(dplyr::if_else(sex_birth %in% c("Male", "Female"), as.character(sex_birth), NA_character_), levels = c("Male", "Female")),
      gender_identity = decode_codes(bl_gender_def, c("1" = "Male", "2" = "Female", "3" = "Other"), gender_levels),
      education_detail = decode_codes(
        bl_education,
        c(
          "0" = "None", "1" = "Incomplete compulsory schooling", "2" = "Compulsory schooling",
          "3" = "One-year transitional program", "4" = "General education school",
          "5" = "Apprenticeship/vocational school", "6" = "Gymnasial maturity",
          "7" = "Professional maturity", "8" = "Federal diploma exam",
          "9" = "Higher vocational school", "10" = "Bachelor degree",
          "11" = "Master degree", "12" = "Doctorate/habilitation"
        ),
        education_levels
      ),
      education_ch = factor(
        dplyr::case_when(
          education_detail %in% c("Incomplete compulsory schooling", "Compulsory schooling", "One-year transitional program") ~ "Compulsory",
          education_detail %in% c("General education school", "Apprenticeship/vocational school", "Gymnasial maturity", "Professional maturity") ~ "Upper secondary",
          education_detail %in% c("Federal diploma exam", "Higher vocational school") ~ "Tertiary professional",
          education_detail %in% c("Bachelor degree", "Master degree", "Doctorate/habilitation") ~ "Tertiary university",
          TRUE ~ NA_character_
        ),
        levels = c("Compulsory", "Upper secondary", "Tertiary professional", "Tertiary university")
      ),
      residence_history = decode_codes(
        bl_live_ch_since,
        c("1" = "Since birth", "2" = "Born in Switzerland but also lived abroad", "3" = "Born abroad"),
        residence_levels
      ),
      live_ch_3 = factor(
        dplyr::recode(
          as.character(residence_history),
          "Born in Switzerland but also lived abroad" = "Swiss-born, lived abroad",
          .default = as.character(residence_history)
        ),
        levels = c("Since birth", "Swiss-born, lived abroad", "Born abroad")
      ),
      marital_status = decode_codes(
        bl_scg_marital_status,
        c(
          "1" = "Married, living with spouse", "2" = "Married, permanently separated",
          "3" = "Registered partnership, living together", "4" = "Registered partnership, separated",
          "5" = "Single, stable partnership", "6" = "Single, living alone",
          "7" = "Divorced", "8" = "Widowed"
        ),
        marital_levels
      ),
      occupation_group = factor(
        unname(occupation_labels[occupation_code]),
        levels = occupation_levels
      ),
      vaccinated = decode_codes(bl_vacc_yn, c("1" = "Yes", "0" = "No"), c("Yes", "No")),
      mobility = decode_codes(
        eq5d_mb_5l_swi_ger,
        c(
          "1" = "I have no problems walking around",
          "2" = "I have slight problems walking around",
          "3" = "I have moderate problems walking around",
          "4" = "I have severe problems walking around",
          "5" = "I am unable to walk around"
        ),
        mobility_levels
      ),
      travel_abroad = decode_codes(bl_abroad_yn, c("1" = "Yes", "0" = "No"), c("Yes", "No")),
      concept_check = decode_codes(bl_vbd_tick_yn, c("1" = "Yes", "0" = "No", "99" = "I don't know"), c("Yes", "No", "I don't know")),
      match_tbe = decode_codes(bl_vbd_tick_enc, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_wnf = decode_codes(bl_vbd_wnf, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_dengue = decode_codes(bl_vbd_dengue, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_zika = decode_codes(bl_vbd_zika, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_chikun = decode_codes(bl_vbd_chikun, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_lyme = decode_codes(bl_vbd_lyme, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_influenza = decode_codes(bl_vbd_influenza, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      match_measles = decode_codes(bl_vbd_measle, c("0" = "None of these", "1" = "Mosquitoes", "2" = "Ticks", "3" = "Wasps", "4" = "Bed bugs", "99" = "I don't know"), match_levels),
      tick_bite_cat = decode_codes(bl_vbd_tickbite_nr, c("0" = "0", "1" = "1–3", "2" = "4–6", "3" = "7–9", "4" = "10+"), c("0", "1–3", "4–6", "7–9", "10+")),
      mosq_bite_raw = decode_codes(
        bl_vbd_mosqbite_nr,
        c("1" = "Every day", "2" = "At least weekly", "3" = "At least monthly", "4" = "At least yearly", "5" = "Less than yearly", "0" = "Never", "99" = "I don't know"),
        c("Every day", "At least weekly", "At least monthly", "At least yearly", "Less than yearly", "Never", "I don't know")
      ),
      mosq_bite_cat = factor(
        dplyr::case_when(
          mosq_bite_raw %in% c("I don't know", "Never", "Less than yearly") ~ "Unknown/exceptional",
          TRUE ~ as.character(mosq_bite_raw)
        ),
        levels = c("Unknown/exceptional", "At least yearly", "At least monthly", "At least weekly", "Every day")
      ),
      control_need = decode_codes(bl_vbd_meth_yn, c("1" = "Yes", "0" = "No", "99" = "I don't know", "Don't know" = "I don't know"), c("No", "I don't know", "Yes")),
      tick_prevention_other = clean_chr(bl_vbd_oth_meas),
      mosquito_prevention_other = clean_chr(bl_vbd_oth_meas_2)
    ) |>
    dplyr::left_join(municipalities, by = "municipality_code") |>
    dplyr::left_join(topography, by = "municipality_code") |>
    dplyr::mutate(
      visited_europe = binary_label(bl_continent___1),
      visited_north_america = binary_label(bl_continent___2),
      visited_central_america = binary_label(bl_continent___3),
      visited_south_america = binary_label(bl_continent___4),
      visited_africa = binary_label(bl_continent___5),
      visited_asia = binary_label(bl_continent___6),
      visited_oceania = binary_label(bl_continent___7)
    )

  out
}

derive_beready_variables <- function(data) {
  correct_binary <- function(x, correct = 1L) {
    y <- as_binary01(x)
    dplyr::case_when(is.na(y) ~ NA_integer_, y == correct ~ 1L, TRUE ~ 0L)
  }
  correct_match <- function(x, expected) {
    y <- clean_chr(x)
    dplyr::case_when(is.na(y) ~ NA_integer_, y == expected ~ 1L, TRUE ~ 0L)
  }

  data |>
    dplyr::mutate(
      kn_cls_westnile = correct_binary(bl_vbd_vector_dis___1),
      kn_cls_dengue = correct_binary(bl_vbd_vector_dis___2),
      kn_cls_zika = correct_binary(bl_vbd_vector_dis___3),
      kn_cls_chikun = correct_binary(bl_vbd_vector_dis___4),
      kn_cls_lyme = correct_binary(bl_vbd_vector_dis___5),
      kn_cls_influ = correct_binary(bl_vbd_vector_dis___6, correct = 0L),
      kn_cls_measles = correct_binary(bl_vbd_vector_dis___7, correct = 0L),
      kn_org_ticks = correct_binary(bl_vbd_transmitter___1),
      kn_org_mosq = correct_binary(bl_vbd_transmitter___2),
      kn_org_wasp = correct_binary(bl_vbd_transmitter___3, correct = 0L),
      kn_org_bed = correct_binary(bl_vbd_transmitter___4, correct = 0L),
      kn_mat_tbe = correct_match(match_tbe, "Ticks"),
      kn_mat_wnf = correct_match(match_wnf, "Mosquitoes"),
      kn_mat_dengue = correct_match(match_dengue, "Mosquitoes"),
      kn_mat_zika = correct_match(match_zika, "Mosquitoes"),
      kn_mat_chikun = correct_match(match_chikun, "Mosquitoes"),
      kn_mat_lyme = correct_match(match_lyme, "Ticks"),
      kn_mat_influ = correct_match(match_influenza, "None of these"),
      kn_mat_measles = correct_match(match_measles, "None of these"),
      contrad_ticks = contradiction_flag(bl_vbd_transmitter___1, list(match_tbe, match_lyme), "Ticks"),
      contrad_mosq = contradiction_flag(bl_vbd_transmitter___2, list(match_wnf, match_dengue, match_zika, match_chikun), "Mosquitoes"),
      contrad_any_logical = contrad_ticks == 1L | contrad_mosq == 1L,
      contrad_any = dplyr::case_when(
        is.na(contrad_any_logical) ~ NA_integer_,
        contrad_any_logical ~ 1L,
        TRUE ~ 0L
      ),
      contrad_ticks_label = factor(dplyr::case_when(contrad_ticks == 0L ~ "Consistent", contrad_ticks == 1L ~ "Contradiction", TRUE ~ NA_character_), levels = c("Consistent", "Contradiction")),
      contrad_mosq_label = factor(dplyr::case_when(contrad_mosq == 0L ~ "Consistent", contrad_mosq == 1L ~ "Contradiction", TRUE ~ NA_character_), levels = c("Consistent", "Contradiction")),
      contrad_any_label = factor(dplyr::case_when(contrad_any == 0L ~ "Consistent", contrad_any == 1L ~ "Contradiction", TRUE ~ NA_character_), levels = c("Consistent", "Contradiction")),
      included_in_analysis = concept_check == "Yes" & contrad_any == 0L
    ) |>
    dplyr::select(-contrad_any_logical)
}

prepare_beready_data <- function(
  data_file,
  municipality_file,
  profession_file,
  topography_file
) {
  municipalities <- read_municipality_lookup(municipality_file)
  professions <- read_profession_lookup(profession_file)
  topography <- read_topography_lookup(topography_file)

  all_data <- read_beready_data(data_file) |>
    recode_beready_data(
      municipalities = municipalities,
      professions = professions,
      topography = topography
    ) |>
    derive_beready_variables()

  analysis_data <- all_data |>
    dplyr::filter(included_in_analysis %in% TRUE)

  list(all = all_data, analysis = analysis_data)
}

read_stakeholder_data <- function(path) {
  readr::read_csv2(path, show_col_types = FALSE, guess_max = 10000) |>
    dplyr::mutate(record_id = as.character(record_id))
}

prepare_stakeholder_data <- function(data_file, supplement_file) {
  survey <- read_stakeholder_data(data_file)
  supplement <- readr::read_csv2(supplement_file, show_col_types = FALSE) |>
    dplyr::mutate(record_id = as.character(record_id))

  checkbox_vars <- names(survey)[stringr::str_detect(names(survey), "___")]
  survey[checkbox_vars] <- lapply(survey[checkbox_vars], as_binary01)

  survey |>
    dplyr::left_join(supplement, by = "record_id") |>
    dplyr::filter(form_complete == 2, !is.na(clean_chr(form_timestamp))) |>
    dplyr::mutate(
      department_type = decode_codes(type, c("M" = "Human health", "T" = "Animal health", "U" = "Environment"), c("Human health", "Animal health", "Environment")),
      involvement = decode_codes(
        b_1,
        c("1" = "Yes", "2" = "No", "3" = "No information / don't know"),
        c("Yes", "No", "No information / don't know")
      ),
      dplyr::across(dplyr::starts_with("b_1_1___"), ~ dplyr::if_else(involvement == "No", .x, NA_integer_)),
      c3_applicable = c_1___8 != 1L & c_1___10 != 1L & c_2___9 != 1L & c_2___11 != 1L,
      dplyr::across(dplyr::starts_with("c_3___"), ~ dplyr::if_else(c3_applicable, .x, NA_integer_))
    ) |>
    dplyr::select(-c3_applicable)
}
