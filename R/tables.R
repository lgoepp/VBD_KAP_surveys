# CSV tables for the manuscript and supplementary material ----------------

sample_sets <- function(all_data, analysis_data) {
  list(`All respondents` = all_data, `Analysis sample` = analysis_data)
}

summarise_categorical_samples <- function(all_data, analysis_data, variable, question, section, response_levels = NULL) {
  purrr::imap_dfr(sample_sets(all_data, analysis_data), function(data, sample_name) {
    x <- data[[variable]]
    values <- clean_chr(x)
    levels_to_show <- response_levels
    if (is.null(levels_to_show) || !length(levels_to_show)) levels_to_show <- levels(x)
    if (is.null(levels_to_show) || !length(levels_to_show)) levels_to_show <- sort(unique(values[!is.na(values)]))
    denominator <- sum(!is.na(values))
    rows <- tibble::tibble(
      section = section,
      question = question,
      response = levels_to_show,
      sample = sample_name,
      n = vapply(levels_to_show, function(level) sum(values == level, na.rm = TRUE), integer(1)),
      denominator = denominator
    ) |>
      dplyr::mutate(percent = 100 * safe_prop(n, denominator), missing_n = sum(is.na(values)))
    dplyr::bind_rows(
      rows,
      tibble::tibble(
        section = section, question = question, response = "(Missing)", sample = sample_name,
        n = sum(is.na(values)), denominator = NA_integer_, percent = NA_real_, missing_n = sum(is.na(values))
      )
    )
  })
}

summarise_continuous_sample <- function(data, variable, question, section, sample_name = "Analysis sample") {
  x <- as.numeric(data[[variable]])
  tibble::tibble(
    section = section,
    question = question,
    response = "Mean (SD)",
    sample = sample_name,
    n = sum(!is.na(x)),
    denominator = sum(!is.na(x)),
    percent = NA_real_,
    missing_n = sum(is.na(x)),
    mean = safe_mean(x),
    sd = safe_sd(x)
  )
}

summarise_checkbox_samples <- function(all_data, analysis_data, variables, labels, question, section) {
  purrr::imap_dfr(sample_sets(all_data, analysis_data), function(data, sample_name) {
    variables_present <- intersect(variables, names(data))
    values <- data[, variables_present, drop = FALSE]
    missing_rows <- if (length(variables_present)) sum(rowSums(!is.na(values)) == 0L) else nrow(data)
    rows <- tibble::tibble(
      section = section,
      question = question,
      response = unname(labels[variables_present]),
      sample = sample_name,
      n = vapply(variables_present, function(variable) sum(as_binary01(data[[variable]]) == 1L, na.rm = TRUE), integer(1)),
      denominator = nrow(data)
    ) |>
      dplyr::mutate(percent = 100 * safe_prop(n, denominator), missing_n = missing_rows)
    dplyr::bind_rows(
      rows,
      tibble::tibble(
        section = section, question = question, response = "(Missing)", sample = sample_name,
        n = missing_rows, denominator = NA_integer_, percent = NA_real_, missing_n = missing_rows
      )
    )
  })
}

summarise_accuracy_samples <- function(all_data, analysis_data, variables, labels, section) {
  purrr::imap_dfr(sample_sets(all_data, analysis_data), function(data, sample_name) {
    purrr::map_dfr(variables, function(variable) {
      x <- as_binary01(data[[variable]])
      n_incorrect <- sum(x == 0L, na.rm = TRUE)
      n_correct <- sum(x == 1L, na.rm = TRUE)
      n_missing <- sum(is.na(x))
      n_observed <- n_incorrect + n_correct
      
      tibble::tibble(
        section = section,
        question = unname(labels[[variable]]),
        response = c("Incorrect", "Correct", "(Missing)"),
        sample = sample_name,
        n = c(n_incorrect, n_correct, n_missing),
        denominator = c(n_observed, n_observed, NA_integer_),
        percent = c(
          100 * safe_prop(n_incorrect, n_observed),
          100 * safe_prop(n_correct, n_observed),
          NA_real_
        ),
        missing_n = n_missing
      )
    })
  })
}

summarise_open_text <- function(data, variable, question, no_answer_label = "(No open-text answer)") {
  responses <- clean_chr(data[[variable]])
  missing_total <- sum(is.na(responses))
  responses[is.na(responses)] <- no_answer_label
  tibble::tibble(response = responses) |>
    dplyr::count(response, name = "n", sort = TRUE) |>
    dplyr::mutate(
      section = "Open-ended prevention measures",
      question = question,
      sample = "All respondents",
      denominator = nrow(data),
      percent = 100 * n / denominator,
      missing_n = missing_total
    ) |>
    dplyr::select(section, question, response, sample, n, denominator, percent, missing_n)
}

make_table_1 <- function(data) {
  categorical <- list(
    list("sex", "Sex", c("Male", "Female")),
    list("education_ch", "Education", c("Compulsory", "Upper secondary", "Tertiary professional", "Tertiary university")),
    list("live_ch_3", "Residence history", c("Since birth", "Swiss-born, lived abroad", "Born abroad")),
    list("land_topography", "Municipality topography", c("Urban", "Intermediate", "Rural")),
    list("tick_bite_cat", "Tick-bite frequency", c("0", "1–3", "4–6", "7–9", "10+")),
    list("mosq_bite_cat", "Mosquito-bite frequency", c("Unknown/exceptional", "At least yearly", "At least monthly", "At least weekly", "Every day"))
  )
  age <- summarise_continuous_sample(data, "age_incl", "Age, years", "Participant characteristics")
  categories <- purrr::map_dfr(categorical, function(spec) {
    x <- clean_chr(data[[spec[[1]]]])
    denominator <- sum(!is.na(x))
    tibble::tibble(
      section = "Participant characteristics",
      question = spec[[2]],
      response = spec[[3]],
      sample = "Analysis sample",
      n = vapply(spec[[3]], function(level) sum(x == level, na.rm = TRUE), integer(1)),
      denominator = denominator,
      percent = 100 * safe_prop(n, denominator),
      missing_n = sum(is.na(x)),
      mean = NA_real_,
      sd = NA_real_
    )
  })
  dplyr::bind_rows(age, categories)
}

make_table_s1_01 <- function(all_data, analysis_data) {
  specs <- list(
    list("sex_birth", "What sex were you assigned at birth?", c("Female", "Male", "Intersex")),
    list("gender_identity", "Which label best describes your social and felt gender?", c("Female", "Male", "Other")),
    list("education_detail", "What is your highest completed education?", levels(all_data$education_detail)),
    list("residence_history", "Since when have you been living in Switzerland?", levels(all_data$residence_history)),
    list("marital_status", "What is your marital status?", levels(all_data$marital_status)),
    list("occupation_group", "Occupation according to CH-ISCO 2019", levels(droplevels(all_data$occupation_group)))
  )
  purrr::map_dfr(specs, function(spec) {
    summarise_categorical_samples(all_data, analysis_data, spec[[1]], spec[[2]], "Sociodemography", spec[[3]])
  })
}

make_table_s1_02 <- function(all_data, analysis_data) {
  fixed <- dplyr::bind_rows(
    summarise_categorical_samples(all_data, analysis_data, "vaccinated", "Have you ever received any vaccines?", "Exposures from inclusion questionnaire", c("Yes", "No")),
    summarise_categorical_samples(all_data, analysis_data, "mobility", "Please select what best describes your mobility today", "Exposures from inclusion questionnaire", levels(all_data$mobility)),
    summarise_categorical_samples(all_data, analysis_data, "travel_abroad", "Have you ever stayed abroad for more than two days?", "Exposures from inclusion questionnaire", c("Yes", "No"))
  )
  continent_labels <- c(
    bl_continent___1 = "Europe", bl_continent___2 = "North America", bl_continent___3 = "Central America",
    bl_continent___4 = "South America", bl_continent___5 = "Africa", bl_continent___6 = "Asia", bl_continent___7 = "Oceania"
  )
  dplyr::bind_rows(
    fixed,
    summarise_checkbox_samples(all_data, analysis_data, names(continent_labels), continent_labels, "On which continent?", "Exposures from inclusion questionnaire")
  )
}

make_table_s1_03 <- function(all_data, analysis_data) {
  summarise_categorical_samples(
    all_data, analysis_data, "concept_check",
    "After reading the definition and TBE example, would you classify TBE as a vector-borne disease?",
    "Understanding check", c("Yes", "No", "I don't know")
  )
}

make_table_s1_04 <- function(all_data, analysis_data) {
  disease_labels <- c(
    bl_vbd_diseases___1 = "West Nile fever", bl_vbd_diseases___2 = "Dengue", bl_vbd_diseases___3 = "Zika",
    bl_vbd_diseases___4 = "Chikungunya", bl_vbd_diseases___5 = "Lyme disease", bl_vbd_diseases___6 = "Influenza",
    bl_vbd_diseases___7 = "Measles", bl_vbd_diseases___0 = "None of these"
  )
  organism_labels <- c(
    bl_vbd_organism___1 = "Ticks", bl_vbd_organism___2 = "Mosquitoes", bl_vbd_organism___3 = "Wasps",
    bl_vbd_organism___4 = "Bed bugs", bl_vbd_organism___0 = "None of these"
  )
  dplyr::bind_rows(
    summarise_checkbox_samples(all_data, analysis_data, names(disease_labels), disease_labels, "Have you heard of the following diseases?", "Preliminary knowledge"),
    summarise_checkbox_samples(all_data, analysis_data, names(organism_labels), organism_labels, "Have you heard of the following organisms?", "Preliminary knowledge")
  )
}

make_table_s1_05 <- function(all_data, analysis_data) {
  add_combined_contradiction <- function(data) {
    ticks <- clean_chr(data$contrad_ticks_label)
    mosquitoes <- clean_chr(data$contrad_mosq_label)
    
    data |>
      dplyr::mutate(
        contrad_any_supplement = factor(
          dplyr::case_when(
            is.na(ticks) | is.na(mosquitoes) ~ NA_character_,
            ticks == "Contradiction" | mosquitoes == "Contradiction" ~ "Contradiction",
            ticks == "Consistent" & mosquitoes == "Consistent" ~ "Consistent",
            TRUE ~ NA_character_
          ),
          levels = c("Consistent", "Contradiction")
        )
      )
  }
  
  all_data <- add_combined_contradiction(all_data)
  analysis_data <- add_combined_contradiction(analysis_data)
  
  dplyr::bind_rows(
    summarise_categorical_samples(
      all_data, analysis_data,
      "contrad_ticks_label",
      "Denied ticks as transmitters and identified ticks as transmitting a disease",
      "Logical contradictions",
      c("Consistent", "Contradiction")
    ),
    summarise_categorical_samples(
      all_data, analysis_data,
      "contrad_mosq_label",
      "Denied mosquitoes as transmitters and identified mosquitoes as transmitting a disease",
      "Logical contradictions",
      c("Consistent", "Contradiction")
    ),
    summarise_categorical_samples(
      all_data, analysis_data,
      "contrad_any_supplement",
      "Any of the two contradictions",
      "Logical contradictions",
      c("Consistent", "Contradiction")
    )
  )
}

make_table_s1_06 <- function(all_data, analysis_data) {
  labels <- c(
    kn_cls_westnile = "West Nile correctly classified as VBD", kn_cls_dengue = "Dengue correctly classified as VBD",
    kn_cls_zika = "Zika correctly classified as VBD", kn_cls_chikun = "Chikungunya correctly classified as VBD",
    kn_cls_lyme = "Lyme correctly classified as VBD", kn_cls_influ = "Influenza correctly classified as not a VBD",
    kn_cls_measles = "Measles correctly classified as not a VBD"
  )
  summarise_accuracy_samples(all_data, analysis_data, names(labels), labels, "Vector-borne disease test item")
}

make_table_s1_07 <- function(all_data, analysis_data) {
  labels <- c(
    kn_org_ticks = "Ticks correctly identified as transmitting disease",
    kn_org_mosq = "Mosquitoes correctly identified as transmitting disease",
    kn_org_wasp = "Wasps correctly identified as not transmitting disease",
    kn_org_bed = "Bed bugs correctly identified as not transmitting disease"
  )
  summarise_accuracy_samples(all_data, analysis_data, names(labels), labels, "Vector test item")
}

make_table_s1_08 <- function(all_data, analysis_data) {
  labels <- c(
    kn_mat_tbe = "TBE correctly matched to ticks", kn_mat_wnf = "West Nile correctly matched to mosquitoes",
    kn_mat_dengue = "Dengue correctly matched to mosquitoes", kn_mat_zika = "Zika correctly matched to mosquitoes",
    kn_mat_chikun = "Chikungunya correctly matched to mosquitoes", kn_mat_lyme = "Lyme correctly matched to ticks",
    kn_mat_influ = "Influenza correctly matched to none", kn_mat_measles = "Measles correctly matched to none"
  )
  summarise_accuracy_samples(all_data, analysis_data, names(labels), labels, "Disease-vector matching test item")
}

make_table_s1_09 <- function(all_data, analysis_data) {
  dplyr::bind_rows(
    summarise_categorical_samples(all_data, analysis_data, "tick_bite_cat", "How many tick bites do you receive on average per year?", "Self-reported individual exposure", c("0", "1–3", "4–6", "7–9", "10+")),
    summarise_categorical_samples(all_data, analysis_data, "mosq_bite_raw", "In summer, how often are you bitten by mosquitoes?", "Self-reported individual exposure", levels(all_data$mosq_bite_raw))
  )
}

make_table_s1_10 <- function(all_data, analysis_data) {
  tick_labels <- c(
    bl_vbd_meas_tick___1 = "Check for and remove ticks after outdoor activities",
    bl_vbd_meas_tick___2 = "Wear protective clothing",
    bl_vbd_meas_tick___3 = "Use insect repellent on skin or clothing",
    bl_vbd_meas_tick___4 = "Avoid wooded areas during tick activity",
    bl_vbd_meas_tick___88 = "Other measures (open-text)",
    bl_vbd_meas_tick___0 = "No active measures"
  )
  mosquito_labels <- c(
    bl_vbd_meas_mosq___1 = "Use repellent", bl_vbd_meas_mosq___2 = "Wear protective clothing",
    bl_vbd_meas_mosq___3 = "Burn mosquito coils", bl_vbd_meas_mosq___4 = "Stay in screened areas",
    bl_vbd_meas_mosq___5 = "Install window screens", bl_vbd_meas_mosq___6 = "Use fans",
    bl_vbd_meas_mosq___7 = "Use automatic insect spray", bl_vbd_meas_mosq___8 = "Install bed nets",
    bl_vbd_meas_mosq___9 = "Remove containers with standing water", bl_vbd_meas_mosq___88 = "Other measures (open-text)",
    bl_vbd_meas_mosq___0 = "No active measures"
  )
  dplyr::bind_rows(
    summarise_checkbox_samples(all_data, analysis_data, names(tick_labels), tick_labels, "In the last 12 months, what have you done to protect yourself/family against tick bites or tick-borne diseases?", "Self-reported preventive practices"),
    summarise_checkbox_samples(all_data, analysis_data, names(mosquito_labels), mosquito_labels, "In the last 12 months, what have you done to protect yourself/family against mosquito bites?", "Self-reported preventive practices")
  )
}

make_table_s1_11 <- function(all_data) {
  summarise_open_text(all_data, "tick_prevention_other", "Other tick-related individual prevention measures")
}

make_table_s1_12 <- function(all_data) {
  summarise_open_text(all_data, "mosquito_prevention_other", "Other mosquito-related individual prevention measures")
}

make_table_s1_13 <- function(all_data, analysis_data) {
  tick_labels <- c(
    bl_vbd_meas_tick_eff___1 = "Apply environmental pesticides",
    bl_vbd_meas_tick_eff___2 = "Use biological control (e.g. predators)",
    bl_vbd_meas_tick_eff___3 = "Clear vegetation in woods",
    bl_vbd_meas_tick_eff___4 = "Protect deer from ticks",
    bl_vbd_meas_tick_eff___5 = "Control deer numbers in public woods",
    bl_vbd_meas_tick_eff___6 = "Block deer from public woods",
    bl_vbd_meas_tick_eff___7 = "Protect small rodents from ticks",
    bl_vbd_meas_tick_eff___8 = "Promote personal protective measures",
    bl_vbd_meas_tick_eff___0 = "None of these"
  )
  mosquito_labels <- c(
    bl_vbd_meas_mosq_eff___1 = "Apply environmental pesticides",
    bl_vbd_meas_mosq_eff___2 = "Use biological control (e.g. predators)",
    bl_vbd_meas_mosq_eff___3 = "Eliminate standing water in containers",
    bl_vbd_meas_mosq_eff___4 = "Apply larvicides",
    bl_vbd_meas_mosq_eff___5 = "Install repellent sprayers",
    bl_vbd_meas_mosq_eff___6 = "Install mosquito traps",
    bl_vbd_meas_mosq_eff___7 = "Release irradiated or genetically modified mosquitoes",
    bl_vbd_meas_mosq_eff___8 = "Promote personal protective measures",
    bl_vbd_meas_mosq_eff___0 = "None of these"
  )
  dplyr::bind_rows(
    summarise_checkbox_samples(all_data, analysis_data, names(tick_labels), tick_labels, "Which measures effectively protect the public against tick-borne diseases?", "Beliefs regarding effectiveness of public control measures"),
    summarise_checkbox_samples(all_data, analysis_data, names(mosquito_labels), mosquito_labels, "Which measures effectively protect the public against mosquito-borne diseases?", "Beliefs regarding effectiveness of public control measures")
  )
}

make_table_s1_14 <- function(all_data, analysis_data) {
  disease_labels <- c(
    bl_vbd_dis_problem___1 = "Tick-borne encephalitis", bl_vbd_dis_problem___2 = "West Nile fever",
    bl_vbd_dis_problem___3 = "Dengue", bl_vbd_dis_problem___4 = "Zika",
    bl_vbd_dis_problem___5 = "Chikungunya", bl_vbd_dis_problem___6 = "Lyme disease"
  )
  dplyr::bind_rows(
    summarise_checkbox_samples(all_data, analysis_data, names(disease_labels), disease_labels, "Do you consider the following disease a current health problem in Switzerland?", "Perception of VBD risk and control measures"),
    summarise_categorical_samples(all_data, analysis_data, "control_need", "Do you think vector-control measures are necessary in Switzerland?", "Perception of VBD risk and control measures", c("Yes", "No", "I don't know"))
  )
}

stakeholder_groups <- function(data) {
  c(list(Overall = data), split(data, data$department_type, drop = TRUE))
}

summarise_stakeholder_categorical <- function(data, variable, question, section, response_levels) {
  purrr::imap_dfr(stakeholder_groups(data), function(group_data, group_name) {
    x <- clean_chr(group_data[[variable]])
    denominator <- sum(!is.na(x))
    missing_total <- sum(is.na(x))
    rows <- tibble::tibble(
      section = section, question = question, response = response_levels, group = group_name,
      n = vapply(response_levels, function(level) sum(x == level, na.rm = TRUE), integer(1)),
      denominator = denominator, percent = 100 * safe_prop(n, denominator),
      missing_n = missing_total, group_n = nrow(group_data)
    )
    dplyr::bind_rows(
      rows,
      tibble::tibble(
        section = section, question = question, response = "(Missing)", group = group_name,
        n = missing_total, denominator = NA_integer_, percent = NA_real_,
        missing_n = missing_total, group_n = nrow(group_data)
      )
    )
  })
}

summarise_stakeholder_checkboxes <- function(
    data, variables, labels, question, section,
    eligible = function(x) rep(TRUE, nrow(x)),
    missing_variable = NULL
) {
  purrr::imap_dfr(stakeholder_groups(data), function(group_data, group_name) {
    eligible_rows <- eligible(group_data)
    eligible_rows[is.na(eligible_rows)] <- FALSE
    
    explicit_missing <- rep(FALSE, nrow(group_data))
    if (!is.null(missing_variable)) {
      explicit_missing <- as_binary01(group_data[[missing_variable]]) == 1L
      explicit_missing[is.na(explicit_missing)] <- FALSE
    }
    
    observed_rows <- if (length(variables)) {
      rowSums(!is.na(group_data[, variables, drop = FALSE])) > 0L
    } else {
      rep(FALSE, nrow(group_data))
    }
    
    included_rows <- eligible_rows & !explicit_missing & observed_rows
    included_data <- group_data[included_rows, , drop = FALSE]
    
    denominator <- nrow(included_data)
    missing_total <- sum(!included_rows)
    
    rows <- tibble::tibble(
      section = section,
      question = question,
      response = unname(labels[variables]),
      group = group_name,
      n = vapply(
        variables,
        function(variable) {
          sum(as_binary01(included_data[[variable]]) == 1L, na.rm = TRUE)
        },
        integer(1)
      ),
      denominator = denominator,
      percent = 100 * safe_prop(n, denominator),
      missing_n = missing_total,
      group_n = nrow(group_data)
    )
    
    dplyr::bind_rows(
      rows,
      tibble::tibble(
        section = section,
        question = question,
        response = "(Missing)",
        group = group_name,
        n = missing_total,
        denominator = NA_integer_,
        percent = NA_real_,
        missing_n = missing_total,
        group_n = nrow(group_data)
      )
    )
  })
}

make_table_s2_01 <- function(data) {
  data <- data |>
    dplyr::mutate(involvement = dplyr::na_if(
        clean_chr(involvement),
        "No information / don't know")
    )
  reasons <- c(
    b_1_1___1 = "Introduction of measures/activities is being reviewed or planned",
    b_1_1___2 = "VBDs are not considered part of the authority's responsibilities",
    b_1_1___3 = "VBDs are not a high priority",
    b_1_1___4 = "Case numbers are not yet sufficient to trigger measures",
    b_1_1___5 = "Lack of financial and/or human resources",
    b_1_1___6 = "Lack of professionally trained personnel",
    b_1_1___7 = "Lack of public interest",
    b_1_1___8 = "Lack of political decision/mandate",
    b_1_1___9 = "Lack of national guidelines or recommendations",
    b_1_1___11 = "Other reasons"
  )
  four_option_labels <- c(
    b_2___1 = "Yes, for tick-borne diseases",
    b_2___2 = "Yes, for mosquito-borne diseases",
    b_2___3 = "No, neither for tick-borne nor mosquito-borne diseases"
  )
  human_resource_labels <- setNames(
    unname(four_option_labels),
    sub("b_2", "b_3", names(four_option_labels))
  )
  budget_labels <- setNames(
    unname(four_option_labels),
    sub("b_2", "b_4", names(four_option_labels))
  )
  dplyr::bind_rows(
    summarise_stakeholder_categorical(
      data,
      "involvement",
      "Is your authority currently involved in VBD-related measures or activities?",
      "Involvement and resources",
      c("Yes", "No")
    ),
    summarise_stakeholder_checkboxes(
      data,
      names(reasons),
      reasons,
      "If no, what are the reasons?",
      "Involvement and resources",
      eligible = function(x) x$involvement == "No",
      missing_variable = "b_1_1___10"
    ),
    summarise_stakeholder_checkboxes(
      data,
      names(four_option_labels),
      four_option_labels,
      "Do you know of any cantonal strategy or action plan?",
      "Involvement and resources",
      missing_variable = "b_2___4"
    ),
    summarise_stakeholder_checkboxes(
      data,
      names(human_resource_labels),
      human_resource_labels,
      "Does your authority have human resources for VBD activities?",
      "Involvement and resources",
      missing_variable = "b_3___4"
    ),
    summarise_stakeholder_checkboxes(
      data,
      names(budget_labels),
      budget_labels,
      "Does your authority have a budget for VBD activities?",
      "Involvement and resources",
      missing_variable = "b_4___4"
    )
  )
}

make_table_s2_02 <- function(data) {
  tick_activities <- c(
    c_1___1 = "Public relations for tick-borne diseases", c_1___2 = "Targeted awareness of professionals",
    c_1___3 = "Vaccination advice for the population", c_1___4 = "Advice for homeowners on tick management",
    c_1___5 = "Active tick monitoring", c_1___6 = "Passive tick monitoring",
    c_1___7 = "Training and distribution of educational materials", c_1___8 = "No information / don't know",
    c_1___9 = "Other: open answer", c_1___10 = "No activities for tick-borne diseases"
  )
  mosquito_activities <- c(
    c_2___1 = "Public relations for mosquito-borne diseases", c_2___2 = "Targeted awareness of professionals",
    c_2___3 = "Active mosquito monitoring", c_2___4 = "Passive mosquito monitoring",
    c_2___5 = "Verification of mosquito sightings at new locations", c_2___6 = "Mosquito control on public land",
    c_2___7 = "Mosquito control on private properties", c_2___8 = "Training and distribution of educational materials",
    c_2___9 = "No information / don't know", c_2___10 = "Other: open answer", c_2___11 = "No activities for mosquito-borne diseases"
  )
  information_topics <- c(
    c_3___1 = "Correct removal of ticks", c_3___2 = "Elimination of mosquito breeding sites", c_3___3 = "Vaccinations",
    c_3___4 = "Information on mosquito/tick sprays", c_3___5 = "Protection against bites/stings",
    c_3___6 = "Awareness for careful symptom monitoring", c_3___7 = "Information on risk areas",
    c_3___8 = "Correct installation of mosquito nets", c_3___9 = "No information / don't know", c_3___10 = "Other: open answer"
  )
  dplyr::bind_rows(
    summarise_stakeholder_checkboxes(data, names(tick_activities), tick_activities, "Current activities for tick-borne diseases", "Concrete measures and activities - part 1"),
    summarise_stakeholder_checkboxes(data, names(mosquito_activities), mosquito_activities, "Current activities for mosquito-borne diseases", "Concrete measures and activities - part 1"),
    summarise_stakeholder_checkboxes(data, names(information_topics), information_topics, "If the authority develops information, which subjects are covered?", "Concrete measures and activities - part 1", eligible = function(x) !is.na(x$c_3___1))
  )
}

make_table_s2_03 <- function(data) {
  tick_needs <- c(
    c_6___1 = "Prevention of tick-borne diseases", c_6___2 = "Detection of ticks and pathogens in ticks",
    c_6___3 = "Diagnosis of tick-borne diseases", c_6___4 = "Management and control of tick-borne diseases",
    c_6___5 = "Therapy/treatment of tick-borne diseases", c_6___6 = "Other: open answer"
  )
  mosquito_needs <- c(
    c_7___1 = "Prevention of mosquito-borne diseases", c_7___2 = "Detection of invasive mosquitoes and pathogens",
    c_7___3 = "Diagnosis of mosquito-borne diseases", c_7___4 = "Management and control of mosquitoes/mosquito-borne diseases",
    c_7___5 = "Therapy/treatment of mosquito-borne diseases", c_7___6 = "Other: open answer"
  )
  control_measures <- c(
    c_8___1 = "Adulticides (pyrethroids)", c_8___2 = "Non-chemical larvicides", c_8___3 = "Biological larvicides",
    c_8___4 = "Elimination of breeding sites", c_8___5 = "Sterilization of mosquitoes", c_8___6 = "None", c_8___7 = "Other: open answer"
  )
  dplyr::bind_rows(
    summarise_stakeholder_checkboxes(data, names(tick_needs), tick_needs, "Greatest information need or knowledge gap for tick-borne diseases", "Concrete measures and activities - part 2"),
    summarise_stakeholder_checkboxes(data, names(mosquito_needs), mosquito_needs, "Greatest information need or knowledge gap for mosquito-borne diseases", "Concrete measures and activities - part 2"),
    summarise_stakeholder_checkboxes(data, names(control_measures), control_measures, "Measures implemented to control invasive mosquitoes", "Concrete measures and activities - part 2")
  )
}

make_table_s2_04 <- function(data) {
  network <- c(
    d_1___1 = "Yes, for tick-borne diseases", d_1___2 = "Yes, for mosquito-borne diseases",
    d_1___3 = "No, neither for tick-borne nor mosquito-borne diseases", d_1___4 = "No information / don't know"
  )
  climate_labels <- c(
    d_3___1 = "Yes, sufficient for tick-borne diseases", d_3___2 = "Yes, sufficient for mosquito-borne diseases",
    d_3___3 = "Insufficient for tick-borne diseases", d_3___4 = "Insufficient for mosquito-borne diseases",
    d_3___5 = "No, neither for tick-borne nor mosquito-borne diseases", d_3___6 = "No information / don't know"
  )
  cantonal_labels <- setNames(unname(climate_labels), sub("d_3", "d_4", names(climate_labels)))
  dplyr::bind_rows(
    summarise_stakeholder_checkboxes(data, names(network), network, "Does the authority maintain active exchanges with researchers and specialists?", "Collaboration and coordination"),
    summarise_stakeholder_checkboxes(data, names(climate_labels), climate_labels, "Are VBDs sufficiently addressed in the national climate adaptation strategy?", "Collaboration and coordination"),
    summarise_stakeholder_checkboxes(data, names(cantonal_labels), cantonal_labels, "Are VBDs sufficiently addressed in the cantonal climate adaptation strategy?", "Collaboration and coordination")
  )
}

make_all_tables <- function(beready_all, beready_analysis, stakeholder) {
  list(
    table_1_participant_characteristics = make_table_1(beready_analysis),
    table_s1_01_sociodemography = make_table_s1_01(beready_all, beready_analysis),
    table_s1_02_baseline_exposures = make_table_s1_02(beready_all, beready_analysis),
    table_s1_03_understanding_check = make_table_s1_03(beready_all, beready_analysis),
    table_s1_04_preliminary_knowledge = make_table_s1_04(beready_all, beready_analysis),
    table_s1_05_logical_contradictions = make_table_s1_05(beready_all, beready_analysis),
    table_s1_06_disease_classification = make_table_s1_06(beready_all, beready_analysis),
    table_s1_07_vector_recognition = make_table_s1_07(beready_all, beready_analysis),
    table_s1_08_disease_vector_matching = make_table_s1_08(beready_all, beready_analysis),
    table_s1_09_self_reported_exposure = make_table_s1_09(beready_all, beready_analysis),
    table_s1_10_preventive_practices = make_table_s1_10(beready_all, beready_analysis),
    table_s1_11_open_text_tick_prevention = make_table_s1_11(beready_all),
    table_s1_12_open_text_mosquito_prevention = make_table_s1_12(beready_all),
    table_s1_13_control_measure_effectiveness = make_table_s1_13(beready_all, beready_analysis),
    table_s1_14_risk_and_control_perception = make_table_s1_14(beready_all, beready_analysis),
    table_s2_01_involvement_and_resources = make_table_s2_01(stakeholder),
    table_s2_02_activities = make_table_s2_02(stakeholder),
    table_s2_03_information_needs = make_table_s2_03(stakeholder),
    table_s2_04_collaboration_and_coordination = make_table_s2_04(stakeholder)
  )
}