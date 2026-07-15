# Statistical analyses used by Figures 2-4 -------------------------------

xgb_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

xgb_init_numeric <- function(x, fallback = 0) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(fallback, length(x)))
  x[is.na(x)] <- stats::median(x, na.rm = TRUE)
  x
}

xgb_init_categorical <- function(x, fallback = "No") {
  x <- clean_chr(x)
  levels_original <- levels(factor(x))
  if (!length(levels_original)) levels_original <- fallback
  fill <- xgb_mode(x)
  if (is.na(fill)) fill <- levels_original[[1]]
  levels_original <- unique(c(levels_original, fill))
  x[is.na(x)] <- fill
  factor(x, levels = levels_original, ordered = FALSE)
}

xgb_chained_impute <- function(data, vars, categorical_vars, numeric_vars, settings, seed) {
  set.seed(seed)
  vars <- unique(intersect(vars, names(data)))
  numeric_vars <- unique(intersect(numeric_vars, vars))
  categorical_vars <- unique(c(intersect(categorical_vars, vars), setdiff(vars, numeric_vars)))
  if (!length(vars)) stop("No variables were selected for imputation.", call. = FALSE)

  imp <- data[, vars, drop = FALSE]
  missing_mask <- lapply(imp, is.na)
  vars_with_missing <- names(missing_mask)[vapply(missing_mask, any, logical(1))]

  for (variable in numeric_vars) imp[[variable]] <- xgb_init_numeric(imp[[variable]])
  for (variable in categorical_vars) imp[[variable]] <- xgb_init_categorical(imp[[variable]])
  if (anyNA(imp)) stop("Initial imputation failed.", call. = FALSE)

  flags <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), is.na, .names = "{.col}_was_missing"))

  if (!length(vars_with_missing)) return(list(data = data, flags = flags, iterations = 0L))

  max_iter <- settings$max_iter %||% 5L
  nrounds <- settings$nrounds %||% 120L
  eta <- settings$eta %||% 0.05
  max_depth <- settings$max_depth %||% 3L
  min_child_weight <- settings$min_child_weight %||% 5
  subsample <- settings$subsample %||% 0.9
  colsample_bytree <- settings$colsample_bytree %||% 0.9
  nthread <- settings$nthread %||% 1L
  last_iteration <- 0L

  for (iteration in seq_len(max_iter)) {
    last_iteration <- iteration
    categorical_changes <- 0L
    numeric_max_change <- 0

    for (target in vars_with_missing) {
      target_missing <- missing_mask[[target]]
      target_observed <- !target_missing
      if (!any(target_missing) || sum(target_observed) < 10L) next

      predictors <- setdiff(vars, target)
      predictors <- predictors[vapply(imp[predictors], function(x) dplyr::n_distinct(x) > 1L, logical(1))]
      if (!length(predictors)) next

      x <- stats::model.matrix(~ . - 1, data = imp[, predictors, drop = FALSE])
      x_train <- x[target_observed, , drop = FALSE]
      x_predict <- x[target_missing, , drop = FALSE]

      common_params <- list(
        eta = eta,
        max_depth = max_depth,
        min_child_weight = min_child_weight,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        nthread = nthread,
        seed = seed + iteration
      )

      if (target %in% numeric_vars) {
        y <- as.numeric(imp[[target]][target_observed])
        fit <- xgboost::xgb.train(
          params = c(common_params, list(objective = "reg:squarederror", eval_metric = "rmse")),
          data = xgboost::xgb.DMatrix(x_train, label = y),
          nrounds = nrounds,
          verbose = 0
        )
        previous <- as.numeric(imp[[target]][target_missing])
        predicted <- as.numeric(stats::predict(fit, x_predict))
        imp[[target]][target_missing] <- predicted
        numeric_max_change <- max(numeric_max_change, max(abs(previous - predicted), na.rm = TRUE))
      } else {
        y_factor <- droplevels(factor(as.character(imp[[target]][target_observed])))
        classes <- levels(y_factor)
        if (length(classes) < 2L) next

        if (length(classes) == 2L) {
          y <- as.integer(y_factor == classes[[2]])
          fit <- xgboost::xgb.train(
            params = c(common_params, list(objective = "binary:logistic", eval_metric = "logloss")),
            data = xgboost::xgb.DMatrix(x_train, label = y),
            nrounds = nrounds,
            verbose = 0
          )
          probability <- stats::predict(fit, x_predict)
          predicted <- ifelse(probability >= 0.5, classes[[2]], classes[[1]])
        } else {
          y <- as.integer(y_factor) - 1L
          fit <- xgboost::xgb.train(
            params = c(common_params, list(objective = "multi:softprob", eval_metric = "mlogloss", num_class = length(classes))),
            data = xgboost::xgb.DMatrix(x_train, label = y),
            nrounds = nrounds,
            verbose = 0
          )
          probability <- stats::predict(fit, x_predict)
          probability <- matrix(probability, ncol = length(classes), byrow = TRUE)
          predicted <- classes[max.col(probability, ties.method = "first")]
        }

        previous <- as.character(imp[[target]][target_missing])
        target_values <- as.character(imp[[target]])
        target_values[target_missing] <- predicted
        imp[[target]] <- factor(target_values, levels = levels(imp[[target]]), ordered = FALSE)
        categorical_changes <- categorical_changes + sum(previous != predicted, na.rm = TRUE)
      }
    }

    if (categorical_changes == 0L && numeric_max_change < 1e-8) break
  }

  completed <- data
  completed[, vars] <- imp
  remaining <- names(completed[, vars, drop = FALSE])[vapply(completed[, vars, drop = FALSE], anyNA, logical(1))]
  if (length(remaining)) {
    stop("Imputation did not complete: ", paste(remaining, collapse = ", "), call. = FALSE)
  }
  list(data = completed, flags = flags, iterations = last_iteration)
}

impute_analysis_data <- function(data, settings, seed) {
  binary_vars <- unique(intersect(
    c(
      unlist(active_blocks, use.names = FALSE),
      names(knowledge_irt_labels),
      paste0("bl_continent___", 1:7)
    ),
    names(data)
  ))
  factor_vars <- intersect(
    c("sex", "education_ch", "live_ch_3", "tick_bite_cat", "mosq_bite_cat", "land_topography", "control_need"),
    names(data)
  )
  numeric_vars <- intersect("age_incl", names(data))
  vars <- unique(c(numeric_vars, binary_vars, factor_vars))

  input <- data
  input[binary_vars] <- lapply(input[binary_vars], binary_label)
  input[factor_vars] <- lapply(input[factor_vars], function(x) {
    values <- clean_chr(x)
    variable_levels <- levels(x)
    if (is.null(variable_levels) || !length(variable_levels)) variable_levels <- unique(values[!is.na(values)])
    factor(values, levels = variable_levels, ordered = FALSE)
  })
  input[numeric_vars] <- lapply(input[numeric_vars], as.numeric)

  result <- xgb_chained_impute(
    data = input,
    vars = vars,
    categorical_vars = c(binary_vars, factor_vars),
    numeric_vars = numeric_vars,
    settings = settings,
    seed = seed
  )

  completed <- result$data
  completed[binary_vars] <- lapply(completed[binary_vars], as_binary01)
  completed <- completed |>
    dplyr::mutate(
      sex = factor(clean_chr(sex), levels = c("Male", "Female")),
      education_ch = factor(clean_chr(education_ch), levels = c("Compulsory", "Upper secondary", "Tertiary professional", "Tertiary university")),
      live_ch_3 = factor(clean_chr(live_ch_3), levels = c("Since birth", "Swiss-born, lived abroad", "Born abroad")),
      tick_bite_cat = factor(clean_chr(tick_bite_cat), levels = c("0", "1–3", "4–6", "7–9", "10+")),
      mosq_bite_cat = factor(clean_chr(mosq_bite_cat), levels = c("Unknown/exceptional", "At least yearly", "At least monthly", "At least weekly", "Every day")),
      land_topography = factor(clean_chr(land_topography), levels = c("Urban", "Intermediate", "Rural")),
      control_need = factor(clean_chr(control_need), levels = c("No", "I don't know", "Yes")),
      visited_europe = binary_label(bl_continent___1),
      visited_north_america = binary_label(bl_continent___2),
      visited_central_america = binary_label(bl_continent___3),
      visited_south_america = binary_label(bl_continent___4),
      visited_africa = binary_label(bl_continent___5),
      visited_asia = binary_label(bl_continent___6),
      visited_oceania = binary_label(bl_continent___7)
    )

  missing_summary <- tibble::tibble(
    variable = vars,
    n_missing_before = vapply(vars, function(v) sum(is.na(data[[v]])), integer(1)),
    pct_missing_before = 100 * n_missing_before / nrow(data),
    n_missing_after = vapply(vars, function(v) sum(is.na(completed[[v]])), integer(1)),
    pct_missing_after = 100 * n_missing_after / nrow(completed)
  ) |>
    dplyr::arrange(dplyr::desc(pct_missing_before), variable)

  list(data = completed, missing_summary = missing_summary, missing_flags = result$flags, iterations = result$iterations)
}

fit_irt_model <- function(data) {
  items <- intersect(names(knowledge_irt_labels), names(data))
  if (length(items) < 4L) stop("Too few IRT items are available.", call. = FALSE)

  item_data <- data |>
    dplyr::select(record_id, dplyr::all_of(items)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(items), as_binary01))
  if (anyNA(item_data |> dplyr::select(-record_id))) stop("IRT items remain missing after imputation.", call. = FALSE)

  fit <- mirt::mirt(item_data |> dplyr::select(-record_id), 1, itemtype = "2PL", verbose = FALSE)
  scores <- as.data.frame(mirt::fscores(fit, full.scores = TRUE, method = "EAP", full.scores.SE = TRUE))
  names(scores) <- c("theta", "theta_se")
  scores <- tibble::as_tibble(scores) |>
    dplyr::mutate(record_id = item_data$record_id, theta_z = as.numeric(scale(theta)))

  coefficients <- as.data.frame(mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items)
  coefficients$variable <- rownames(coefficients)
  rownames(coefficients) <- NULL
  discrimination_columns <- intersect(c("a", "a1"), names(coefficients))
  difficulty_columns <- intersect(c("b", "d"), names(coefficients))
  if (!length(discrimination_columns) || !length(difficulty_columns)) {
    stop("Could not identify IRT discrimination and difficulty parameters.", call. = FALSE)
  }
  discrimination_col <- discrimination_columns[[1]]
  difficulty_col <- difficulty_columns[[1]]
  if (difficulty_col == "d") coefficients[[difficulty_col]] <- -coefficients[[difficulty_col]] / coefficients[[discrimination_col]]

  parameters <- coefficients |>
    dplyr::transmute(
      variable,
      label = unname(knowledge_irt_labels[variable]),
      discrimination = .data[[discrimination_col]],
      difficulty = .data[[difficulty_col]],
      pct_correct = 100 * vapply(variable, function(v) mean(item_data[[v]]), numeric(1)),
      vector_group = dplyr::case_when(
        variable %in% c("kn_cls_influ", "kn_cls_measles", "kn_mat_influ", "kn_mat_measles") ~ "Distractor items",
        variable %in% c("kn_cls_lyme", "kn_mat_tbe", "kn_mat_lyme") ~ "Ticks",
        TRUE ~ "Mosquitoes"
      )
    ) |>
    dplyr::mutate(vector_group = factor(vector_group, levels = c("Distractor items", "Ticks", "Mosquitoes")))

  profiles <- scores |>
    dplyr::summarise(
      `Low score (10th percentile)` = stats::quantile(theta, 0.10),
      `Median score (50th percentile)` = stats::quantile(theta, 0.50),
      `High score (90th percentile)` = stats::quantile(theta, 0.90)
    ) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "profile", values_to = "theta") |>
    dplyr::mutate(profile = factor(profile, levels = profile))

  profile_probabilities <- tidyr::crossing(parameters, profiles) |>
    dplyr::mutate(
      probability_correct = stats::plogis(discrimination * (theta - difficulty)),
      label = forcats::fct_reorder(label, difficulty)
    )

  model_data <- data |>
    dplyr::left_join(scores, by = "record_id")

  list(
    fit = fit,
    data = model_data,
    scores = scores,
    parameters = parameters,
    profile_probabilities = profile_probabilities
  )
}

reference_rows_for_forest <- function(data) {
  make_row <- function(variable, group, prefix) {
    levels_variable <- levels(data[[variable]])
    if (!length(levels_variable)) return(NULL)
    tibble::tibble(
      term = paste0(variable, "__reference"), estimate = 0, std.error = NA_real_, statistic = NA_real_,
      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_, term_group = group,
      term_label = paste0(prefix, levels_variable[[1]], " (reference)"), is_reference = TRUE
    )
  }
  dplyr::bind_rows(
    make_row("sex", "Sex", "Sex: "),
    make_row("education_model", "Education", "Education: "),
    make_row("live_ch_3", "Residence history", "Residence: "),
    make_row("tick_bite_cat", "Tick exposure", "Tick bites: "),
    make_row("mosq_bite_cat", "Mosquito exposure", "Mosquito bites: "),
    make_row("land_topography", "Topography", "Topography: ")
  )
}

fit_knowledge_regression <- function(data) {
  reg_data <- data |>
    dplyr::transmute(
      theta_z,
      age_incl = as.numeric(age_incl),
      sex = factor(sex, levels = c("Male", "Female")),
      education_model = factor(
        dplyr::case_when(
          education_ch %in% c("Compulsory", "Upper secondary") ~ "Compulsory / upper secondary",
          TRUE ~ as.character(education_ch)
        ),
        levels = c("Compulsory / upper secondary", "Tertiary professional", "Tertiary university")
      ),
      live_ch_3 = factor(live_ch_3, levels = c("Since birth", "Swiss-born, lived abroad", "Born abroad")),
      tick_bite_cat = factor(tick_bite_cat, levels = c("0", "1–3", "4–6", "7–9", "10+")),
      mosq_bite_cat = factor(mosq_bite_cat, levels = c("Unknown/exceptional", "At least yearly", "At least monthly", "At least weekly", "Every day")),
      land_topography = factor(land_topography, levels = c("Urban", "Intermediate", "Rural")),
      visited_europe = factor(visited_europe, levels = c("No", "Yes")),
      visited_north_america = factor(visited_north_america, levels = c("No", "Yes")),
      visited_central_america = factor(visited_central_america, levels = c("No", "Yes")),
      visited_south_america = factor(visited_south_america, levels = c("No", "Yes")),
      visited_africa = factor(visited_africa, levels = c("No", "Yes")),
      visited_asia = factor(visited_asia, levels = c("No", "Yes")),
      visited_oceania = factor(visited_oceania, levels = c("No", "Yes"))
    ) |>
    dplyr::mutate(
      age_center = mean(age_incl),
      age10 = (age_incl - age_center) / 10,
      age10_sq = age10^2
    ) |>
    dplyr::select(-age_center)

  if (anyNA(reg_data)) stop("Regression variables remain missing after imputation.", call. = FALSE)

  fit <- stats::lm(
    theta_z ~ age10 + age10_sq + sex + education_model + live_ch_3 + tick_bite_cat +
      mosq_bite_cat + land_topography + visited_europe + visited_north_america +
      visited_central_america + visited_south_america + visited_africa + visited_asia + visited_oceania,
    data = reg_data
  )

  coefficients <- broom::tidy(fit, conf.int = TRUE) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      term_group = dplyr::case_when(
        term %in% c("age10", "age10_sq") ~ "Age terms",
        stringr::str_starts(term, "sex") ~ "Sex",
        stringr::str_starts(term, "education_model") ~ "Education",
        stringr::str_starts(term, "live_ch_3") ~ "Residence history",
        stringr::str_starts(term, "tick_bite_cat") ~ "Tick exposure",
        stringr::str_starts(term, "mosq_bite_cat") ~ "Mosquito exposure",
        stringr::str_starts(term, "land_topography") ~ "Topography",
        stringr::str_starts(term, "visited") ~ "Travel history",
        TRUE ~ "Other"
      ),
      term_label = term |>
        stringr::str_replace("^age10_sq$", "Age²: per 10-year²") |>
        stringr::str_replace("^age10$", "Age: per 10 years") |>
        stringr::str_replace("^sex", "Sex: ") |>
        stringr::str_replace("^education_model", "Education: ") |>
        stringr::str_replace("^live_ch_3", "Residence: ") |>
        stringr::str_replace("^tick_bite_cat", "Tick bites: ") |>
        stringr::str_replace("^mosq_bite_cat", "Mosquito bites: ") |>
        stringr::str_replace("^land_topography", "Topography: ") |>
        stringr::str_replace("^visited_europe", "Visited Europe: ") |>
        stringr::str_replace("^visited_north_america", "Visited North America: ") |>
        stringr::str_replace("^visited_central_america", "Visited Central America: ") |>
        stringr::str_replace("^visited_south_america", "Visited South America: ") |>
        stringr::str_replace("^visited_africa", "Visited Africa: ") |>
        stringr::str_replace("^visited_asia", "Visited Asia: ") |>
        stringr::str_replace("^visited_oceania", "Visited Oceania: "),
      is_reference = FALSE
    )

  forest_order <- c(
    "Age: per 10 years", "Age²: per 10-year²", "Sex: Male (reference)", "Sex: Female",
    "Education: Compulsory / upper secondary (reference)", "Education: Tertiary professional", "Education: Tertiary university",
    "Residence: Since birth (reference)", "Residence: Swiss-born, lived abroad", "Residence: Born abroad",
    "Tick bites: 0 (reference)", "Tick bites: 1–3", "Tick bites: 4–6", "Tick bites: 7–9", "Tick bites: 10+",
    "Mosquito bites: Unknown/exceptional (reference)", "Mosquito bites: At least yearly", "Mosquito bites: At least monthly",
    "Mosquito bites: At least weekly", "Mosquito bites: Every day", "Topography: Urban (reference)",
    "Topography: Intermediate", "Topography: Rural", "Visited Europe: Yes", "Visited North America: Yes",
    "Visited Central America: Yes", "Visited South America: Yes", "Visited Africa: Yes", "Visited Asia: Yes", "Visited Oceania: Yes"
  )
  group_order <- c("Age terms", "Sex", "Education", "Residence history", "Tick exposure", "Mosquito exposure", "Topography", "Travel history")
  coefficients_with_references <- dplyr::bind_rows(coefficients, reference_rows_for_forest(reg_data)) |>
    dplyr::mutate(
      term_group = factor(term_group, levels = group_order),
      term_label = factor(term_label, levels = rev(forest_order))
    ) |>
    dplyr::filter(!is.na(term_group), !is.na(term_label))

  age_values <- seq(min(reg_data$age_incl), max(reg_data$age_incl), length.out = 200)
  age_center <- mean(reg_data$age_incl)
  age_data <- tibble::tibble(age_incl = age_values, age10 = (age_values - age_center) / 10) |>
    dplyr::mutate(
      age10_sq = age10^2,
      sex = factor("Male", levels = levels(reg_data$sex)),
      education_model = factor("Compulsory / upper secondary", levels = levels(reg_data$education_model)),
      live_ch_3 = factor("Since birth", levels = levels(reg_data$live_ch_3)),
      tick_bite_cat = factor("0", levels = levels(reg_data$tick_bite_cat)),
      mosq_bite_cat = factor("Unknown/exceptional", levels = levels(reg_data$mosq_bite_cat)),
      land_topography = factor("Urban", levels = levels(reg_data$land_topography)),
      visited_europe = factor("No", levels = levels(reg_data$visited_europe)),
      visited_north_america = factor("No", levels = levels(reg_data$visited_north_america)),
      visited_central_america = factor("No", levels = levels(reg_data$visited_central_america)),
      visited_south_america = factor("No", levels = levels(reg_data$visited_south_america)),
      visited_africa = factor("No", levels = levels(reg_data$visited_africa)),
      visited_asia = factor("No", levels = levels(reg_data$visited_asia)),
      visited_oceania = factor("No", levels = levels(reg_data$visited_oceania))
    )
  prediction <- stats::predict(fit, newdata = age_data, se.fit = TRUE)
  age_curve <- age_data |>
    dplyr::mutate(
      fit = as.numeric(prediction$fit),
      se = as.numeric(prediction$se.fit),
      conf_low = fit - 1.96 * se,
      conf_high = fit + 1.96 * se
    )

  list(
    fit = fit,
    data = reg_data,
    coefficients = coefficients,
    coefficients_with_references = coefficients_with_references,
    age_curve = age_curve
  )
}

make_block_weights <- function(data, blocks) {
  blocks <- lapply(blocks, function(vars) intersect(vars, names(data)))
  blocks <- blocks[lengths(blocks) > 0L]
  weights <- setNames(rep(0, ncol(data)), names(data))
  for (block in names(blocks)) weights[blocks[[block]]] <- 1 / length(blocks[[block]])
  list(blocks = blocks, weights = weights)
}

block_score <- function(data, variables) {
  variables <- intersect(variables, names(data))
  if (!length(variables)) return(rep(NA_real_, nrow(data)))
  rowMeans(as.data.frame(lapply(data[variables], as_binary01)), na.rm = TRUE)
}

fit_kap_clusters <- function(data, k = 6L) {
  active_vars <- intersect(unlist(active_blocks, use.names = FALSE), names(data))
  source <- data |>
    dplyr::select(record_id, dplyr::all_of(active_vars), theta_z, sex, education_ch, land_topography, tick_bite_cat, mosq_bite_cat)
  active <- source[, active_vars, drop = FALSE]
  active[] <- lapply(active, binary_label)
  if (anyNA(active)) stop("Active clustering variables remain missing after imputation.", call. = FALSE)
  active <- active[, vapply(active, function(x) dplyr::n_distinct(x) > 1L, logical(1)), drop = FALSE]

  weights <- make_block_weights(active, active_blocks)
  ordered_vars <- unlist(weights$blocks, use.names = FALSE)
  distance <- cluster::daisy(active[, ordered_vars, drop = FALSE], metric = "gower", weights = weights$weights[ordered_vars])
  fit <- cluster::pam(distance, k = k, diss = TRUE)
  membership <- tibble::tibble(record_id = source$record_id, cluster = factor(fit$clustering))
  profile_data <- source |>
    dplyr::left_join(membership, by = "record_id")

  cluster_size <- profile_data |>
    dplyr::count(cluster, name = "n") |>
    dplyr::mutate(proportion = n / sum(n), label = paste0("n=", n, "\n", scales::percent(proportion, accuracy = 0.1)))

  completed_profile <- tibble::tibble(record_id = source$record_id) |>
    dplyr::bind_cols(active) |>
    dplyr::left_join(membership, by = "record_id")

  block_scores <- completed_profile |>
    dplyr::transmute(
      cluster,
      Knowledge = block_score(completed_profile, active_blocks$Knowledge),
      `Tick practices` = block_score(completed_profile, active_blocks$`Tick preventive practices`),
      `Mosquito practices` = block_score(completed_profile, active_blocks$`Mosquito preventive practices`),
      `Tick relevance` = block_score(completed_profile, active_blocks$`Tick-borne disease relevance`),
      `Mosquito relevance` = block_score(completed_profile, active_blocks$`Mosquito-borne disease relevance`),
      `Tick-control effectiveness` = block_score(completed_profile, active_blocks$`Tick-control effectiveness`),
      `Mosquito-control effectiveness` = block_score(completed_profile, active_blocks$`Mosquito-control effectiveness`)
    ) |>
    tidyr::pivot_longer(-cluster, names_to = "block", values_to = "score") |>
    dplyr::group_by(cluster, block) |>
    dplyr::summarise(mean_score = mean(score), .groups = "drop") |>
    dplyr::group_by(block) |>
    dplyr::mutate(score_z = as.numeric(scale(mean_score))) |>
    dplyr::ungroup()

  composition <- profile_data |>
    dplyr::select(cluster, education_ch, sex, land_topography, tick_bite_cat, mosq_bite_cat) |>
    dplyr::mutate(dplyr::across(-cluster, as.character)) |>
    tidyr::pivot_longer(-cluster, names_to = "variable", values_to = "category") |>
    dplyr::filter(!is.na(category)) |>
    dplyr::count(cluster, variable, category, name = "n") |>
    dplyr::group_by(cluster, variable) |>
    dplyr::mutate(proportion = n / sum(n)) |>
    dplyr::ungroup()

  list(
    fit = fit,
    distance = distance,
    membership = membership,
    data = profile_data,
    cluster_size = cluster_size,
    block_scores = block_scores,
    composition = composition
  )
}

run_figure_analyses <- function(data, config) {
  imputation <- impute_analysis_data(data, settings = config$imputation, seed = config$seed)
  irt <- fit_irt_model(imputation$data)
  regression <- fit_knowledge_regression(irt$data)
  clusters <- fit_kap_clusters(irt$data, k = config$pam_k)
  list(imputation = imputation, irt = irt, regression = regression, clusters = clusters)
}
