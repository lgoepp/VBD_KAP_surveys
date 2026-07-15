# Publication figures -----------------------------------------------------

make_figure1 <- function(data) {
  response_levels <- c("Yes", "No", "I don't know", "Missing")
  superblocks <- list(
    Knowledge = "Knowledge",
    Practices = c("Tick preventive practices", "Mosquito preventive practices"),
    Attitudes = c(
      "Tick-borne disease relevance", "Mosquito-borne disease relevance",
      "Tick-control effectiveness", "Mosquito-control effectiveness"
    ),
    `Control need` = "Need for vector control"
  )

  metadata <- tibble::tibble(
    block = rep(names(fig1_blocks), lengths(fig1_blocks)),
    variable = unlist(fig1_blocks, use.names = FALSE)
  ) |>
    dplyr::filter(variable %in% names(data)) |>
    dplyr::mutate(
      label = unname(fig1_label_map[variable]),
      item_order = dplyr::row_number(),
      y = dplyr::n() - item_order + 1L,
      superblock = purrr::map_chr(block, function(block_name) {
        hit <- names(superblocks)[vapply(superblocks, function(x) block_name %in% x, logical(1))]
        hit[[1]]
      })
    )

  item_data <- purrr::pmap_dfr(metadata, function(block, variable, label, item_order, y, superblock) {
    response <- if (variable == "control_need") {
      dplyr::case_when(
        clean_chr(data[[variable]]) == "Yes" ~ "Yes",
        clean_chr(data[[variable]]) == "No" ~ "No",
        clean_chr(data[[variable]]) == "I don't know" ~ "I don't know",
        TRUE ~ "Missing"
      )
    } else {
      dplyr::case_when(as_binary01(data[[variable]]) == 1L ~ "Yes", as_binary01(data[[variable]]) == 0L ~ "No", TRUE ~ "Missing")
    }
    tibble::tibble(block, variable, label, y, superblock, response = factor(response, levels = response_levels))
  }) |>
    dplyr::count(block, variable, label, y, superblock, response, name = "n") |>
    dplyr::group_by(block, variable, label, y, superblock) |>
    tidyr::complete(response = factor(response_levels, levels = response_levels), fill = list(n = 0L)) |>
    dplyr::mutate(denominator = sum(n), proportion = n / denominator) |>
    dplyr::ungroup()

  block_spans <- metadata |>
    dplyr::group_by(block, superblock) |>
    dplyr::summarise(ymin = min(y) - 0.5, ymax = max(y) + 0.5, ymid = mean(range(y)), .groups = "drop") |>
    dplyr::mutate(
      block_label = dplyr::case_when(
        block == "Need for vector control" ~ "Need for VBD control",
        block == "Tick-borne disease relevance" ~ "TBD\nrelevance",
        block == "Mosquito-borne disease relevance" ~ "MBD\nrelevance",
        TRUE ~ stringr::str_wrap(block, width = 22)
      ),
      label_angle = dplyr::if_else(block == "Need for vector control", 0, 90),
      label_x = dplyr::if_else(block == "Need for vector control", 1.035, 1.060)
    )

  super_spans <- metadata |>
    dplyr::group_by(superblock) |>
    dplyr::summarise(ymin = min(y) - 0.5, ymax = max(y) + 0.5, ymid = mean(range(y)), .groups = "drop") |>
    dplyr::mutate(label = dplyr::if_else(superblock == "Control need", "", superblock))

  separators <- block_spans |>
    dplyr::filter(ymin > min(ymin))

  ggplot2::ggplot(item_data, ggplot2::aes(x = proportion, y = y, fill = response)) +
    ggplot2::geom_segment(
      data = separators,
      ggplot2::aes(x = 0, xend = 1.11, y = ymin, yend = ymin),
      inherit.aes = FALSE,
      linewidth = 0.18,
      colour = "grey82"
    ) +
    ggplot2::geom_hline(
      data = super_spans |> dplyr::filter(ymin > min(ymin)),
      ggplot2::aes(yintercept = ymin),
      inherit.aes = FALSE,
      linewidth = 0.9,
      colour = "grey35"
    ) +
    ggplot2::geom_col(width = 0.72, orientation = "y", position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_segment(
      data = block_spans,
      ggplot2::aes(x = 1.01, xend = 1.01, y = ymin, yend = ymax),
      inherit.aes = FALSE,
      linewidth = 0.35
    ) +
    ggplot2::geom_segment(
      data = super_spans |> dplyr::filter(label != ""),
      ggplot2::aes(x = 1.115, xend = 1.115, y = ymin, yend = ymax),
      inherit.aes = FALSE,
      linewidth = 0.55
    ) +
    ggplot2::geom_text(
      data = block_spans,
      ggplot2::aes(x = label_x, y = ymid, label = block_label, angle = label_angle),
      inherit.aes = FALSE,
      size = 3.0,
      lineheight = 0.85
    ) +
    ggplot2::geom_text(
      data = super_spans |> dplyr::filter(label != ""),
      ggplot2::aes(x = 1.155, y = ymid, label = label),
      inherit.aes = FALSE,
      angle = 90,
      size = 3.4,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Yes" = "#2166AC", "No" = "#B2182B", "I don't know" = "#1A9850", "Missing" = "grey75"),
      breaks = response_levels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = metadata$y, labels = metadata$label, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(x = "Response proportion", y = NULL, fill = "Response") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9.5, margin = ggplot2::margin(r = 4)),
      plot.margin = ggplot2::margin(5.5, 135, 5.5, 5.5),
      legend.position = "bottom"
    )
}

make_figure2 <- function(irt) {
  panel_a <- ggplot2::ggplot(irt$parameters, ggplot2::aes(x = difficulty, y = discrimination, colour = vector_group)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = pct_correct), alpha = 0.9) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = stringr::str_wrap(label, 22)),
      size = 2.8,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.9,
      point.padding = 0.8,
      segment.linewidth = 0.25,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("Distractor items" = "#666666", "Ticks" = "#1B9E77", "Mosquitoes" = "#D95F02"), drop = FALSE) +
    ggplot2::scale_size_continuous(name = "% correct", range = c(1.8, 5.2)) +
    ggplot2::labs(x = "Difficulty (b)", y = "Discrimination (a)", colour = "Vectors") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  panel_b <- ggplot2::ggplot(
    irt$profile_probabilities,
    ggplot2::aes(x = probability_correct, y = label, colour = profile)
  ) +
    ggplot2::geom_line(ggplot2::aes(group = label), colour = "black", linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "Model-implied probability of correct response", y = NULL, colour = "Latent knowledge profile") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", axis.text.y = ggplot2::element_text(size = 11.5))

  (panel_a / panel_b) + patchwork::plot_annotation(tag_levels = "A")
}

make_density_panel <- function(data, variable, label, colours) {
  plot_data <- data |>
    dplyr::select(theta_z, category = dplyr::all_of(variable)) |>
    dplyr::filter(!is.na(category))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = theta_z, colour = category, fill = category)) +
    ggplot2::geom_density(alpha = 0.13, linewidth = 0.55, adjust = 1.1) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::scale_fill_manual(values = colours, drop = FALSE) +
    ggplot2::labs(title = label, x = "Standardized latent knowledge score", y = "Density", colour = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 9.3) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9.5),
      legend.position = c(1.01, 0.5),
      legend.justification = c(0, 0.5),
      plot.margin = ggplot2::margin(t = 2, r = 135, b = 2, l = 2),
      legend.key.height = grid::unit(0.36, "cm"),
      legend.key.width = grid::unit(0.45, "cm"),
      legend.text = ggplot2::element_text(size = 7.5)
    )
}

make_figure3 <- function(regression) {
  coefficients <- regression$coefficients_with_references
  panel_a <- ggplot2::ggplot(coefficients, ggplot2::aes(x = estimate, y = term_label)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_errorbarh(
      data = coefficients |> dplyr::filter(!is_reference),
      ggplot2::aes(xmin = conf.low, xmax = conf.high),
      height = 0.15,
      linewidth = 0.45
    ) +
    ggplot2::geom_point(data = coefficients |> dplyr::filter(!is_reference), size = 1.8) +
    ggplot2::geom_point(data = coefficients |> dplyr::filter(is_reference), shape = 23, size = 2.35, stroke = 0.45, fill = "white") +
    ggplot2::facet_grid(term_group ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Difference in standardized latent knowledge score (95% CI)", y = NULL) +
    ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), strip.text.y = ggplot2::element_blank())

  panel_b <- ggplot2::ggplot() +
    ggplot2::geom_point(data = regression$data, ggplot2::aes(x = age_incl, y = theta_z), alpha = 0.18, size = 1.5, stroke = 0) +
    ggplot2::geom_ribbon(data = regression$age_curve, ggplot2::aes(x = age_incl, ymin = conf_low, ymax = conf_high), alpha = 0.20) +
    ggplot2::geom_line(data = regression$age_curve, ggplot2::aes(x = age_incl, y = fit), linewidth = 0.8) +
    ggplot2::labs(x = "Age (years)", y = "Standardized latent knowledge score") +
    ggplot2::theme_minimal(base_size = 10)

  density_panels <- list(
    make_density_panel(regression$data, "education_model", "Education", c(
      "Compulsory / upper secondary" = "#008837", "Tertiary professional" = "#E66101", "Tertiary university" = "#0571B0"
    )),
    make_density_panel(regression$data, "sex", "Sex", c("Male" = "#2166AC", "Female" = "#B2182B")),
    make_density_panel(regression$data, "tick_bite_cat", "Tick-bite frequency", c(
      "0" = "#F7FCF5", "1–3" = "#C7E9C0", "4–6" = "#74C476", "7–9" = "#238B45", "10+" = "#00441B"
    )),
    make_density_panel(regression$data, "mosq_bite_cat", "Mosquito-bite frequency", c(
      "Unknown/exceptional" = "#F7FBFF", "At least yearly" = "#C6DBEF", "At least monthly" = "#6BAED6",
      "At least weekly" = "#2171B5", "Every day" = "#08306B"
    )),
    make_density_panel(regression$data, "land_topography", "Topography", c(
      "Urban" = "#1B9E77", "Intermediate" = "#D95F02", "Rural" = "#7570B3"
    ))
  )
  panel_c <- patchwork::wrap_plots(density_panels, ncol = 1, heights = c(1.1, 0.85, 1.15, 1.15, 0.95))

  (panel_a | (patchwork::wrap_elements(full = panel_b) / patchwork::wrap_elements(full = panel_c))) +
    patchwork::plot_layout(widths = c(1, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

make_composition_panel <- function(data, variable_name, label, palette) {
  panel_data <- data |>
    dplyr::filter(.data$variable == variable_name) |>
    dplyr::mutate(category = factor(category, levels = names(palette)))
  ggplot2::ggplot(panel_data, ggplot2::aes(x = cluster, y = proportion, fill = category)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_fill_manual(values = palette, breaks = names(palette), drop = FALSE) +
    ggplot2::labs(title = label, x = "Cluster", y = "Composition", fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9.5),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = c(1.01, 0.5),
      legend.justification = c(0, 0.5),
      plot.margin = ggplot2::margin(t = 2, r = 135, b = 2, l = 2),
      legend.key.height = grid::unit(0.34, "cm"),
      legend.key.width = grid::unit(0.45, "cm"),
      legend.text = ggplot2::element_text(size = 7.5)
    )
}

make_figure4 <- function(clusters) {
  panel_a <- ggplot2::ggplot(clusters$cluster_size, ggplot2::aes(x = cluster, y = proportion)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.15, size = 3) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(x = "Cluster", y = "Participants") +
    ggplot2::theme_minimal(base_size = 12)

  block_order <- c(
    "Knowledge", "Tick practices", "Mosquito practices", "Tick-control effectiveness",
    "Mosquito-control effectiveness", "Tick relevance", "Mosquito relevance"
  )
  block_data <- clusters$block_scores |>
    dplyr::mutate(block = factor(block, levels = block_order))
  panel_b <- ggplot2::ggplot(block_data, ggplot2::aes(x = cluster, y = block, fill = score_z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(mean_score, accuracy = 1)), size = 3.5) +
    ggplot2::scale_y_discrete(limits = rev(block_order), drop = FALSE) +
    ggplot2::scale_fill_gradient2(name = "Block mean\nstandardized", midpoint = 0) +
    ggplot2::labs(x = "Cluster", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")

  palettes <- list(
    education_ch = c("Compulsory" = "#7B3294", "Upper secondary" = "#008837", "Tertiary professional" = "#E66101", "Tertiary university" = "#0571B0"),
    sex = c("Male" = "#2166AC", "Female" = "#B2182B"),
    land_topography = c("Urban" = "#1B9E77", "Intermediate" = "#D95F02", "Rural" = "#7570B3"),
    tick_bite_cat = c("0" = "#F7FCF5", "1–3" = "#C7E9C0", "4–6" = "#74C476", "7–9" = "#238B45", "10+" = "#00441B"),
    mosq_bite_cat = c("Unknown/exceptional" = "#F7FBFF", "At least yearly" = "#C6DBEF", "At least monthly" = "#6BAED6", "At least weekly" = "#2171B5", "Every day" = "#08306B")
  )
  composition <- patchwork::wrap_plots(
    list(
      make_composition_panel(clusters$composition, "education_ch", "Education", palettes$education_ch),
      make_composition_panel(clusters$composition, "sex", "Sex", palettes$sex),
      make_composition_panel(clusters$composition, "land_topography", "Topography", palettes$land_topography),
      make_composition_panel(clusters$composition, "tick_bite_cat", "Tick-bite frequency", palettes$tick_bite_cat),
      make_composition_panel(clusters$composition, "mosq_bite_cat", "Mosquito-bite frequency", palettes$mosq_bite_cat)
    ),
    ncol = 1,
    heights = c(1.05, 0.85, 0.95, 1.05, 1.05)
  )

  ((panel_a / panel_b) | patchwork::wrap_elements(full = composition)) +
    patchwork::plot_layout(widths = c(1, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

make_all_figures <- function(observed_data, results) {
  list(
    figure1_questionnaire_responses = make_figure1(observed_data),
    figure2_irt_results = make_figure2(results$irt),
    figure3_knowledge_determinants = make_figure3(results$regression),
    figure4_cluster_profiles = make_figure4(results$clusters)
  )
}
