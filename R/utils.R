# Shared project utilities -------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

clean_chr <- function(x) {
  y <- stringr::str_squish(as.character(x))
  y[y %in% c("", "NA", "NaN", "NULL", "[not completed]")] <- NA_character_
  y
}

normalize_code <- function(x) {
  y <- clean_chr(x)
  stringr::str_replace(y, "\\.0+$", "")
}

decode_codes <- function(x, labels, levels = NULL, ordered = FALSE) {
  code <- normalize_code(x)
  out <- unname(labels[code])
  already_labelled <- !is.na(code) & code %in% unname(labels)
  out[is.na(out) & already_labelled] <- code[is.na(out) & already_labelled]
  if (is.null(levels)) out else factor(out, levels = levels, ordered = ordered)
}

as_binary01 <- function(x) {
  if (is.logical(x)) return(dplyr::if_else(is.na(x), NA_integer_, as.integer(x)))
  y <- stringr::str_to_lower(normalize_code(x))
  dplyr::case_when(
    is.na(y) ~ NA_integer_,
    y %in% c("1", "yes", "y", "true", "checked", "selected", "correct", "consistent") ~ 1L,
    y %in% c("0", "no", "n", "false", "unchecked", "unselected", "incorrect", "contradiction") ~ 0L,
    TRUE ~ NA_integer_
  )
}

binary_label <- function(x, no = "No", yes = "Yes") {
  y <- as_binary01(x)
  factor(dplyr::case_when(y == 0L ~ no, y == 1L ~ yes, TRUE ~ NA_character_), levels = c(no, yes))
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)
safe_prop <- function(n, denominator) ifelse(is.na(denominator) | denominator == 0, NA_real_, n / denominator)

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

first_existing_name <- function(candidates, names_available) {
  hit <- candidates[candidates %in% names_available]
  if (length(hit)) hit[[1]] else NA_character_
}

check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Install the following packages before running Main.R: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_input_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Required input file(s) not found:\n- ",
      paste(missing, collapse = "\n- "),
      "\nSee README.md for the expected files and schemas.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

create_output_directories <- function(config) {
  dirs <- c(config$processed_dir, config$figure_dir, config$table_dir)
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

save_processed_data <- function(beready, stakeholder, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  saveRDS(beready$all, file.path(directory, "beready_all.rds"))
  saveRDS(beready$analysis, file.path(directory, "beready_analysis.rds"))
  saveRDS(stakeholder, file.path(directory, "stakeholder_analysis.rds"))
  invisible(TRUE)
}

write_named_csvs <- function(objects, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  purrr::iwalk(objects, function(x, name) {
    if (!inherits(x, "data.frame")) stop("Output '", name, "' is not a data frame.", call. = FALSE)
    readr::write_csv(x, file.path(directory, paste0(name, ".csv")), na = "")
  })
  invisible(objects)
}

save_all_figures <- function(figures, directory, dpi = 320, save_pdf = FALSE) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  specs <- list(
    figure1_questionnaire_responses = c(width = 11.5, height = 13.0),
    figure2_irt_results = c(width = 12.0, height = 11.0),
    figure3_knowledge_determinants = c(width = 16.0, height = 15.0),
    figure4_cluster_profiles = c(width = 15.0, height = 12.0)
  )

  purrr::iwalk(figures, function(plot, name) {
    spec <- specs[[name]] %||% c(width = 12, height = 9)
    ggplot2::ggsave(
      filename = file.path(directory, paste0(name, ".png")),
      plot = plot,
      width = unname(spec[["width"]]),
      height = unname(spec[["height"]]),
      dpi = dpi,
      limitsize = FALSE
    )
    if (isTRUE(save_pdf)) {
      ggplot2::ggsave(
        filename = file.path(directory, paste0(name, ".pdf")),
        plot = plot,
        width = unname(spec[["width"]]),
        height = unname(spec[["height"]]),
        limitsize = FALSE
      )
    }
  })
  invisible(figures)
}

write_session_info <- function(output_dir = "output") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
  invisible(TRUE)
}

report_outputs <- function(figures, tables, config) {
  message("Analysis completed.")
  message("Figures: ", normalizePath(config$figure_dir, winslash = "/", mustWork = FALSE),
          " (", length(figures), " files)")
  message("Tables:  ", normalizePath(config$table_dir, winslash = "/", mustWork = FALSE),
          " (", length(tables), " files)")
  invisible(TRUE)
}
