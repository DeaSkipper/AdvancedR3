#' Make summary table
#'
#' @param data
#'
#' @returns "A tibble"
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(across(
      value,
      list(mean = mean, sd = sd, median = median, iqr = IQR)
    )) |>
    dplyr::mutate(across(
      tidyselect::where(is.numeric),
      \(x) round(x, digits = 1)
    )) |>
    dplyr::mutate(
      MeanSD = glue::glue("{value_mean} ({value_sd})"),
      MedianIQR = glue::glue("{value_median} ({value_iqr})")
    ) |>
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD, "Median IQR" = MedianIQR)
}

#' Plot of metabolite distributions
#'
#' @param data
#'
#' @returns "A plot object"
create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Metabolites",
      y = "Counts"
    )
}

#' Clean data and average over duplicates
#'
#' @param data
#'
#' @returns "A data frame"
clean <- function(data) {
  data |>
    dplyr::group_by(pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}

#' Pre-processing the data
#'
#' @param data
#'
#' @returns "A data frame"
preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}
#' Fit data
#'
#' @param data
#' @param model
#'
#' @returns "Model results"
fit_model <- function(data, model) {
  glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = everything()
    )
}

#' Fit all models to a given data frame
#'
#' @param data
#'
#' @returns "Model results"
fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + gender + age
  ) |>
    purrr::map(\(model) fit_model(data, model = model)) |>
    purrr::list_rbind()
}

#' Create model results
#'
#' @param data
#'
#' @returns "Model results for all models on all metabolites"
create_model_results <- function(data) {
  data |>
    dplyr::group_split(metabolite) |>
    purrr::map(preprocess) |>
    purrr::map(fit_all_models) |>
    purrr::list_rbind()
}
