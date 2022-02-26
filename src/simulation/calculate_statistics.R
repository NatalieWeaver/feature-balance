calculate_statistics <- function(data, config) {
  
  data <- data %>%
    tidyr::pivot_longer(
      cols = 1:config$n_vars,
      names_to = "feature",
      values_to = "value"
    )
  
  data <- data %>%
    dplyr::group_by(group, feature) %>%
    dplyr::summarize(
      mean = mean(value)
    ) %>%
    dplyr::arrange(group, feature)
  
  data
}

get_statistics <- function(config) {
  NULL
}
