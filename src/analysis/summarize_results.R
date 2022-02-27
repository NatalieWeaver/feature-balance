library(pacman)
pacman::p_load("magrittr", "dplyr", "readr")

summarize_results <- function(results, config, write = TRUE, append = FALSE) {
  
  summary <- results %>%
    dplyr::group_by(feature, .add = TRUE) %>%
    dplyr::summarize(
      min_mean = min(mean),
      max_mean = max(mean)
    ) %>%
    dplyr::mutate(
      diff = max_mean - min_mean,
      balanced = as.numeric(diff <= config$tolerance)
    )
  
  if (write) {
    readr::write_csv(
      summary,
      file.path(config$dirs$data, "summary.csv"),
      append = append
    )
  }
  
  summary
}