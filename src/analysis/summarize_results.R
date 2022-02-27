library(pacman)
pacman::p_load("magrittr", "dplyr", "readr")

summarize_results <- function(results, config) {
  
  summary <- results %>%
    dplyr::group_by(iter, feature) %>%
    dplyr::summarize(
      min_mean = min(mean),
      max_mean = max(mean)
    ) %>%
    dplyr::mutate(
      diff = max_mean - min_mean,
      balanced = as.numeric(diff <= config$tolerance)
    )
  
  readr::write_csv(summary, file.path(config$out_dir, "summary.csv"))
  
  summary
}