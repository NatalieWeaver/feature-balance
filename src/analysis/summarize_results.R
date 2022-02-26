summarize_results <- function(config) {
  
  results <- readr::read_csv(
    file.path(config$out_dir, "results.csv")
  )
  
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