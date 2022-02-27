library(pacman)
pacman::p_load("magrittr", "dplyr", "readr")

describe_iter_balance <- function(summary, config, write = TRUE) {
  
  iter_balance <- summary %>%
    dplyr::summarize(
      feature_balance_rate = mean(balanced)
    ) %>%
    dplyr::mutate(
      meets_threshold = as.numeric(feature_balance_rate >= config$threshold)
    )
  
  if (write) {
    readr::write_csv(
      iter_balance,
      file.path(config$out_dir, "balance_by_iter.csv")
    )
  }
  
  iter_balance
}

describe_overall_balance <- function(iter_balance, config, append = FALSE) {
  
  overall_balance <- iter_balance %>%
    dplyr::summarize(
      mean_feature_balance_rate = mean(feature_balance_rate),
      var_feature_balance_rate = var(feature_balance_rate),
      pct_iters_meeting_threshold = mean(meets_threshold)
    )
  
  readr::write_csv(
    overall_balance,
    file.path(config$out_dir, "overall_balance.csv"),
    append = append
  )
  
  overall_balance
}