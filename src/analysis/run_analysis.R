run_analysis <- function(config) {
  
  results <- readr::read_csv(
    file.path(config$out_dir, "results.csv")
  )
  
  results %>%
    summarize_results(config) %>%
    describe_iter_balance(config) %>%
    describe_overall_balance(config)
}