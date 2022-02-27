run_analyze_multi_simulations <- function(config) {
  
  sample_sizes <- config$sample_size
  
  for (i in seq_along(sample_sizes)) {
    
    config$sample_size <- sample_sizes[[i]]
    message("running simulation for sample size ", config$sample_size)
    
    pb <- progress::progress_bar$new(total = config$n_iterations)
    
    for (j in seq(config$n_iterations)) {
      
      results <- run_one_iteration(config, j, write = FALSE, append = FALSE) %>%
        dplyr::mutate(sample_size = config$sample_size, .before = iter) %>%
        dplyr::group_by(sample_size, .add = FALSE) %>%
        summarize_results(config, write = FALSE) %>%
        describe_iter_balance(config, write = TRUE, append = (j > 1))
      
      pb$tick()
    }
    
    iter_balance <- readr::read_csv(
      file.path(config$dirs$data, "balance_by_iter.csv"),
      show_col_types = FALSE
    )
    
    iter_balance %>%
      dplyr::group_by(sample_size) %>%
      describe_overall_balance(config, write = TRUE, append = (i > 1))
  }
}
