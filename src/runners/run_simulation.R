library(pacman)
pacman::p_load("magrittr", "dplyr", "readr", "progress")
options(dplyr.summarise.inform = FALSE)
options(readr.show_progress = FALSE)

run_simulation <- function(config) {
  
  if (length(config$sample_size > 1)) {
    stop("For single runs, specify only one sample size")
  }
  
  pb <- progress::progress_bar$new(total = config$n_iterations)
  
  for (i in seq(config$n_iterations)) {
    run_one_iteration(config, i, write = TRUE, append = (i > 1))
    pb$tick()
  }

}

run_one_iteration <- function(config, i, write = TRUE, append = FALSE) {
  
  out <- make_feature_matrix(config) %>%
    assign_rows_to_groups(config) %>%
    calculate_statistics(config) %>%
    dplyr::mutate(iter = i, .before = group)
  
  if (write) {
    readr::write_csv(
      out,
      file.path(config$dirs$data, "results.csv"),
      append = append)
  }
  
  out
}
