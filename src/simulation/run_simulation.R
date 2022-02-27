library(pacman)
pacman::p_load("magrittr", "dplyr", "readr", "progress")
options(dplyr.summarise.inform = FALSE)
options(readr.show_progress = FALSE)

run_simulation <- function(config) {
  
  out_path <- file.path(config$out_dir, "results.csv")
  
  pb <- progress::progress_bar$new(total = config$n_iterations)
  
  for (i in seq(config$n_iterations)) {
    run_one_iteration(config, i, out_path, (i > 1))
    pb$tick()
  }

}

run_one_iteration <- function(config, i, out_path, append) {
  
  out <- make_feature_matrix(config) %>%
    assign_rows_to_groups(config) %>%
    calculate_statistics(config) %>%
    dplyr::mutate(iter = i, .before = group)
  
  readr::write_csv(out, out_path, append = append)
  
  out
}
