assign_rows_to_groups <- function(data, config) {
  
  if ((config$sample_size %% config$n_groups) != 0) {
    warning("n_groups does not divide into sample_size; ",
            "groups will have different numbers of observations")
  }
  
  group_size <- ceiling(config$sample_size / config$n_groups)
  groups <- rep(1:config$n_groups, group_size)
  
  data[["group"]] <- sample(groups, size = config$sample_size, replace = FALSE)
  data
}
