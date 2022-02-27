library(pacman)
pacman::p_load("tibble")

make_feature_matrix <- function(config, to_tibble = TRUE) {
  
  n <- config$sample_size
  p <- config$n_vars
  
  out <- matrix(
    data = get_dist(config)(n * p),
    nrow = n,
    ncol = p
  )
  
  if (to_tibble) {
    out <- tibble::as_tibble(out, .name_repair = make_names)
  }
  
  out
}

get_dist <- function(config) {
  
  if (config$feature_spec$dist == "binom") {
    function(n) {
      rbinom(n = n,
             size = config$feature_spec$size,
             prob = config$feature_spec$prob)
    }
  }
  
}

make_names <- function(vec) {
  paste("X", seq_along(vec), sep = "_")
}
