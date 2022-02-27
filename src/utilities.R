# Utility functions for simulation runs

library(pacman)
pacman::p_load("yaml", "sessioninfo")

setup <- function(config) {
  
  source_required_scripts()
  
  validate_config(config,
                  list("sample_size", "n_groups", "n_vars", "feature_spec",
                       "n_iterations", "tolerance", "threshold", "out_dir"))
  
  run_spec <- parse_config(config)
  set.seed(run_spec$random_seed)
  
  make_directories(run_spec)
  
  yaml::write_yaml(run_spec, file.path(config$out_dir, "run_spec.yaml"))
  
  run_spec
}

source_required_scripts <- function() {
  files <- list.files(path = c(file.path("src", "simulation"),
                               file.path("src", "analysis")),
                      full.names = TRUE)
  sapply(files, source)
}

validate_config <- function(config, required_keys) {
  for (key in required_keys) {
    if (!(key %in% names(config))) {
      stop(key, " key is required but missing from config")
    }
  }
}

parse_config <- function(config) {
  
  run_spec <- config
  
  if (!("random_seed" %in% names(config))) {
    run_spec$random_seed <- sample(10000:99999, 1)
  }
  
  pkgs <- sessioninfo::package_info("loaded", include_base = TRUE)
  run_spec$session_info <- list(
    r_version = list(
      version_string = R.Version()$version.string,
      nickname = R.Version()$nickname,
      platform = R.Version()$platform
    ),
    locale = Sys.getlocale(),
    os_version = osVersion,
    rng_kind = RNGkind(),
    loaded_pkgs = paste(pkgs$package, pkgs$loadedversion, sep = "_")
  )
  
  run_spec
}

make_directories <- function(config) {
  if (!dir.exists(config$out_dir)) {
    dir.create(config$out_dir, recursive = TRUE)
  }
}
