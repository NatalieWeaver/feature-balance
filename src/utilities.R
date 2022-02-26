# Utility functions for simulation runs

setup <- function(config) {
  
  validate_config(config)
  run_spec <- parse_config(config)
  
  source_required_scripts()
  make_directories(run_spec)
  
  yaml::write_yaml(run_spec, file.path(config$out_dir, "run_spec.yaml"))
  
  run_spec
}