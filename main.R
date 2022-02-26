# Run simulation using specification from config, and save results

library(pacman)
pacman::p_load("yaml")

source(file.path("src", "utilities.R"))

config <- yaml::read_yaml("config.yaml")
run_spec <- setup(config)

run_simulation(run_spec)

summarize_results(run_spec)
