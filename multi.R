# Run multiple simulations using specification from config

library(pacman)
pacman::p_load("yaml")

source(file.path("src", "utilities.R"))

config <- yaml::read_yaml("config.yaml")
run_spec <- setup(config)

run_analyze_multi_simulations(run_spec)
run_plot_generator(run_spec)
