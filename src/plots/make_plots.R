library(pacman)
pacman::p_load("ggplot2", "magrittr", "readr")

feature_dist_string <- function(config) {
  dist <- config$feature_spec$dist
  params <- config$feature_spec[names(config$feature_spec) != "dist"]
  fds <- paste0(
    dist, "(",
    paste(names(params), "=", params, collapse = ", "),
    ")"
  )
  fds
}

mean_rate_base_plot <- function(data) {
  data %>%
    ggplot2::ggplot(aes(x = sample_size, y = mean_feature_balance_rate)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = mean_feature_balance_rate - sqrt(var_feature_balance_rate),
          ymax = mean_feature_balance_rate + sqrt(var_feature_balance_rate)),
      alpha = 0.5)
}

mean_rate_plot_titles <- function(config) {
  labs(
    title = "Mean feature balance rate vs sample size",
    subtitle = paste("where each feature X_i ~",
                     feature_dist_string(config))
  )
}

mean_rate_linear_plot <- function(data, config) {
  mean_rate_base_plot(data) +
    ylab("mean feature balance rate") +
    xlab("sample size (linear scale)") +
    mean_rate_plot_titles(config)
}

mean_rate_log_plot <- function(data, config) {
  mean_rate_base_plot(data) +
    scale_x_log10() +
    ylab("mean feature balance rate") +
    xlab("sample size (log scale)") +
    mean_rate_plot_titles(config)
}

meets_thresh_base_plot <- function(data) {
  data %>%
    ggplot2::ggplot(aes(x = sample_size, y = pct_iters_meeting_threshold)) +
    geom_point()
}

meets_thresh_plot_titles <- function(config) {
  labs(
    title = paste("Percentage of trials having balance in at least",
                  round(config$threshold * 100, 2),
                  "% of features vs sample size"),
    subtitle = paste("where each feature X_i ~",
                     feature_dist_string(config))
  )
}

meets_thresh_linear_plot <- function(data, config) {
  meets_thresh_base_plot(data) +
    ylab("% of trials meeting threshold") +
    xlab("sample size (linear scale)") +
    meets_thresh_plot_titles(config)
}

meets_thresh_log_plot <- function(data, config) {
  meets_thresh_base_plot(data) +
    scale_x_log10() +
    ylab("% of trials meeting threshold") +
    xlab("sample size (log scale)") +
    meets_thresh_plot_titles(config)
}

# TODO: all plots together on 2x2 grid using patchwork(?)

generate_all_plots <- function(data, config, save) {
  
  p1 <- mean_rate_linear_plot(data, config)
  p2 <- mean_rate_log_plot(data, config)
  p3 <- meets_thresh_linear_plot(data, config)
  p4 <- meets_thresh_log_plot(data, config)
  
  if (save) {
    ggsave(file.path(config$out_dir, "figures", "mean_rate_linear_plot.png"),
           plot = p1, width = 8, height = 8)
    ggsave(file.path(config$out_dir, "figures", "mean_rate_log_plot.png"),
           plot = p2, width = 8, height = 8)
    ggsave(file.path(config$out_dir, "figures", "meets_thresh_linear_plot.png"),
           plot = p3, width = 8, height = 8)
    ggsave(file.path(config$out_dir, "figures", "meets_thresh_log_plot.png"),
           plot = p4, width = 8, height = 8)
  }
  
  plots <- list(p1, p2, p3, p4)
  plots
}

run_plot_generator <- function(config) {
  data <- readr::read_csv(file.path(config$out_dir, "overall_balance.csv"))
  p <- generate_all_plots(data, config, save = TRUE)
  invisible(p)
}
