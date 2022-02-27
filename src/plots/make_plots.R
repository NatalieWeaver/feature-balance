library(pacman)
pacman::p_load("ggplot2")

mean_rate_base_plot <- function(data) {
  p <- data %>%
    ggplot2::ggplot(aes(x = sample_size, y = mean_feature_balance_rate)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = mean_feature_balance_rate - sqrt(var_feature_balance_rate),
          ymax = mean_feature_balance_rate + sqrt(var_feature_balance_rate)),
      alpha = 0.5)
  p
}

meets_thresh_base_plot <- function(data) {
  p <- data %>%
    ggplot2::ggplot(aes(x = sample_size, y = pct_iters_meeting_threshold)) +
    geom_point()
  p
}
