library(readr)
library(dplyr)
library(ggplot2)

read_project_csv <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    read_csv(file.path("output", path), show_col_types = FALSE)
  }
}

summary <- read_project_csv("single_item_christians_subsampling_summary_disjoint.csv") %>%
  mutate(
    method_label = case_when(
      estimator == "human_only" ~ "Human-only",
      estimator == "llm_only" ~ "LLM-only",
      estimator == "naive_pooling" ~ "Naive pooling",
      estimator == "ppi_disjoint" ~ "PPI/disjoint",
      estimator == "bayes_shrinkage_disjoint" ~ paste0("Bayes tau=", tau),
      TRUE ~ estimator
    ),
    method_label = factor(
      method_label,
      levels = c(
        "Human-only", "LLM-only", "Naive pooling", "PPI/disjoint",
        "Bayes tau=2", "Bayes tau=5", "Bayes tau=10", "Bayes tau=20"
      )
    )
  )

base_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

p_rmse <- ggplot(
  summary,
  aes(x = anchor_n_per_group, y = rmse, color = method_label)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(summary$anchor_n_per_group))) +
  labs(
    title = "RMSE by Anchor Size for Christians Item",
    subtitle = "Repeated paired-anchor subsampling; PPI/Bayes use non-anchor LLM proxy",
    x = "Anchor sample size per subgroup",
    y = "RMSE",
    color = NULL
  ) +
  base_theme

dir.create(file.path("figures", "figs_ggplot"), showWarnings = FALSE, recursive = TRUE)
ggsave(file.path("figures", "figs_ggplot", "christians_subsampling_rmse_disjoint.png"), p_rmse, width = 7.4, height = 4.6, dpi = 300)

plot_data <- summary %>%
  filter(method_label %in% c(
    "Human-only", "PPI/disjoint", "Bayes tau=5",
    "Bayes tau=10", "Bayes tau=20"
  )) %>%
  select(anchor_n_per_group, method_label, empirical_coverage, mean_interval_width) %>%
  tidyr::pivot_longer(
    cols = c(empirical_coverage, mean_interval_width),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      empirical_coverage = "Empirical coverage",
      mean_interval_width = "Mean interval width"
    )
  )

p_cov_width <- ggplot(
  plot_data,
  aes(x = anchor_n_per_group, y = value, color = method_label)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = sort(unique(summary$anchor_n_per_group))) +
  labs(
    title = "Coverage and Interval Width by Anchor Size",
    subtitle = "Shown for human-only, disjoint PPI, and selected Bayesian shrinkage priors",
    x = "Anchor sample size per subgroup",
    y = NULL,
    color = NULL
  ) +
  base_theme

ggsave(file.path("figures", "figs_ggplot", "christians_subsampling_coverage_width_disjoint.png"), p_cov_width, width = 7.4, height = 6.2, dpi = 300)
