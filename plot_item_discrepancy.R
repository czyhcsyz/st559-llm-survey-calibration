library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

read_project_csv <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    read_csv(file.path("output", path), show_col_types = FALSE)
  }
}

audit <- read_project_csv("item_audit_table.csv") %>%
  mutate(
    ci_low = correction_delta_hat - 2 * se_delta_hat,
    ci_high = correction_delta_hat + 2 * se_delta_hat,
    item = fct_reorder(item, correction_delta_hat),
    role = case_when(
      candidate_role == "tentative_main" ~ "Tentative main",
      candidate_role == "contrast" ~ "Contrast",
      candidate_role == "sensitivity" ~ "Sensitivity",
      TRUE ~ "Other"
    )
  )

p <- ggplot(audit, aes(x = correction_delta_hat, y = item, color = role)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.18, linewidth = 0.65) +
  geom_point(size = 2.4) +
  scale_color_manual(
    values = c(
      "Tentative main" = "#0072B2",
      "Contrast" = "#009E73",
      "Sensitivity" = "#D55E00",
      "Other" = "gray35"
    )
  ) +
  labs(
    title = "Estimated LLM-Human Correction by Thermometer Item",
    subtitle = "College minus non-college gap; intervals show approximately +/- 2 SE",
    x = expression(hat(Delta)[j] == hat(psi)[H,j] - hat(psi)[L,j]),
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )

dir.create(file.path("figures", "figs_ggplot"), showWarnings = FALSE, recursive = TRUE)
ggsave(file.path("figures", "figs_ggplot", "item_discrepancy_plot.png"), p, width = 7.2, height = 4.6, dpi = 300)
