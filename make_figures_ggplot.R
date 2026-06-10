# make_figures_ggplot.R
# Run from the project root. Figures are written to figs_ggplot/.

suppressMessages({
  library(ggplot2); library(dplyr); library(readr)
  library(tidyr); library(forcats); library(scales)
  library(colorspace); library(ggforce)
})
source("theme_st559.R")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

BENCH      <- -3.7164
DELTA_FULL <- -3.561

read_project_csv <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    read_csv(file.path("output", path), show_col_types = FALSE)
  }
}

summ  <- read_project_csv("single_item_christians_subsampling_summary_disjoint.csv")
draws <- read_project_csv("single_item_christians_subsampling_draws_disjoint.csv")

lab_est <- function(est, tau) dplyr::case_when(
  est == "human_only" ~ "Human-only",
  est == "llm_only" ~ "LLM-only",
  est == "ppi_disjoint" ~ "PPI",
  est == "ppi_plus" ~ "PPI++",
  est == "bayes_shrinkage_disjoint" ~ paste0("Bayes \u03c4=", tau),
  TRUE ~ est
)

# A1. Repeated-sampling estimates
d100 <- draws %>%
  filter(anchor_n_per_group == 100,
         estimator %in% c("llm_only","human_only","ppi_disjoint","ppi_plus",
                          "bayes_shrinkage_disjoint"),
         is.na(tau) | tau %in% c(2,5,20)) %>%
  mutate(lab = lab_est(estimator, tau),
         lab = fct_relevel(lab, "LLM-only","Human-only","PPI","PPI++",
                           "Bayes \u03c4=2","Bayes \u03c4=5","Bayes \u03c4=20"))
colmap <- c("LLM-only"=PAL[["llm"]],"Human-only"=PAL[["human"]],"PPI"=PAL[["ppi"]],
            "PPI++"=PAL[["ppi_plus"]],"Bayes \u03c4=2"=PAL_TAU[["2"]],
            "Bayes \u03c4=5"=PAL[["bayes"]],"Bayes \u03c4=20"=PAL[["aux"]])

pA1 <- ggplot(d100, aes(estimate, lab, fill = lab)) +
  geom_violin(colour = NA, alpha = 0.85, scale = "width", width = 0.95) +
  stat_summary(fun = median, geom = "point", colour = "white", size = 1.6) +
  geom_vline(xintercept = BENCH, colour = PAL[["benchmark"]],
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = BENCH, y = Inf, label = "benchmark  -3.72",
           colour = PAL[["benchmark"]], hjust = 1.05, vjust = 1.5, size = 3.4) +
  scale_fill_manual(values = colmap, guide = "none") +
  labs(title = wrap_title("LLM-only is a confident spike in the wrong place"),
       subtitle = "Sampling distribution over 500 reps (Christians, n=100)",
       x = "Estimated education gap", y = NULL) +
  theme_st559() + theme(panel.grid.major.y = element_blank())
save_fig(pA1, "A1_violin_report.png", w = 8.4, h = 5.2)

# A1b. Estimate intervals for key methods
d100f <- draws %>%
  filter(anchor_n_per_group == 100,
         estimator %in% c("llm_only","human_only","ppi_disjoint","ppi_plus",
                          "bayes_shrinkage_disjoint"),
         is.na(tau) | tau == 5) %>%
  mutate(lab = lab_est(estimator, tau)) %>%
  group_by(lab) %>%
  summarise(med = median(estimate), lo = quantile(estimate, 0.05),
            hi = quantile(estimate, 0.95), .groups = "drop") %>%
  mutate(lab = fct_relevel(lab, "LLM-only","Human-only","PPI","PPI++","Bayes \u03c4=5"),
         highlight = lab == "Bayes \u03c4=5")
fcol <- c("LLM-only"=PAL[["llm"]],"Human-only"=PAL[["human"]],"PPI"=PAL[["ppi"]],
          "PPI++"=PAL[["ppi_plus"]],"Bayes \u03c4=5"=PAL[["bayes"]])

pA1b <- ggplot(d100f, aes(y = lab, colour = lab)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25,
                 linewidth = 0.7, show.legend = FALSE) +
  geom_point(aes(x = med, shape = highlight, size = highlight), show.legend = FALSE) +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16)) +
  scale_size_manual(values = c("TRUE" = 6, "FALSE" = 3.5)) +
  geom_vline(xintercept = BENCH, colour = PAL[["benchmark"]],
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = BENCH, y = Inf, label = "benchmark  -3.72",
           colour = PAL[["benchmark"]], hjust = 1.05, vjust = 1.5, size = 3.4) +
  geom_text(aes(x = hi + 0.3, label = sprintf("width %.1f", hi - lo)),
            hjust = 0, size = 3, colour = "#999999") +
  scale_colour_manual(values = fcol) +
  coord_cartesian(clip = "off") +
  labs(title = wrap_title("LLM-only is precise but biased; Bayes \u03c4=5 gets closest"),
       subtitle = "Median \u00b1 90% of 500 reps (Christians, n=100)",
       x = "Estimated education gap", y = NULL) +
  theme_st559() + theme(panel.grid.major.y = element_blank(),
                        panel.grid.major.x = element_line(colour = "#eeeeee"),
                        plot.margin = margin(16, 50, 12, 12))
save_fig(pA1b, "A1b_forest_slide.png", w = 8.4, h = 4.0)

# A2. Bias-variance decomposition
bv_n100 <- draws %>%
  filter(
    anchor_n_per_group == 100,
    estimator %in% c(
      "llm_only",
      "ppi_disjoint",
      "ppi_plus",
      "human_only",
      "bayes_shrinkage_disjoint"
    ),
    is.na(tau) | tau %in% c(2, 5, 20)
  ) %>%
  group_by(estimator, tau) %>%
  summarise(
    bias2 = (mean(estimate) - BENCH)^2,
    variance = var(estimate),
    .groups = "drop"
  ) %>%
  mutate(
    lab = lab_est(estimator, tau),
    mse = bias2 + variance,
    lab = fct_relevel(
      lab,
      "LLM-only",
      "PPI",
      "PPI++",
      "Bayes τ=2",
      "Bayes τ=5",
      "Bayes τ=20",
      "Human-only"
    ),
    highlight = grepl("Bayes.*5", lab)
  )

label_gap <- 0.9
mse_x <- max(bv_n100$variance, na.rm = TRUE) + 2.6

pA2 <- ggplot(bv_n100) +
  geom_segment(
    aes(y = lab, yend = lab, x = -bias2, xend = variance),
    colour = "#d0d0d0",
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = lab, x = -bias2, size = highlight),
    colour = "#C76B15",
    show.legend = FALSE
  ) +
  geom_point(
    aes(y = lab, x = variance, size = highlight),
    colour = PAL[["ppi"]],
    show.legend = FALSE
  ) +
  scale_size_manual(values = c("TRUE" = 6, "FALSE" = 4)) +
  
  geom_text(
    data = bv_n100 %>%
      filter(bias2 > 0.3) %>%
      mutate(label_x = -bias2 - label_gap),
    aes(y = lab, x = label_x, label = sprintf("%.1f", bias2)),
    colour = "#C76B15",
    fontface = "bold",
    size = 3,
    hjust = 1
  ) +
  
  geom_text(
    data = bv_n100 %>%
      filter(variance > 0.3) %>%
      mutate(label_x = variance + label_gap),
    aes(y = lab, x = label_x, label = sprintf("%.1f", variance)),
    colour = PAL[["ppi"]],
    fontface = "bold",
    size = 3,
    hjust = 0
  ) +
  
  geom_text(
    aes(y = lab, x = mse_x, label = sprintf("MSE=%.1f", mse)),
    colour = "#999999",
    size = 2.8,
    hjust = 0
  ) +
  
  geom_point(
    data = bv_n100 %>% filter(highlight),
    aes(y = lab, x = -bias2),
    shape = 21,
    size = 8,
    colour = PAL[["bayes"]],
    fill = NA,
    stroke = 1.2
  ) +
  geom_point(
    data = bv_n100 %>% filter(highlight),
    aes(y = lab, x = variance),
    shape = 21,
    size = 8,
    colour = PAL[["bayes"]],
    fill = NA,
    stroke = 1.2
  ) +
  
  geom_vline(xintercept = 0, colour = "#bbbbbb", linewidth = 0.6) +
  coord_cartesian(
    xlim = c(
      -max(bv_n100$bias2, na.rm = TRUE) - 1.4,
      mse_x + 1.6
    ),
    clip = "off"
  ) +
  labs(
    title = wrap_title("Bayes τ=5 minimises both bias and variance (n=100)"),
    subtitle = "← bias² (warm)                              variance (cool) →",
    x = NULL,
    y = NULL
  ) +
  theme_st559() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#eeeeee"),
    axis.text.x = element_blank(),
    plot.margin = margin(16, 70, 12, 12)
  )

save_fig(pA2, "A2_butterfly.png", w = 9.2, h = 4.5)



# A3. Calibration by anchor size
levels_seq <- seq(0.5, 0.99, by = 0.02)
cal_one <- function(est, tau = NA) {
  d <- draws %>% filter(anchor_n_per_group == 100, estimator == est)
  if (!is.na(tau)) d <- d %>% filter(tau == !!tau)
  sd_v <- sqrt(d$variance); e <- d$estimate
  tibble(level = levels_seq,
         emp = sapply(levels_seq, function(L) {
           z <- qnorm(0.5 + L/2); mean(abs(e - BENCH) <= z * sd_v) }),
         lab = lab_est(est, tau))
}
cal <- bind_rows(
  cal_one("llm_only"), cal_one("human_only"), cal_one("ppi_disjoint"),
  cal_one("ppi_plus"), cal_one("bayes_shrinkage_disjoint", 5),
  cal_one("bayes_shrinkage_disjoint", 2)
) %>% mutate(lab = factor(lab))
calcol <- c("LLM-only"=PAL[["llm"]],"Human-only"=PAL[["human"]],"PPI"=PAL[["ppi"]],
            "PPI++"=PAL[["ppi_plus"]],"Bayes \u03c4=5"=PAL[["bayes"]],
            "Bayes \u03c4=2"=PAL_TAU[["2"]])

pA3 <- ggplot(cal, aes(level, emp, colour = lab)) +
  geom_abline(slope = 1, intercept = 0, colour = PAL[["grayline"]], linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = calcol) +
  coord_equal(xlim = c(0.5, 1), ylim = c(0, 1)) +
  labs(title = wrap_title("Honest uncertainty tracks the diagonal"),
       subtitle = "Empirical vs nominal coverage at n=100 (500 reps)",
       x = "Nominal coverage level", y = "Empirical coverage") +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pA3, "A3_calibration.png", w = 6.6, h = 6.2)


# A4. Shrinkage weight by tau and anchor size

lam <- draws %>%
  filter(estimator == "bayes_shrinkage_disjoint") %>%
  group_by(tau, anchor_n_per_group) %>%
  summarise(lambda = mean(lambda, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    row = paste0("tau=", tau),
    group = "Bayes"
  )

lam_pp <- draws %>%
  filter(estimator == "ppi_plus") %>%
  group_by(anchor_n_per_group) %>%
  summarise(lambda = mean(lambda, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    row = "lambda*",
    tau = NA,
    group = "PPI++"
  )

row_levels <- c("lambda*", "tau=20", "tau=10", "tau=5", "tau=2")
x_levels <- c(25, 50, 100, 200)

# Muted orange-white-blue scale
lambda_cols <- c(
  "#DE8E22",
  "#E7B45D",
  "#F7F3E8",
  "#D9DEDA",
  "#AFC8D2",
  "#759EBA",
  "#5B7FA1",
  "#43658B",
  "#334F74",
  "#263E60",
  "#1B2F4D",
  "#12223D"
)

lambda_vals <- c(0, .20, .45, .60, .72, .83, .87, .91, .94, .96, .99, 1)

lam_all <- bind_rows(lam, lam_pp) %>%
  filter(is.finite(lambda)) %>%
  mutate(
    lambda = pmin(pmax(lambda, 0), 1),
    row = factor(row, levels = row_levels),
    x_pos = match(anchor_n_per_group, x_levels),
    y_pos = as.numeric(row),
    text_col = if_else(lambda >= 0.91, "white", "dark")
  ) %>%
  filter(!is.na(row), !is.na(x_pos))

row_axis <- tibble(
  row = factor(row_levels, levels = row_levels),
  y_pos = seq_along(row_levels)
)

cell_poly <- lam_all %>%
  mutate(
    xmin = x_pos - 0.43,
    xmax = x_pos + 0.43,
    ymin = y_pos - 0.43,
    ymax = y_pos + 0.43,
    cell_id = paste(row, anchor_n_per_group, sep = "_")
  ) %>%
  tidyr::crossing(corner = 1:4) %>%
  mutate(
    x = case_when(
      corner == 1 ~ xmin,
      corner == 2 ~ xmax,
      corner == 3 ~ xmax,
      TRUE ~ xmin
    ),
    y = case_when(
      corner == 1 ~ ymin,
      corner == 2 ~ ymin,
      corner == 3 ~ ymax,
      TRUE ~ ymax
    )
  )

pA4 <- ggplot() +
  ggforce::geom_shape(
    data = cell_poly,
    aes(x = x, y = y, group = cell_id, fill = lambda),
    radius = grid::unit(0.09, "in"),
    expand = grid::unit(0, "pt"),
    colour = NA
  ) +
  geom_text(
    data = lam_all,
    aes(x = x_pos, y = y_pos, label = sprintf("%.2f", lambda), colour = text_col),
    size = 2.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_segment(aes(x = 0.48, xend = 0.48, y = 1.57, yend = 5.43),
               linewidth = 0.35, colour = "#777777") +
  geom_segment(aes(x = 0.48, xend = 0.56, y = 5.43, yend = 5.43),
               linewidth = 0.35, colour = "#777777") +
  geom_segment(aes(x = 0.48, xend = 0.56, y = 1.57, yend = 1.57),
               linewidth = 0.35, colour = "#777777") +
  geom_text(aes(x = 0.34, y = 3.78, label = "Bayes"),
            angle = 90, size = 2.25, colour = "#666666") +
  geom_segment(aes(x = 0.48, xend = 0.48, y = 0.57, yend = 1.43),
               linewidth = 0.35, colour = "#777777") +
  geom_segment(aes(x = 0.48, xend = 0.56, y = 1.43, yend = 1.43),
               linewidth = 0.35, colour = "#777777") +
  geom_segment(aes(x = 0.48, xend = 0.56, y = 0.57, yend = 0.57),
               linewidth = 0.35, colour = "#777777") +
  geom_text(aes(x = 0.34, y = 1.12, label = "PPI++"),
            angle = 90, size = 2.25, colour = "#666666") +
  annotate("text", x = 2.5, y = 0.22, label = "Anchor n per subgroup",
           size = 2.25, colour = "#666666") +
  scale_fill_gradientn(
    colours = lambda_cols,
    values = lambda_vals,
    limits = c(0, 1),
    breaks = c(0, .25, .5, .75, 1),
    labels = c("0", ".25", ".5", ".75", "1"),
    name = expression(lambda),
    guide = guide_colourbar(
      barheight = grid::unit(1.15, "in"),
      barwidth = grid::unit(0.08, "in"),
      ticks = TRUE,
      title.position = "top"
    )
  ) +
  scale_colour_manual(
    values = c("white" = "white", "dark" = PAL[["ink"]]),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq_along(x_levels),
    labels = x_levels,
    limits = c(0.25, 4.55),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_y_continuous(
    breaks = row_axis$y_pos,
    labels = row_axis$row,
    limits = c(0.15, 5.55),
    expand = c(0, 0)
  ) +
  coord_fixed(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_st559(base_size = 8.8) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(margin = margin(b = 1), colour = "#666666"),
    axis.text.y = element_text(margin = margin(r = 1), colour = "#666666"),
    legend.position = "right",
    legend.title = element_text(size = 7.5),
    legend.text = element_text(size = 7),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(2, 4, 2, 1)
  )

save_fig(pA4, "A4_lambda_heatmap.png", w = 4.15, h = 2.7)

# A5. Pooling comparison
ps <- read_project_csv("pooling_sensitivity_christians_rep011.csv")
order_pool <- c("No pooling","Partial pooling","Complete pooling")
pcol2 <- c("No pooling"=PAL[["human"]],"Partial pooling"=PAL[["bayes"]],
           "Complete pooling"=PAL[["llm"]])
ps2 <- ps %>% mutate(method = fct_relevel(method, order_pool))

density_grid <- do.call(rbind, lapply(seq_len(nrow(ps2)), function(i) {
  x <- seq(ps2$mean[i] - 4 * ps2$sd[i], ps2$mean[i] + 4 * ps2$sd[i], length.out = 400)
  data.frame(method = ps2$method[i], x = x, density = dnorm(x, ps2$mean[i], ps2$sd[i]))
}))

pA5 <- ggplot(density_grid, aes(x, density, colour = method, fill = method)) +
  geom_area(alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = DELTA_FULL, colour = PAL[["benchmark"]],
             linetype = "dashed", linewidth = 0.7) +
  scale_colour_manual(values = pcol2) +
  scale_fill_manual(values = pcol2) +
  labs(title = wrap_title("Pooling changes both center and uncertainty"),
       subtitle = "Approximate posterior densities for the Christians correction",
       x = expression("Christians correction  "*Delta), y = "Density") +
  theme_st559()
save_fig(pA5, "A5_posterior_density.png", w = 7.2, h = 4.8)

pA5b <- ggplot(ps2, aes(y = method, colour = method)) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.25,
                 linewidth = 0.8, show.legend = FALSE) +
  geom_point(aes(x = mean), size = 5, show.legend = FALSE) +
  annotate("curve", x = ps2$mean[1], xend = ps2$mean[2], y = 1, yend = 2,
           curvature = -0.3, arrow = arrow(length = unit(6, "pt")),
           colour = PAL[["deeporange"]], linewidth = 0.8) +
  annotate("curve", x = ps2$mean[2], xend = ps2$mean[3], y = 2, yend = 3,
           curvature = -0.3, arrow = arrow(length = unit(6, "pt")),
           colour = PAL[["deeporange"]], linewidth = 0.8) +
  geom_vline(xintercept = DELTA_FULL, colour = PAL[["benchmark"]],
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = DELTA_FULL, y = Inf, label = "full correction  -3.56",
           colour = PAL[["benchmark"]], hjust = 1.05, vjust = 1.5, size = 3.4) +
  geom_vline(xintercept = 0, colour = "#d0d0d0", linewidth = 0.5) +
  geom_text(aes(x = high + 0.25, label = sprintf("width %.1f", high - low)),
            hjust = 0, size = 3, colour = "#999999") +
  scale_colour_manual(values = pcol2) +
  coord_cartesian(clip = "off") +
  labs(title = wrap_title("Pooling narrows intervals but pulls Christians toward the item mean"),
       subtitle = "Christians correction posterior (95% interval, rep 11); red arrows show shrinkage",
       x = expression("Christians LLM\u2013human correction  "*Delta), y = NULL) +
  theme_st559() + theme(panel.grid.major.y = element_blank(),
                        panel.grid.major.x = element_line(colour = "#eeeeee"),
                        plot.margin = margin(16, 50, 12, 12))
save_fig(pA5b, "A5b_pooling_forest_slide.png", w = 8.4, h = 3.8)

# A6. Shrinkage geometry
sg <- read_project_csv("hier_summary_grid_rep011.csv")
mu_mean <- 0.327
lims <- c(min(sg$delta_hat_j) - 1, max(sg$delta_hat_j) + 1)
pA6 <- ggplot(sg) +
  geom_abline(slope = 1, intercept = 0, colour = PAL[["grayline"]], linewidth = 0.9) +
  geom_hline(yintercept = mu_mean, colour = "#777777", linetype = "dotted") +
  annotate("text", x = lims[2], y = mu_mean, label = "\u03bc", hjust = 1.4, vjust = -0.4,
           colour = "#777777", size = 4) +
  geom_segment(aes(x = delta_hat_j, xend = delta_hat_j,
                   y = delta_hat_j, yend = partial_pool_mean,
                   colour = role == "target"),
               arrow = arrow(length = unit(6, "pt")), linewidth = 0.8) +
  geom_point(aes(delta_hat_j, partial_pool_mean, colour = role == "target",
                 size = role == "target")) +
  scale_colour_manual(values = c("TRUE"=PAL[["benchmark"]],"FALSE"="#777777"), guide="none") +
  scale_size_manual(values = c("TRUE"=4,"FALSE"=2.3), guide = "none") +
  coord_equal(xlim = lims, ylim = lims) +
  labs(title = wrap_title("Partial pooling pulls every item toward \u03bc"),
       subtitle = "No-pooling vs partial-pooling posterior mean (rep 11); Christians in red",
       x = expression(hat(Delta)[j]*" (no pooling)"),
       y = expression(E*"["*Delta[j]*"|data] (partial pooling)")) +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pA6, "A6_shrinkage_geometry.png", w = 6.8, h = 6.6)

# A7. Posterior predictive intervals
ppc <- read_project_csv("hier_ppc_summary_rep011.csv") %>%
  arrange(desc(observed_delta_hat)) %>%
  mutate(
    item = factor(item, levels = rev(item)),
    role = if ("role" %in% names(.)) role else "pool",
    is_target = role == "target"
  )

pA7 <- ggplot(ppc, aes(y = item)) +
  geom_vline(
    xintercept = 0,
    colour = "#c8c8c8",
    linewidth = 0.55
  ) +
  
  geom_segment(
    aes(x = ppc_low_90, xend = ppc_high_90, yend = item),
    linewidth = 3.0,
    colour = "#D9E9F5",
    lineend = "round"
  ) +
  geom_segment(
    aes(
      x = ppc_low_90,
      xend = ppc_high_90,
      yend = item,
      colour = is_target
    ),
    linewidth = 0.75,
    lineend = "round"
  ) +
  
  geom_segment(
    aes(
      x = ppc_median,
      xend = ppc_median,
      y = as.numeric(item) - 0.22,
      yend = as.numeric(item) + 0.22
    ),
    colour = "#2F5B81",
    linewidth = 0.75
  ) +
  
  geom_point(
    aes(x = observed_delta_hat, fill = is_target, size = is_target),
    shape = 21,
    colour = "white",
    stroke = 0.85,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c("FALSE" = "#6699CC", "TRUE" = PAL[["deeporange"]]),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("FALSE" = PAL[["deeporange"]], "TRUE" = PAL[["deeporange"]]),
    guide = "none"
  ) +
  scale_size_manual(
    values = c("FALSE" = 2.4, "TRUE" = 4.2),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(-8, -4, 0, 4, 8),
    limits = c(-8.2, 8.2),
    expand = c(0, 0)
  ) +
  labs(
    title = wrap_title("Observed item corrections fall inside replicated intervals"),
    subtitle = "Thin line = 90% replicated interval; tick = PPC median; dot = observed correction",
    x = expression("replicated " * hat(Delta)[j]^{rep}),
    y = NULL
  ) +
  theme_st559(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#eeeeee", linewidth = 0.5),
    axis.ticks = element_blank(),
    plot.margin = margin(10, 16, 10, 10)
  )

save_fig(pA7, "A7_ppc_intervals.png", w = 7.2, h = 4.8)

# A8. RMSE and coverage
sm <- summ %>%
  filter(estimator %in% c("human_only","ppi_disjoint","ppi_plus",
                          "bayes_shrinkage_disjoint"),
         is.na(tau) | tau == 5) %>%
  mutate(lab = lab_est(estimator, tau)) %>%
  select(anchor_n_per_group, lab, rmse, empirical_coverage) %>%
  pivot_longer(c(rmse, empirical_coverage), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, rmse = "RMSE",
                         empirical_coverage = "Empirical coverage"))
smcol <- c("Human-only"=PAL[["human"]],"PPI"=PAL[["ppi"]],"PPI++"=PAL[["ppi_plus"]],
           "Bayes \u03c4=5"=PAL[["bayes"]])
pA8 <- ggplot(sm, aes(anchor_n_per_group, value, colour = lab)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = smcol) +
  scale_x_log10(breaks = c(25,50,100,200)) +
  labs(title = wrap_title("Bayes \u03c4=5: best coverage\u2013RMSE trade-off"),
       subtitle = "Single-item Christians, by anchor size",
       x = "Anchor sample size per subgroup", y = NULL) +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pA8, "A8_rmse_coverage.png", w = 7.6, h = 6.4)

cat("Done. All figures in figures/figs_ggplot/\n")
cat("  Slide figures: A1b, A4, A5b (SLIDE_MODE strips titles)\n")
cat("  Report figures: A1, A2, A3, A5, A6, A7, A8\n")

# B1. Hierarchical shrinkage diagnostic
pFunnel <- ggplot(sg) +
  geom_segment(aes(x = delta_hat_j, xend = partial_pool_mean,
                   y = se_j, yend = partial_pool_sd,
                   colour = role == "target", alpha = role == "target"),
               arrow = arrow(length = unit(5,"pt")), linewidth = 0.7, show.legend = FALSE) +
  geom_point(aes(delta_hat_j, se_j, colour = role == "target"),
             alpha = 0.25, size = 3, show.legend = FALSE) +
  geom_point(aes(partial_pool_mean, partial_pool_sd, colour = role == "target",
                 size = role == "target"), show.legend = FALSE) +
  geom_vline(xintercept = 0.327, colour = "#cccccc", linetype = "dotted") +
  annotate("text", x = 0.327, y = min(sg$partial_pool_sd) - 0.1,
           label = "\u03bc", colour = "#999999", size = 4) +
  scale_colour_manual(values = c("TRUE" = "#C76B15", "FALSE" = "#6A89A7")) +
  scale_alpha_manual(values = c("TRUE" = 0.95, "FALSE" = 0.55)) +
  scale_size_manual(values = c("TRUE" = 6, "FALSE" = 3.5)) +
  scale_y_reverse() +
  labs(title = wrap_title("Items with more uncertainty get pulled harder toward \u03bc"),
       subtitle = "Funnel plot: arrows show shrinkage from no-pooling to partial-pooling (rep 11)",
       x = expression("Posterior mean of "*Delta[j]),
       y = expression("Posterior SD of "*Delta[j]*" (inverted)")) +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pFunnel, "ADV1_funnel.png", w = 7.5, h = 5.5)

# B2. Risk curves for shrinkage weights
V_delta_est <- mean(draws$variance[draws$estimator == "human_only" &
                                    draws$anchor_n_per_group == 100])
risk_df <- expand.grid(lambda = seq(0, 1, length.out = 200), tau = c(2, 5, 10)) %>%
  mutate(R = (1 - lambda)^2 * tau^2 + lambda^2 * V_delta_est,
         lab = paste0("\u03c4=", tau))
optima <- data.frame(tau = c(2, 5, 10)) %>%
  mutate(lam_B = tau^2 / (tau^2 + V_delta_est),
         R_B = (1 - lam_B)^2 * tau^2 + lam_B^2 * V_delta_est,
         lab = paste0("\u03c4=", tau))

rcol <- c("\u03c4=2" = PAL_TAU[["2"]], "\u03c4=5" = PAL[["bayes"]], "\u03c4=10" = PAL[["llm"]])

pRisk <- ggplot(risk_df, aes(lambda, R, colour = lab)) +
  geom_line(linewidth = 1.4) +
  geom_point(data = optima, aes(lam_B, R_B), shape = 18, size = 5,
             show.legend = FALSE) +
  geom_text(data = optima, aes(lam_B, R_B,
            label = sprintf("\u03bb*=%.2f", lam_B)),
            vjust = -1.2, size = 3.3, show.legend = FALSE) +
  geom_vline(xintercept = 1.0, colour = PAL[["ppi"]], linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 0.99, y = Inf, label = "PPI  \u03bb=1",
           colour = PAL[["ppi"]], hjust = 1, vjust = 1.5, size = 3.3) +
  geom_vline(xintercept = 0.94, colour = PAL[["ppi_plus"]], linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 0.93, y = Inf, label = "PPI++  \u03bb*\u22480.94",
           colour = PAL[["ppi_plus"]], hjust = 1, vjust = 3, size = 3.3) +
  scale_colour_manual(values = rcol) +
  coord_cartesian(ylim = c(0, max(risk_df$R[risk_df$tau == 10]) * 1.1), clip = "off") +
  labs(title = wrap_title("Each \u03c4 defines a risk parabola; the minimum is the Bayes-optimal \u03bb"),
       subtitle = "R(\u03bb) = (1\u2212\u03bb)\u00b2\u03c4\u00b2 + \u03bb\u00b2V\u0394  at n=100",
       x = expression("Correction weight "*lambda),
       y = expression("Prior-averaged risk  R("*lambda*")")) +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pRisk, "ADV2_risk_surface.png", w = 8, h = 5)

# B3. Joint posterior over mu and sigma_delta
mu_seq  <- seq(-5, 5, length.out = 201)
sig_seq <- seq(0.05, 8, length.out = 201)
grid_df <- expand.grid(mu = mu_seq, sig = sig_seq)
yhat <- sg$delta_hat_j; Vj <- sg$se_j^2

logpost <- rep(0, nrow(grid_df))
for (j in seq_along(yhat)) {
  vv <- Vj[j] + grid_df$sig^2
  logpost <- logpost - 0.5 * log(2 * pi * vv) - 0.5 * (yhat[j] - grid_df$mu)^2 / vv
}
logpost <- logpost + dnorm(grid_df$mu, 0, 10, log = TRUE)
logpost <- logpost + dnorm(grid_df$sig, 0, 10, log = TRUE)
logpost <- logpost - max(logpost)
grid_df$post <- exp(logpost)

pJoint <- ggplot(grid_df, aes(mu, sig, z = post)) +
  geom_contour_filled(bins = 9, show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  geom_contour(colour = PAL[["bayes"]], linewidth = 0.4, alpha = 0.5, bins = 9) +
  labs(title = wrap_title("Joint posterior of \u03bc and \u03c3\u0394 (rep 11)"),
       subtitle = "Classic hierarchical geometry: mode near small \u03c3\u0394, long tail upward",
       x = expression(mu*" (cross-item mean correction)"),
       y = expression(sigma[Delta]*" (cross-item SD)")) +
  theme_st559() + theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
save_fig(pJoint, "ADV3_joint_posterior.png", w = 7, h = 5.5)

cat("Advanced figures: ADV1 funnel, ADV2 risk, ADV3 joint posterior\n")


cat("A2b single-panel slide scatter added.\n")
