# theme_st559.R
library(ggplot2)
suppressMessages(library(stringr))

PAL <- c(
  bayes      = "#1f3a5f",
  llm        = "#e08214",
  human      = "#b0b0b0",
  ppi        = "#6699cc",
  ppi_plus   = "#a3c4dc",
  benchmark  = "#b2182b",
  deeporange = "#C76B15",
  aux        = "#e6b800",
  ink        = "#1a1a1a",
  grayline   = "#d9d9d9"
)
PAL_TAU <- c("2" = "#bcbddc", "5" = "#1f3a5f", "10" = "#e08214", "20" = "#b2182b")

LAMBDA_LOW <- "#e08214"
LAMBDA_MID <- "#f7f7f7"
LAMBDA_HIGH <- "#1f3a5f"

BASE_FONT <- ""
SLIDE_MODE <- TRUE
FIG_DIR <- file.path("figures", "figs_ggplot")

wrap_title <- function(s, width = 48) str_wrap(s, width = width)

theme_st559 <- function(base_size = 14) {
  th <- theme_minimal(base_size = base_size, base_family = BASE_FONT) +
    theme(
      plot.title = element_text(
        face = "bold", size = base_size + 3, colour = PAL[["ink"]],
        hjust = 0, lineheight = 1.05, margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = base_size - 3, colour = "#777777",
        hjust = 0, margin = margin(b = 14)
      ),
      plot.title.position = "plot",
      plot.caption = element_text(size = base_size - 5, colour = "#aaaaaa", hjust = 0),
      axis.title = element_text(size = base_size - 3, colour = "#555555"),
      axis.text = element_text(size = base_size - 4, colour = "#666666"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#efefef", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.key.height = unit(10, "pt"),
      legend.text = element_text(size = base_size - 4, colour = "#555555"),
      plot.margin = margin(16, 22, 12, 12),
      strip.text = element_text(size = base_size - 3, colour = "#555555", face = "bold")
    )

  if (SLIDE_MODE) {
    th <- th + theme(plot.title = element_blank(), plot.subtitle = element_blank())
  }
  th
}

bench_x <- -3.7164

save_fig <- function(p, name, w = 8, h = 5) {
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(FIG_DIR, name), p, width = w, height = h, dpi = 300, bg = "white")
}
