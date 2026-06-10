library(readr)
library(dplyr)
library(cmdstanr)
library(posterior)

rep_id <- 11

read_project_csv <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    read_csv(file.path("output", path), show_col_types = FALSE)
  }
}

dat <- read_project_csv("hier_item_anchor_n100_all_reps.csv") |>
  filter(rep == rep_id)

target_idx <- which(dat$item == "Christians")

stan_data <- list(
  J = nrow(dat),
  delta_hat = dat$delta_hat_j,
  se_delta = dat$se_j,
  target_idx = target_idx,
  llm_gap_target = dat$llm_gap_nonanchor_j[target_idx]
)

mod <- cmdstan_model("hier_delta_normal.stan")

fit <- mod$sample(
  data = stan_data,
  seed = 5592031,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95
)

stan_summary <- fit$summary()
write_csv(stan_summary, "hier_stan_summary_rep011.csv")

sampler_diag <- fit$sampler_diagnostics(format = "df")
diagnostics <- sampler_diag |>
  summarise(
    n_divergent = sum(divergent__),
    max_treedepth_observed = max(treedepth__),
    n_max_treedepth = sum(treedepth__ == max(treedepth__)),
    mean_accept_stat = mean(accept_stat__),
    min_accept_stat = min(accept_stat__)
  )
write_csv(diagnostics, "hier_stan_diagnostics_rep011.csv")

selected_draws <- as_draws_df(
  fit$draws(variables = c("mu", "sigma_delta", "psi_target"))
)
write_csv(as.data.frame(selected_draws), "hier_stan_selected_draws_rep011.csv")

message("Wrote hier_stan_summary_rep011.csv")
message("Wrote hier_stan_diagnostics_rep011.csv")
message("Wrote hier_stan_selected_draws_rep011.csv")
