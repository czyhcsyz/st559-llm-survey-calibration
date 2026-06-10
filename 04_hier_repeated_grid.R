## Repeated grid evaluation for no pooling, complete pooling, and partial
## pooling in the hierarchical discrepancy model.

set.seed(5592029)

input_csv <- "hier_item_anchor_n100_all_reps.csv"
if (!file.exists(input_csv)) {
  input_csv <- file.path("output", input_csv)
}
target_item <- "Christians"
mu_grid <- seq(-15, 15, length.out = 301)
sigma_grid <- seq(0.05, 15, length.out = 300)
n_draws <- 5000

qfun <- function(x, p) as.numeric(quantile(x, p, names = FALSE, type = 7))

draw_grid_posterior <- function(rep_dat) {
  grid <- expand.grid(mu = mu_grid, sigma_delta = sigma_grid)
  logp <- dnorm(grid$mu, 0, 10, log = TRUE) +
    log(2) + dnorm(grid$sigma_delta, 0, 10, log = TRUE)

  for (j in seq_len(nrow(rep_dat))) {
    var_j <- grid$sigma_delta^2 + rep_dat$se_j[j]^2
    logp <- logp + dnorm(rep_dat$delta_hat_j[j], grid$mu, sqrt(var_j), log = TRUE)
  }

  w <- exp(logp - max(logp))
  w <- w / sum(w)
  idx <- sample(seq_len(nrow(grid)), n_draws, replace = TRUE, prob = w)

  boundary <- data.frame(
    mass_mu_near_lower = sum(w[grid$mu <= min(mu_grid) + 0.5]),
    mass_mu_near_upper = sum(w[grid$mu >= max(mu_grid) - 0.5]),
    mass_sigma_near_lower = sum(w[grid$sigma_delta <= min(sigma_grid) + 0.05]),
    mass_sigma_near_upper = sum(w[grid$sigma_delta >= max(sigma_grid) - 0.5])
  )

  list(
    mu = grid$mu[idx],
    sigma_delta = grid$sigma_delta[idx],
    boundary = boundary
  )
}

target_draws_no_pool <- function(target) {
  delta <- rnorm(n_draws, target$delta_hat_j, target$se_j)
  l_noise <- rnorm(n_draws, 0, sqrt(target$V_l_nonanchor_j))
  target$llm_gap_nonanchor_j + delta + l_noise
}

target_draws_complete_pool <- function(target, rep_dat) {
  prior_var <- 100
  post_var <- 1 / (1 / prior_var + sum(1 / rep_dat$V_j))
  post_mean <- post_var * sum(rep_dat$delta_hat_j / rep_dat$V_j)
  mu_draw <- rnorm(n_draws, post_mean, sqrt(post_var))
  l_noise <- rnorm(n_draws, 0, sqrt(target$V_l_nonanchor_j))
  target$llm_gap_nonanchor_j + mu_draw + l_noise
}

target_draws_partial_pool <- function(target, mu_draw, sigma_draw) {
  obs_var <- target$V_j
  prior_var <- sigma_draw^2
  post_var <- 1 / (1 / obs_var + 1 / prior_var)
  post_mean <- post_var * (target$delta_hat_j / obs_var + mu_draw / prior_var)
  delta <- rnorm(n_draws, post_mean, sqrt(post_var))
  l_noise <- rnorm(n_draws, 0, sqrt(target$V_l_nonanchor_j))
  target$llm_gap_nonanchor_j + delta + l_noise
}

method_record <- function(rep_id, method, target, draws) {
  lo <- qfun(draws, 0.025)
  hi <- qfun(draws, 0.975)
  est <- mean(draws)
  benchmark <- target$human_gap_full_j
  data.frame(
    rep = rep_id,
    method = method,
    estimate = est,
    sd = sd(draws),
    low = lo,
    high = hi,
    interval_width = hi - lo,
    benchmark = benchmark,
    error = est - benchmark,
    squared_error = (est - benchmark)^2,
    covered = as.integer(lo <= benchmark && benchmark <= hi),
    wrong_sign = as.integer(sign(est) != 0 && sign(est) != sign(benchmark)),
    target_delta_hat = target$delta_hat_j,
    target_llm_gap_nonanchor = target$llm_gap_nonanchor_j
  )
}

summarise_performance <- function(target_rows) {
  methods <- sort(unique(target_rows$method))
  do.call(rbind, lapply(methods, function(m) {
    sub <- target_rows[target_rows$method == m, ]
    data.frame(
      method = m,
      n_reps = nrow(sub),
      mean_estimate = mean(sub$estimate),
      bias = mean(sub$error),
      rmse = sqrt(mean(sub$squared_error)),
      mean_interval_width = mean(sub$interval_width),
      empirical_coverage = mean(sub$covered),
      wrong_sign_probability = mean(sub$wrong_sign),
      benchmark = sub$benchmark[1]
    )
  }))
}

dat <- read.csv(input_csv)
target_rows <- list()
hyper_rows <- list()
boundary_rows <- list()
target_delta_rows <- list()

for (rep_id in sort(unique(dat$rep))) {
  rep_dat <- dat[dat$rep == rep_id, ]
  target <- rep_dat[rep_dat$item == target_item, ][1, ]

  grid_draws <- draw_grid_posterior(rep_dat)
  mu_draw <- grid_draws$mu
  sigma_draw <- grid_draws$sigma_delta

  target_rows[[length(target_rows) + 1]] <-
    method_record(rep_id, "no_pooling", target, target_draws_no_pool(target))
  target_rows[[length(target_rows) + 1]] <-
    method_record(rep_id, "complete_pooling", target, target_draws_complete_pool(target, rep_dat))
  target_rows[[length(target_rows) + 1]] <-
    method_record(rep_id, "partial_pooling", target, target_draws_partial_pool(target, mu_draw, sigma_draw))

  hyper_rows[[length(hyper_rows) + 1]] <- data.frame(
    rep = rep_id,
    mu_mean = mean(mu_draw),
    mu_sd = sd(mu_draw),
    mu_low = qfun(mu_draw, 0.025),
    mu_high = qfun(mu_draw, 0.975),
    sigma_delta_mean = mean(sigma_draw),
    sigma_delta_sd = sd(sigma_draw),
    sigma_delta_low = qfun(sigma_draw, 0.025),
    sigma_delta_high = qfun(sigma_draw, 0.975)
  )

  boundary <- grid_draws$boundary
  boundary$rep <- rep_id
  boundary_rows[[length(boundary_rows) + 1]] <- boundary
  target_delta_rows[[length(target_delta_rows) + 1]] <- data.frame(
    rep = rep_id,
    target_delta_hat = target$delta_hat_j
  )
}

target_out <- do.call(rbind, target_rows)
hyper_out <- do.call(rbind, hyper_rows)
boundary_out <- do.call(rbind, boundary_rows)
perf_out <- summarise_performance(target_out)
target_delta_out <- do.call(rbind, target_delta_rows)

median_delta <- median(target_delta_out$target_delta_hat)
full_correction <- -3.5611
rep001 <- target_delta_out[target_delta_out$rep == 1, ]
nearest_median <- target_delta_out[
  which.min(abs(target_delta_out$target_delta_hat - median_delta)),
]
nearest_full <- target_delta_out[
  which.min(abs(target_delta_out$target_delta_hat - full_correction)),
]

representative <- rbind(
  data.frame(role = "demo_cautionary", rep001, reference_value = NA, distance = NA),
  data.frame(
    role = "nearest_median_delta",
    nearest_median,
    reference_value = median_delta,
    distance = abs(nearest_median$target_delta_hat - median_delta)
  ),
  data.frame(
    role = "nearest_full_correction",
    nearest_full,
    reference_value = full_correction,
    distance = abs(nearest_full$target_delta_hat - full_correction)
  )
)

write.csv(target_out, "hier_repeated_target_summary_n100.csv", row.names = FALSE)
write.csv(hyper_out, "hier_repeated_hyper_summary_n100.csv", row.names = FALSE)
write.csv(boundary_out, "grid_boundary_check_n100.csv", row.names = FALSE)
write.csv(perf_out, "hier_repeated_pooling_performance_n100.csv", row.names = FALSE)
write.csv(representative, "hier_representative_reps_n100.csv", row.names = FALSE)

cat("Wrote hierarchical repeated grid outputs\n")
print(perf_out)
