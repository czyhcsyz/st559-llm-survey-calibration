set.seed(55911)

target_item <- "Christians"
rep_id <- 11
n_draws <- 8000

input_path <- "hier_item_anchor_n100_all_reps.csv"
if (!file.exists(input_path)) {
  input_path <- file.path("output", input_path)
}
if (!file.exists(input_path)) {
  stop("Missing hier_item_anchor_n100_all_reps.csv. Run 03_multi_item_anchor_prep.R first.")
}

dat <- read.csv(input_path, stringsAsFactors = FALSE)
rep_dat <- dat[dat$rep == rep_id, ]
if (nrow(rep_dat) == 0) {
  stop("No rows found for rep = ", rep_id)
}

rep_dat <- rep_dat[order(rep_dat$item), ]
y <- rep_dat$delta_hat_j
se <- rep_dat$se_j
V <- se^2
items <- rep_dat$item
J <- length(items)
target_idx <- match(target_item, items)
if (is.na(target_idx)) stop("Target item not found: ", target_item)

mu_grid <- seq(-15, 15, length.out = 301)
sigma_grid <- seq(0.05, 15, length.out = 300)

log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

weighted_quantile <- function(x, w, probs) {
  ord <- order(x)
  x <- x[ord]
  w <- w[ord] / sum(w)
  cw <- cumsum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

grid_posterior <- function(y, se, mu_grid, sigma_grid) {
  log_post <- matrix(NA_real_, nrow = length(mu_grid), ncol = length(sigma_grid))
  for (a in seq_along(mu_grid)) {
    mu <- mu_grid[a]
    for (b in seq_along(sigma_grid)) {
      sig <- sigma_grid[b]
      sd_marg <- sqrt(se^2 + sig^2)
      ll <- sum(dnorm(y, mean = mu, sd = sd_marg, log = TRUE))
      lp_mu <- dnorm(mu, mean = 0, sd = 10, log = TRUE)
      lp_sig <- dnorm(sig, mean = 0, sd = 10, log = TRUE) + log(2)
      log_post[a, b] <- ll + lp_mu + lp_sig
    }
  }
  log_post <- log_post - log_sum_exp(as.vector(log_post))
  weights <- exp(log_post)
  list(weights = weights, log_post = log_post)
}

sample_partial_pooling <- function(y, se, mu_grid, sigma_grid, weights, n_draws) {
  cell <- sample.int(length(weights), size = n_draws, replace = TRUE, prob = as.vector(weights))
  mu_idx <- ((cell - 1) %% length(mu_grid)) + 1
  sigma_idx <- ((cell - 1) %/% length(mu_grid)) + 1
  mu_draw <- mu_grid[mu_idx]
  sigma_draw <- sigma_grid[sigma_idx]

  delta_draws <- matrix(NA_real_, nrow = n_draws, ncol = length(y))
  for (d in seq_len(n_draws)) {
    post_var <- 1 / (1 / se^2 + 1 / sigma_draw[d]^2)
    post_mean <- post_var * (y / se^2 + mu_draw[d] / sigma_draw[d]^2)
    delta_draws[d, ] <- rnorm(length(y), mean = post_mean, sd = sqrt(post_var))
  }

  list(mu = mu_draw, sigma = sigma_draw, delta = delta_draws)
}

summarise_draws <- function(x) {
  c(
    mean = mean(x),
    sd = sd(x),
    q025 = unname(quantile(x, 0.025)),
    q50 = unname(quantile(x, 0.5)),
    q975 = unname(quantile(x, 0.975))
  )
}

target_truth <- unique(rep_dat$human_gap_full_college_minus_noncollege)
if (length(target_truth) != 1) {
  target_truth <- target_truth[1]
}

posterior <- grid_posterior(y, se, mu_grid, sigma_grid)
draws <- sample_partial_pooling(y, se, mu_grid, sigma_grid, posterior$weights, n_draws)

delta_summary <- t(apply(draws$delta, 2, summarise_draws))
hier_summary <- data.frame(
  rep = rep_id,
  item = items,
  role = ifelse(items == target_item, "target", "pooling_item"),
  delta_hat_j = y,
  se_j = se,
  delta_post_mean = delta_summary[, "mean"],
  delta_post_sd = delta_summary[, "sd"],
  delta_post_q025 = delta_summary[, "q025"],
  delta_post_q50 = delta_summary[, "q50"],
  delta_post_q975 = delta_summary[, "q975"],
  stringsAsFactors = FALSE
)
write.csv(hier_summary, "hier_summary_grid_rep011.csv", row.names = FALSE)

hyper_summary <- data.frame(
  rep = rep_id,
  parameter = c("mu", "sigma_delta"),
  mean = c(mean(draws$mu), mean(draws$sigma)),
  sd = c(sd(draws$mu), sd(draws$sigma)),
  q025 = c(unname(quantile(draws$mu, 0.025)), unname(quantile(draws$sigma, 0.025))),
  q50 = c(unname(quantile(draws$mu, 0.5)), unname(quantile(draws$sigma, 0.5))),
  q975 = c(unname(quantile(draws$mu, 0.975)), unname(quantile(draws$sigma, 0.975)))
)
write.csv(hyper_summary, "hier_hyper_summary_grid_rep011.csv", row.names = FALSE)

near_mu_lower <- sum(posterior$weights[seq_len(5), ])
near_mu_upper <- sum(posterior$weights[(length(mu_grid) - 4):length(mu_grid), ])
near_sigma_upper <- sum(posterior$weights[, (length(sigma_grid) - 4):length(sigma_grid)])
boundary <- data.frame(
  rep = rep_id,
  mass_mu_near_lower = near_mu_lower,
  mass_mu_near_upper = near_mu_upper,
  mass_sigma_near_upper = near_sigma_upper
)
write.csv(boundary, "grid_boundary_check_rep011.csv", row.names = FALSE)

target_delta <- draws$delta[, target_idx]
target_llm_nonanchor <- unique(rep_dat$llm_gap_nonanchor_j[rep_dat$item == target_item])
target_v_llm <- unique(rep_dat$v_l_nonanchor_j[rep_dat$item == target_item])
if (length(target_llm_nonanchor) != 1) target_llm_nonanchor <- target_llm_nonanchor[1]
if (length(target_v_llm) != 1 || is.na(target_v_llm)) target_v_llm <- 0
target_psi_partial <- rnorm(
  n_draws,
  mean = target_llm_nonanchor + target_delta,
  sd = sqrt(max(target_v_llm, 0))
)

no_pool_delta <- rnorm(n_draws, mean = y[target_idx], sd = se[target_idx])
no_pool_psi <- rnorm(
  n_draws,
  mean = target_llm_nonanchor + no_pool_delta,
  sd = sqrt(max(target_v_llm, 0))
)

prior_var_mu <- 100
complete_var <- 1 / (sum(1 / V) + 1 / prior_var_mu)
complete_mean <- complete_var * sum(y / V)
complete_delta <- rnorm(n_draws, complete_mean, sqrt(complete_var))
complete_psi <- rnorm(
  n_draws,
  mean = target_llm_nonanchor + complete_delta,
  sd = sqrt(max(target_v_llm, 0))
)

pooling_summary <- rbind(
  data.frame(method = "no_pooling", t(summarise_draws(no_pool_psi))),
  data.frame(method = "partial_pooling", t(summarise_draws(target_psi_partial))),
  data.frame(method = "complete_pooling", t(summarise_draws(complete_psi)))
)
names(pooling_summary)[names(pooling_summary) == "q025"] <- "ci_low"
names(pooling_summary)[names(pooling_summary) == "q975"] <- "ci_high"
pooling_summary$truth <- target_truth
pooling_summary$covered <- pooling_summary$ci_low <= target_truth & pooling_summary$ci_high >= target_truth
write.csv(pooling_summary, "pooling_sensitivity_christians_rep011.csv", row.names = FALSE)

yrep <- matrix(NA_real_, nrow = n_draws, ncol = J)
for (j in seq_len(J)) {
  yrep[, j] <- rnorm(n_draws, mean = draws$delta[, j], sd = se[j])
}
ppc_summary <- data.frame(
  rep = rep_id,
  item = items,
  observed = y,
  yrep_mean = colMeans(yrep),
  yrep_q025 = apply(yrep, 2, quantile, 0.025),
  yrep_q50 = apply(yrep, 2, quantile, 0.5),
  yrep_q975 = apply(yrep, 2, quantile, 0.975),
  observed_in_95 = y >= apply(yrep, 2, quantile, 0.025) &
    y <= apply(yrep, 2, quantile, 0.975),
  stringsAsFactors = FALSE
)
write.csv(ppc_summary, "hier_ppc_summary_rep011.csv", row.names = FALSE)

rank_of_target <- function(x, idx) {
  rank(x, ties.method = "average")[idx]
}

obs_stats <- c(
  range = diff(range(y)),
  number_positive = sum(y > 0),
  target_rank = rank_of_target(y, target_idx)
)
rep_stats <- data.frame(
  range = apply(yrep, 1, function(x) diff(range(x))),
  number_positive = rowSums(yrep > 0),
  target_rank = apply(yrep, 1, rank_of_target, idx = target_idx)
)

ppc_stats <- data.frame(
  statistic = names(obs_stats),
  observed = as.numeric(obs_stats),
  yrep_mean = c(mean(rep_stats$range), mean(rep_stats$number_positive), mean(rep_stats$target_rank)),
  yrep_q025 = c(
    unname(quantile(rep_stats$range, 0.025)),
    unname(quantile(rep_stats$number_positive, 0.025)),
    unname(quantile(rep_stats$target_rank, 0.025))
  ),
  yrep_q50 = c(
    unname(quantile(rep_stats$range, 0.5)),
    unname(quantile(rep_stats$number_positive, 0.5)),
    unname(quantile(rep_stats$target_rank, 0.5))
  ),
  yrep_q975 = c(
    unname(quantile(rep_stats$range, 0.975)),
    unname(quantile(rep_stats$number_positive, 0.975)),
    unname(quantile(rep_stats$target_rank, 0.975))
  )
)
write.csv(ppc_stats, "hier_ppc_test_statistics_rep011.csv", row.names = FALSE)

cat("Wrote representative hierarchical grid summaries and PPC diagnostics for rep", rep_id, "\n")
