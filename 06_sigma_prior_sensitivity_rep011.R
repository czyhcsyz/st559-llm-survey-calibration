set.seed(55912)

target_item <- "Christians"
rep_id <- 11
n_draws <- 10000

input_path <- "hier_item_anchor_n100_all_reps.csv"
if (!file.exists(input_path)) {
  input_path <- file.path("output", input_path)
}
if (!file.exists(input_path)) {
  stop("Missing hier_item_anchor_n100_all_reps.csv. Run 03_multi_item_anchor_prep.R first.")
}

dat <- read.csv(input_path, stringsAsFactors = FALSE)
rep_dat <- dat[dat$rep == rep_id, ]
rep_dat <- rep_dat[order(rep_dat$item), ]
if (nrow(rep_dat) == 0) stop("No rows found for rep = ", rep_id)

y <- rep_dat$delta_hat_j
se <- rep_dat$se_j
V <- se^2
items <- rep_dat$item
target_idx <- match(target_item, items)
if (is.na(target_idx)) stop("Target item not found: ", target_item)

mu_grid <- seq(-15, 15, length.out = 301)
sigma_grid <- seq(0.05, 20, length.out = 400)

log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

log_half_normal <- function(x, scale) {
  ifelse(x > 0, dnorm(x, 0, scale, log = TRUE) + log(2), -Inf)
}

log_half_t <- function(x, df, scale) {
  ifelse(x > 0, dt(x / scale, df = df, log = TRUE) - log(scale) + log(2), -Inf)
}

prior_specs <- list(
  half_normal_5 = function(x) log_half_normal(x, 5),
  half_normal_10 = function(x) log_half_normal(x, 10),
  half_normal_20 = function(x) log_half_normal(x, 20),
  half_t_3_10 = function(x) log_half_t(x, df = 3, scale = 10)
)

prior_labels <- c(
  half_normal_5 = "HalfNormal(0,5)",
  half_normal_10 = "HalfNormal(0,10)",
  half_normal_20 = "HalfNormal(0,20)",
  half_t_3_10 = "Half-t(3,0,10)"
)

grid_posterior <- function(y, se, mu_grid, sigma_grid, sigma_log_prior) {
  log_post <- matrix(NA_real_, nrow = length(mu_grid), ncol = length(sigma_grid))
  for (a in seq_along(mu_grid)) {
    mu <- mu_grid[a]
    for (b in seq_along(sigma_grid)) {
      sig <- sigma_grid[b]
      sd_marg <- sqrt(se^2 + sig^2)
      ll <- sum(dnorm(y, mean = mu, sd = sd_marg, log = TRUE))
      lp_mu <- dnorm(mu, mean = 0, sd = 10, log = TRUE)
      lp_sig <- sigma_log_prior(sig)
      log_post[a, b] <- ll + lp_mu + lp_sig
    }
  }
  log_post <- log_post - log_sum_exp(as.vector(log_post))
  exp(log_post)
}

sample_target_delta <- function(y, se, mu_grid, sigma_grid, weights, target_idx, n_draws) {
  cell <- sample.int(length(weights), size = n_draws, replace = TRUE, prob = as.vector(weights))
  mu_idx <- ((cell - 1) %% length(mu_grid)) + 1
  sigma_idx <- ((cell - 1) %/% length(mu_grid)) + 1
  mu <- mu_grid[mu_idx]
  sig <- sigma_grid[sigma_idx]

  Vj <- se[target_idx]^2
  yj <- y[target_idx]
  post_var <- 1 / (1 / Vj + 1 / sig^2)
  post_mean <- post_var * (yj / Vj + mu / sig^2)
  delta <- rnorm(n_draws, post_mean, sqrt(post_var))
  data.frame(mu = mu, sigma_delta = sig, target_delta = delta)
}

summarise_vector <- function(x) {
  c(
    mean = mean(x),
    sd = sd(x),
    q025 = unname(quantile(x, 0.025)),
    q50 = unname(quantile(x, 0.5)),
    q975 = unname(quantile(x, 0.975))
  )
}

rows <- list()
for (nm in names(prior_specs)) {
  weights <- grid_posterior(y, se, mu_grid, sigma_grid, prior_specs[[nm]])
  draws <- sample_target_delta(y, se, mu_grid, sigma_grid, weights, target_idx, n_draws)

  sigma_s <- summarise_vector(draws$sigma_delta)
  delta_s <- summarise_vector(draws$target_delta)

  rows[[nm]] <- data.frame(
    sigma_prior = prior_labels[[nm]],
    sigma_delta_mean = sigma_s["mean"],
    sigma_delta_low = sigma_s["q025"],
    sigma_delta_high = sigma_s["q975"],
    christians_delta_mean = delta_s["mean"],
    christians_delta_low = delta_s["q025"],
    christians_delta_high = delta_s["q975"],
    stringsAsFactors = FALSE
  )
}

out <- do.call(rbind, rows)
rownames(out) <- NULL
write.csv(out, "sigma_prior_sensitivity_rep011.csv", row.names = FALSE)

cat("Wrote sigma prior sensitivity to sigma_prior_sensitivity_rep011.csv\n")
