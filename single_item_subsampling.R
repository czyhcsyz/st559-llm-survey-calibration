set.seed(5592026)

rds_path <- "ANES_LLM_combined.rds"
item_name <- "Christians"
human_col <- "thermometer_ANES"
llm_col <- "LLM_RICH_full_therm_m"
anchor_sizes <- c(25, 50, 100, 200)
n_reps <- 500
taus <- c(2, 5, 10, 20)

gap <- function(dat, col) {
  mean(dat[dat$college == 1, col], na.rm = TRUE) -
    mean(dat[dat$college == 0, col], na.rm = TRUE)
}

gap_var <- function(dat, col) {
  x1 <- dat[dat$college == 1, col]
  x0 <- dat[dat$college == 0, col]
  var(x1, na.rm = TRUE) / length(na.omit(x1)) +
    var(x0, na.rm = TRUE) / length(na.omit(x0))
}

gap_cov <- function(dat, col_a, col_b) {
  out <- 0
  for (g in 0:1) {
    a <- dat[dat$college == g, col_a]
    b <- dat[dat$college == g, col_b]
    ok <- !is.na(a) & !is.na(b)
    n <- sum(ok)
    if (n > 1) {
      out <- out + cov(a[ok], b[ok]) / n
    }
  }
  out
}

add_record <- function(records, rep, n, estimator, tau, lambda, est, variance, benchmark) {
  se <- sqrt(max(variance, 0))
  lo <- est - 1.96 * se
  hi <- est + 1.96 * se
  records[[length(records) + 1]] <- data.frame(
    item = item_name,
    rep = rep,
    anchor_n_per_group = n,
    estimator = estimator,
    tau = tau,
    lambda = lambda,
    estimate = est,
    variance = variance,
    ci_low = lo,
    ci_high = hi,
    interval_width = hi - lo,
    benchmark = benchmark,
    error = est - benchmark,
    squared_error = (est - benchmark)^2,
    covered = as.integer(lo <= benchmark && benchmark <= hi),
    wrong_sign = as.integer(sign(est) != 0 && sign(est) != sign(benchmark))
  )
  records
}

df <- readRDS(rds_path)
df$college <- as.integer(df$educ == "bachelor's degree or more")

item <- df[
  df$group == item_name &
    !is.na(df[[human_col]]) &
    !is.na(df[[llm_col]]),
]
item$D <- item[[human_col]] - item[[llm_col]]

college <- item[item$college == 1, ]
noncollege <- item[item$college == 0, ]

benchmark <- gap(item, human_col)
llm_gap_complete <- gap(item, llm_col)
v_l_complete <- gap_var(item, llm_col)

records <- list()

for (n in anchor_sizes) {
  for (rep in seq_len(n_reps)) {
    anchor <- rbind(
      college[sample(seq_len(nrow(college)), n, replace = FALSE), ],
      noncollege[sample(seq_len(nrow(noncollege)), n, replace = FALSE), ]
    )

    h_est <- gap(anchor, human_col)
    h_var <- gap_var(anchor, human_col)
    records <- add_record(records, rep, n, "human_only", NA, NA, h_est, h_var, benchmark)

    records <- add_record(records, rep, n, "llm_only", NA, NA, llm_gap_complete, v_l_complete, benchmark)

    pooled_estimates <- lapply(0:1, function(g) {
      y <- c(item[item$college == g, llm_col], anchor[anchor$college == g, human_col])
      c(mean = mean(y, na.rm = TRUE), variance = var(y, na.rm = TRUE), n = length(na.omit(y)))
    })
    pool_est <- pooled_estimates[[2]]["mean"] - pooled_estimates[[1]]["mean"]
    pool_var <- pooled_estimates[[2]]["variance"] / pooled_estimates[[2]]["n"] +
      pooled_estimates[[1]]["variance"] / pooled_estimates[[1]]["n"]
    records <- add_record(records, rep, n, "naive_pooling", NA, NA, pool_est, pool_var, benchmark)

    delta_hat <- gap(anchor, "D")
    v_delta <- gap_var(anchor, "D")
    non_anchor <- item[!(item$respID %in% anchor$respID), ]
    llm_gap_nonanchor <- gap(non_anchor, llm_col)
    v_l_nonanchor <- gap_var(non_anchor, llm_col)
    ppi_est <- llm_gap_nonanchor + delta_hat
    ppi_var <- v_l_nonanchor + v_delta
    records <- add_record(records, rep, n, "ppi_disjoint", NA, NA, ppi_est, ppi_var, benchmark)

    v_l_anchor <- gap_var(anchor, llm_col)
    cov_hl_anchor <- gap_cov(anchor, human_col, llm_col)
    denom <- v_l_nonanchor + v_l_anchor
    lambda_pp <- ifelse(denom > 0, cov_hl_anchor / denom, 0)
    lambda_pp <- min(1, max(0, lambda_pp))

    h_gap_anchor <- gap(anchor, human_col)
    l_gap_anchor <- gap(anchor, llm_col)
    ppi_plus_est <- lambda_pp * llm_gap_nonanchor + (h_gap_anchor - lambda_pp * l_gap_anchor)
    v_h_anchor <- gap_var(anchor, human_col)
    ppi_plus_var <- lambda_pp^2 * denom - 2 * lambda_pp * cov_hl_anchor + v_h_anchor
    records <- add_record(
      records, rep, n, "ppi_plus", NA, lambda_pp,
      ppi_plus_est, max(ppi_plus_var, 0), benchmark
    )

    for (tau in taus) {
      lambda <- tau^2 / (tau^2 + v_delta)
      b_est <- llm_gap_nonanchor + lambda * delta_hat
      b_var <- v_l_nonanchor + (tau^2 * v_delta) / (tau^2 + v_delta)
      records <- add_record(records, rep, n, "bayes_shrinkage_disjoint", tau, lambda, b_est, b_var, benchmark)
    }
  }
}

draws <- do.call(rbind, records)
write.csv(draws, "single_item_christians_subsampling_draws_disjoint.csv", row.names = FALSE)

draws$tau_label <- ifelse(is.na(draws$tau), "NA", as.character(draws$tau))
summary <- aggregate(
  cbind(estimate, error, squared_error, interval_width, covered, wrong_sign, lambda) ~
    anchor_n_per_group + estimator + tau_label,
  data = draws,
  FUN = mean,
  na.action = na.pass
)
summary$tau <- suppressWarnings(as.numeric(summary$tau_label))
summary$tau_label <- NULL
names(summary)[names(summary) == "estimate"] <- "mean_estimate"
names(summary)[names(summary) == "error"] <- "bias"
names(summary)[names(summary) == "squared_error"] <- "mse"
summary$rmse <- sqrt(summary$mse)
summary$benchmark <- benchmark
summary$n_reps <- n_reps
write.csv(summary, "single_item_christians_subsampling_summary_disjoint.csv", row.names = FALSE)

ppi_plus_comparison <- summary[
  summary$estimator %in% c("ppi_disjoint", "ppi_plus") |
    (summary$estimator == "bayes_shrinkage_disjoint" & summary$tau == 5),
]
write.csv(ppi_plus_comparison, "ppi_plus_comparison_summary.csv", row.names = FALSE)

print(sprintf("Full human benchmark gap: %.3f", benchmark))
print(sprintf("Complete-case LLM proxy gap: %.3f", llm_gap_complete))
print(sprintf("Complete-case correction: %.3f", benchmark - llm_gap_complete))
print(summary)
