## Construct repeated multi-item anchor correction tables for the hierarchical
## discrepancy model. The same anchor respondents are used across all 11 items.

set.seed(5592027)

rds_path <- "ANES_LLM_combined.rds"
human_col <- "thermometer_ANES"
llm_col <- "LLM_RICH_full_therm_m"
target_item <- "Christians"
contrast_item <- "Gays and Lesbians"
party_items <- c("Democratic Party", "Republican Party")
anchor_n_per_group <- 100
n_reps <- 50

gap <- function(dat, col) {
  mean(dat[dat$college == 1, col], na.rm = TRUE) -
    mean(dat[dat$college == 0, col], na.rm = TRUE)
}

gap_var <- function(dat, col) {
  x1 <- dat[dat$college == 1, col]
  x0 <- dat[dat$college == 0, col]
  var(x1, na.rm = TRUE) / sum(!is.na(x1)) +
    var(x0, na.rm = TRUE) / sum(!is.na(x0))
}

item_role <- function(item) {
  if (item == target_item) return("target")
  if (item == contrast_item) return("contrast")
  if (item %in% party_items) return("party_sensitivity")
  "pool"
}

df <- readRDS(rds_path)
df$college <- as.integer(df$educ == "bachelor's degree or more")
df <- df[!is.na(df[[human_col]]) & !is.na(df[[llm_col]]), ]
df$D <- df[[human_col]] - df[[llm_col]]

items <- sort(unique(df$group))
item_count <- tapply(df$group, df$respID, function(x) length(unique(x)))
complete_ids <- names(item_count)[item_count == length(items)]

respondents <- df[df$respID %in% complete_ids, c("respID", "college")]
respondents <- respondents[!duplicated(respondents$respID), ]
college_ids <- respondents$respID[respondents$college == 1]
noncollege_ids <- respondents$respID[respondents$college == 0]

if (anchor_n_per_group > length(college_ids) ||
    anchor_n_per_group > length(noncollege_ids)) {
  stop("Anchor size exceeds complete-respondent subgroup size.")
}

records <- list()

for (rep_id in seq_len(n_reps)) {
  anchor_ids <- c(
    sample(college_ids, anchor_n_per_group, replace = FALSE),
    sample(noncollege_ids, anchor_n_per_group, replace = FALSE)
  )

  for (item in items) {
    item_df <- df[df$group == item, ]
    anchor <- item_df[item_df$respID %in% anchor_ids, ]
    non_anchor <- item_df[!(item_df$respID %in% anchor_ids), ]

    delta_hat <- gap(anchor, "D")
    v_delta <- gap_var(anchor, "D")
    llm_gap_nonanchor <- gap(non_anchor, llm_col)
    v_l_nonanchor <- gap_var(non_anchor, llm_col)

    records[[length(records) + 1]] <- data.frame(
      rep = rep_id,
      anchor_n_per_group = anchor_n_per_group,
      item = item,
      role = item_role(item),
      delta_hat_j = delta_hat,
      V_j = v_delta,
      se_j = sqrt(v_delta),
      n_college_j = sum(anchor$college == 1),
      n_noncollege_j = sum(anchor$college == 0),
      human_gap_anchor_j = gap(anchor, human_col),
      llm_gap_anchor_j = gap(anchor, llm_col),
      llm_gap_nonanchor_j = llm_gap_nonanchor,
      V_l_nonanchor_j = v_l_nonanchor,
      se_l_nonanchor_j = sqrt(v_l_nonanchor),
      human_gap_full_j = gap(item_df, human_col),
      llm_gap_complete_j = gap(item_df, llm_col),
      target_item = item == target_item
    )
  }
}

all_reps <- do.call(rbind, records)
write.csv(all_reps, "hier_item_anchor_n100_all_reps.csv", row.names = FALSE)

rep001 <- all_reps[all_reps$rep == 1, ]
write.csv(rep001, "hier_item_anchor_n100_rep001.csv", row.names = FALSE)

audit <- rep001
audit$no_pool_low <- audit$delta_hat_j - 1.96 * audit$se_j
audit$no_pool_high <- audit$delta_hat_j + 1.96 * audit$se_j
audit$abs_delta <- abs(audit$delta_hat_j)
audit <- audit[order(audit$delta_hat_j), ]
write.csv(audit, "hier_item_anchor_n100_audit.csv", row.names = FALSE)

cat("Wrote hier_item_anchor_n100_all_reps.csv\n")
cat("Wrote hier_item_anchor_n100_rep001.csv\n")
cat("Wrote hier_item_anchor_n100_audit.csv\n")
print(audit[, c("item", "role", "delta_hat_j", "se_j", "n_college_j", "n_noncollege_j")])
