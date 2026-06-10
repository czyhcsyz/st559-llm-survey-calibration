## Data audit for the Bisbee et al. ANES/LLM combined benchmark.

rds_path <- "ANES_LLM_combined.rds"
human_col <- "thermometer_ANES"
llm_col <- "LLM_RICH_full_therm_m"

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

role_for_item <- function(item) {
  if (item == "Christians") return("tentative_main")
  if (item == "Gays and Lesbians") return("contrast")
  if (item %in% c("Conservatives", "Democratic Party", "Republican Party")) {
    return("sensitivity")
  }
  ""
}

df <- readRDS(rds_path)
df$college <- as.integer(df$educ == "bachelor's degree or more")

items <- sort(unique(df$group))
rows <- lapply(items, function(item_name) {
  item_all <- df[df$group == item_name, ]
  item_pair <- item_all[!is.na(item_all[[human_col]]) & !is.na(item_all[[llm_col]]), ]
  item_pair$D <- item_pair[[human_col]] - item_pair[[llm_col]]

  human_gap <- gap(item_pair, human_col)
  llm_gap <- gap(item_pair, llm_col)
  correction <- human_gap - llm_gap

  data.frame(
    item = item_name,
    human_nonmissing = sum(!is.na(item_all[[human_col]])),
    college_paired_n = sum(item_pair$college == 1),
    noncollege_paired_n = sum(item_pair$college == 0),
    human_gap_college_minus_noncollege = human_gap,
    llm_gap_anchor_college_minus_noncollege = llm_gap,
    correction_delta_hat = correction,
    se_delta_hat = sqrt(gap_var(item_pair, "D")),
    abs_correction = abs(correction),
    mean_abs_individual_discrepancy = mean(abs(item_pair$D), na.rm = TRUE),
    candidate_role = role_for_item(item_name)
  )
})

audit <- do.call(rbind, rows)
numeric_cols <- vapply(audit, is.numeric, logical(1))
audit[numeric_cols] <- lapply(audit[numeric_cols], round, 4)

write.csv(audit, "item_audit_table.csv", row.names = FALSE)

cat("Wrote item_audit_table.csv\n")
print(audit)
