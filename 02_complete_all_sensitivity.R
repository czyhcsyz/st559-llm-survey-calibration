## Check whether restricting the hierarchical anchor pool to respondents
## complete on all 11 thermometer items changes the Christians benchmark.

rds_path <- "ANES_LLM_combined.rds"
item_name <- "Christians"
human_col <- "thermometer_ANES"
llm_col <- "LLM_RICH_full_therm_m"

gap <- function(dat, col) {
  mean(dat[dat$college == 1, col], na.rm = TRUE) -
    mean(dat[dat$college == 0, col], na.rm = TRUE)
}

df <- readRDS(rds_path)
df$college <- as.integer(df$educ == "bachelor's degree or more")
paired <- df[!is.na(df[[human_col]]) & !is.na(df[[llm_col]]), ]

items <- sort(unique(paired$group))
item_count <- tapply(paired$group, paired$respID, function(x) length(unique(x)))
complete_ids <- names(item_count)[item_count == length(items)]

christians_complete_case <- paired[paired$group == item_name, ]
christians_complete_all <- christians_complete_case[
  christians_complete_case$respID %in% complete_ids,
]

make_row <- function(label, dat) {
  human_gap <- gap(dat, human_col)
  llm_gap <- gap(dat, llm_col)
  data.frame(
    sample = label,
    n_total = nrow(dat),
    n_college = sum(dat$college == 1),
    n_noncollege = sum(dat$college == 0),
    human_gap = human_gap,
    llm_gap = llm_gap,
    correction_delta = human_gap - llm_gap
  )
}

out <- rbind(
  make_row("christians_complete_case", christians_complete_case),
  make_row("complete_all_11_items", christians_complete_all)
)

base <- out[1, ]
out$difference_from_christians_complete_case_human_gap <-
  out$human_gap - base$human_gap
out$difference_from_christians_complete_case_llm_gap <-
  out$llm_gap - base$llm_gap
out$difference_from_christians_complete_case_correction <-
  out$correction_delta - base$correction_delta

numeric_cols <- vapply(out, is.numeric, logical(1))
out[numeric_cols] <- lapply(out[numeric_cols], round, 4)

write.csv(out, "complete_all_pool_sensitivity.csv", row.names = FALSE)
cat("Wrote complete_all_pool_sensitivity.csv\n")
print(out)
