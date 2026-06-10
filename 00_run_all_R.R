## R-only reproduction pipeline for the ST 559 project.
##
## The default run reproduces the main data audit, single-item subsampling,
## hierarchical grid evaluation, representative-draw PPC, and prior sensitivity.
## Set RUN_STAN=1 before running this script to also run the CmdStanR fit.
## Set RUN_FIGURES=1 to rebuild the final ggplot figures.

source("01_data_audit.R")
source("02_complete_all_sensitivity.R")
source("single_item_subsampling.R")
source("03_multi_item_anchor_prep.R")
source("04_hier_repeated_grid.R")
source("05_hier_rep011_grid_ppc.R")
source("06_sigma_prior_sensitivity_rep011.R")

if (identical(Sys.getenv("RUN_STAN"), "1")) {
  source("fit_hier_delta_stan_rep011.R")
} else {
  message("Skipping CmdStanR fit. Set RUN_STAN=1 to run fit_hier_delta_stan_rep011.R.")
}

if (identical(Sys.getenv("RUN_FIGURES"), "1")) {
  source("make_figures_ggplot.R")
} else {
  message("Skipping final ggplot figures. Set RUN_FIGURES=1 to run make_figures_ggplot.R.")
}

message("R-only pipeline complete.")
