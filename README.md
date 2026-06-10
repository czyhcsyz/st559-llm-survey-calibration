# Bayesian Shrinkage for Calibrating an LLM Survey Proxy

This repository contains the replication code for an ST 559 Bayesian Statistics project on calibrating LLM-generated survey responses with limited human anchors. The analysis uses the Bisbee et al. replication data linking ANES feeling-thermometer responses to matched ChatGPT responses.

## Data

The raw data file is not included in this repository. Download `ANES_LLM_combined.rds` from the Bisbee et al. Harvard Dataverse replication archive:

https://doi.org/10.7910/DVN/VPN481

Place `ANES_LLM_combined.rds` in the repository root before running the analysis.

## Reproduction

Run the full R pipeline from the repository root:

```r
source("00_run_all_R.R")
```

The default run performs the data audit, repeated single-item subsampling, repeated hierarchical grid evaluation, posterior predictive checks, and prior sensitivity analysis.

To run the CmdStanR fit for the representative hierarchical draw:

```r
Sys.setenv(RUN_STAN = "1")
source("00_run_all_R.R")
```

To rebuild the final ggplot figures:

```r
Sys.setenv(RUN_FIGURES = "1")
source("00_run_all_R.R")
```

The figure script first looks for newly generated CSV files in the repository root. If they are not present, it reads the archived summaries in `output/`.

## Repository structure

```text
.
|-- 00_run_all_R.R
|-- 01_data_audit.R
|-- 02_complete_all_sensitivity.R
|-- single_item_subsampling.R
|-- 03_multi_item_anchor_prep.R
|-- 04_hier_repeated_grid.R
|-- 05_hier_rep011_grid_ppc.R
|-- 06_sigma_prior_sensitivity_rep011.R
|-- hier_delta_normal.stan
|-- output/              # generated CSV summaries used in the report
`-- figures/figs_ggplot/ # final PNG figures used in the presentation/report
```

## Main files

- `00_run_all_R.R`: pipeline runner.
- `01_data_audit.R`: item-level data audit.
- `02_complete_all_sensitivity.R`: complete-all-item sampling frame check.
- `single_item_subsampling.R`: repeated subsampling for the Christians thermometer item.
- `03_multi_item_anchor_prep.R`: multi-item anchor construction.
- `04_hier_repeated_grid.R`: repeated hierarchical grid evaluation.
- `hier_delta_normal.stan`: hierarchical normal model.
- `fit_hier_delta_stan_rep011.R`: CmdStanR fit for the representative draw.
- `05_hier_rep011_grid_ppc.R`: posterior predictive checks.
- `06_sigma_prior_sensitivity_rep011.R`: prior sensitivity analysis.
- `make_figures_ggplot.R`: final figure generation.

## Output

The repository includes generated CSV summaries and figures used in the report. Results may differ slightly across platforms if the R version or package versions differ, but the scripts set seeds for the main repeated-sampling experiments.
