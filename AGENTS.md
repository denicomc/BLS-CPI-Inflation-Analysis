# Repository Guidelines

## Project Structure & Module Organization
This repository is an R analysis workflow focused on CPI data from BLS.

- Top-level scripts (`0_run_analysis.R`, `1_shiny_data_update.R`, `2_january_unadjusted.R`, etc.) are entry points for monthly or topical analyses.
- `scripts/` contains reusable pipeline pieces.
- `scripts/01_download_cpi_data.R` handles BLS pull and weights merge.
- `scripts/02_general_graphic_scripts.R` defines shared transforms and plotting helpers.
- `scripts/03_specific_graphic_scripts.R` contains specialized chart functions.
- `data/` stores intermediate and source CSV/RData inputs.
- `weights/` stores CPI/PCE weight tables used in calculations.
- `graphics/` contains rendered output charts.
- `legacy_code/` and `refacturing code/` hold older or in-progress refactors.

## Build, Test, and Development Commands
Run from repo root.

- `Rscript 0_run_analysis.R`: main monthly analysis run; regenerates core charts in `graphics/`.
- `Rscript 1_shiny_data_update.R`: refreshes data artifacts for Shiny-related outputs.
- `Rscript 2_january_unadjusted.R`: targeted unadjusted January analysis.
- `Rscript 3_trump_tariffs.R`: tariff-specific analysis/graphics.

Install dependencies before first run (example):
`R -e "install.packages(c('tidyverse','lubridate','janitor','ggrepel','ggridges','viridis','gt','bea.R','quantmod','hrbrthemes'))"`
`R -e "install.packages('remotes'); remotes::install_github('mtkonczal/govMacroTools')"`

## Coding Style & Naming Conventions
- Use tidyverse-first R style with pipes and `snake_case` names.
- Indentation is 2 spaces; keep long calls vertically aligned.
- Name new scripts with numeric prefixes when they are run-order dependent (for example, `8_new_topic.R`).
- Keep reusable logic in `scripts/` and keep top-level files as orchestration scripts.

## Testing Guidelines
There is no formal automated test suite yet (`tests/` is absent).

- Treat successful script execution as the baseline check: `Rscript 0_run_analysis.R` should complete without errors.
- Validate outputs by confirming expected files are updated in `graphics/` and key data files in `data/`.
- For function changes, run a focused script that exercises the modified path and sanity-check latest dates/series values.

## Commit & Pull Request Guidelines
Recent history uses short, direct subjects (for example, `November update`, `Update 3_trump_tariffs.R`, `Fixing endnotes.`).

- Keep commit titles concise, imperative, and scoped to one change.
- PRs should include: what changed, why it changed, which scripts were run, and which output files changed.
- Include chart previews/screenshots for visual changes and link relevant CPI/BLS context when applicable.

## Security & Configuration Tips
- Do not commit secrets or local absolute paths (for example, BEA API key files outside the repo).
- Keep `.Rhistory`, temporary files, and machine-specific artifacts out of commits.
