# Study code

R code is grouped into `stages/`, `analysis/`, `figures/`, `utilities/`,
`checks/` and `settings/`. `run.R`, `setup.R` and `config.R` are the entry and setup files. Run commands
from the WAM root.

## Installation

Install metronautica from its source directory:

```sh
R CMD INSTALL /path/to/metronautica
```

Install the study dependencies in R (dplyr >= 1.1.0):

```r
install.packages(c("dplyr", "tidyr", "tibble", "ggplot2", "nlme", "digest",
                   "ragg", "gridExtra", "scales", "ggrepel", "png"))
```

## Execution

```sh
Rscript code/run.R --check
Rscript code/run.R all
Rscript code/checks/check_results.R
```

`run.R` accepts `--list`, `--check`, a stage name or `all`.

| Entry point | Stage | Prerequisites |
|---|---|---|
| `stages/01_static.R` | `static` | None |
| `stages/02_dynamic.R` | `dynamic` | None |
| `stages/03_features.R` | `features` | None |
| `stages/04_diagnostics.R` | `diagnostics` | None |
| `stages/05_sensitivity.R` | `sensitivity` | `dynamic` |
| `stages/06_frequency.R` | `frequency` | `features` |
| `stages/07_activation.R` | `activation` | None |
| `stages/08_threshold_transfer.R` | `threshold_transfer` | `static`, `features` |
| `stages/09_export.R` | `export` | All preceding stages |

Example: `Rscript code/stages/03_features.R` or `Rscript code/run.R features`.

## R modules

| File(s) | Function |
|---|---|
| `config.R`, `setup.R` | Paths, dependency checks, module loading and stage dispatch |
| `analysis/static.R`, `figures/static_figure.R` | Static statistics and figure |
| `analysis/dynamic_native.R`, `analysis/dynamic_secondary.R` | Primary/secondary native dynamic statistics |
| `analysis/dynamic_processing.R`, `figures/composite_figure.R` | Processing comparisons and composite-error figure |
| `figures/dynamic_figure.R` | Dynamic profiles |
| `analysis/configuration.R` | Acquisition-configuration sensitivity |
| `analysis/features.R`, `analysis/feature_statistics.R` | Feature response, equivalence and bootstrap intervals |
| `analysis/regional_equivalence.R` | Joint regional tests and Holm adjustment |
| `figures/feature_figures.R`, `figures/context_figures.R` | Feature, frequency-tracking and orientation figures |
| `analysis/frequency.R`, `figures/frequency_figure.R` | Supplementary frequency estimators |
| `analysis/activation.R`, `figures/activation_figure.R` | Activation threshold and deadband |
| `analysis/diagnostics.R` | Signal/reference diagnostics and Allan/spectral figures |
| `analysis/timing_sensitivity.R` | Pairing and event-alignment sensitivity |
| `analysis/threshold_transfer.R` | Exploratory drift and threshold transfer |
| `analysis/export.R` | Condition mapping and final asset assembly |

`wam_load_code()` loads function modules only; it excludes entry points and checks.
For interactive use:

```r
source("code/config.R")
source("code/setup.R")
cfg <- wam_config()
wam_check_setup(cfg)
wam_load_code(cfg)
wam_static(cfg)
```

## R utilities

| File | Function |
|---|---|
| `utilities/diagnostic_tables.R` | Aggregate five-rule diagnostics |
| `utilities/feature_tables.R` | Export equivalence/response tables |
| `figures/spectral_figure.R` | Render spectral peaks and filter feasibility |
| `figures/device_clips.R` | Assemble the hardware figure |
| `utilities/export_assets.R` | Export numbered assets and checksums |

`utilities/table_io.R` preserves numeric strings during CSV/LaTeX export; `figures/figure_devices.R`
provides PNG/PDF graphics devices.

## Analysis inputs

| File | Content |
|---|---|
| `settings/inputs.csv` | Master filenames, sizes and SHA-256 checksums |
| `settings/feature_pipelines.csv` | Pipeline labels, units and response inputs |
| `settings/condition_mapping.csv` | RPM, frequency, acceleration and behavioural analogues |
| `settings/threshold_cutoffs.csv` | ENMO/MAD cutoffs and source references |
| `settings/figures.csv`, `settings/tables.csv` | Generated-asset mappings to article numbering |

## Paths and outputs

`config.R` defaults to `data/` for grouped masters, `analysis/` for intermediate outputs
and `results/` for numbered assets. Override these paths through `WAM_MASTER_ROOT`,
`WAM_ANALYSIS_ROOT` and `WAM_RESULTS_ROOT`. `WAM_R_LIBRARY` selects an additional
R library. Input and output directories must be separate.

Generated outputs are ignored by Git. CSV tables retain full precision; LaTeX
fragments require `longtable`, `array`, `booktabs` and `pdflscape`.

Definitions and methodological specifications are provided in the research article
under submission and the Metronautica package documentation (repository link pending).

## Verification

Run from the WAM root. Checks do not refit study models.

| Command | Verification |
|---|---|
| `Rscript code/checks/check_integration.R` | Metronautica API and numerical/statistical contracts |
| `Rscript code/checks/check_tools.R` | Source syntax, input integrity and export formatting |
| `Rscript code/checks/check_results.R` | Completed assets and study-result checks |
| `Rscript code/checks/check_samples.R` | Catalogue and available sample checksums |
| `Rscript code/checks/check_samples.R --require-all` | Require and verify all nine sample payloads |

`checks/check_results.R` requires a completed workflow. Missing optional sample payloads
are permitted unless `--require-all` is supplied.

Maintainer: [Millen J. Theophilus](https://github.com/miltheo).
