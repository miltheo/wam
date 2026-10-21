# Study code

All analysis, execution, verification and rendering code is in this directory. Run commands
from the WAM root.

## Installation

Install metronautica 0.1.0 from its source directory:

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
Rscript code/check_results.R
```

`run.R` accepts `--list`, `--check`, a stage name or `all`.

| Entry point | Stage | Prerequisites |
|---|---|---|
| `01_static.R` | `static` | None |
| `02_dynamic.R` | `dynamic` | None |
| `03_features.R` | `features` | None |
| `04_diagnostics.R` | `diagnostics` | None |
| `05_sensitivity.R` | `sensitivity` | `dynamic` |
| `06_frequency.R` | `frequency` | `features` |
| `07_activation.R` | `activation` | None |
| `08_threshold_transfer.R` | `threshold_transfer` | `static`, `features` |
| `09_export.R` | `export` | All preceding stages |

Example: `Rscript code/03_features.R` or `Rscript code/run.R features`.

## R modules

| File(s) | Function |
|---|---|
| `config.R`, `setup.R` | Paths, dependency checks, module loading and stage dispatch |
| `static.R`, `static_figure.R` | Static statistics and figure |
| `dynamic_native.R`, `dynamic_secondary.R` | Primary/secondary native dynamic statistics |
| `dynamic_processing.R`, `composite_figure.R` | Processing comparisons and composite-error figure |
| `dynamic_figure.R` | Dynamic profiles |
| `configuration.R` | Acquisition-configuration sensitivity |
| `features.R`, `feature_statistics.R` | Feature response, equivalence and bootstrap intervals |
| `regional_equivalence.R` | Joint regional tests and Holm adjustment |
| `feature_figures.R`, `context_figures.R` | Feature, frequency-tracking and orientation figures |
| `frequency.R`, `frequency_figure.R` | Supplementary frequency estimators |
| `activation.R`, `activation_figure.R` | Activation threshold and deadband |
| `diagnostics.R` | Signal/reference diagnostics and Allan/spectral figures |
| `timing_sensitivity.R` | Pairing and event-alignment sensitivity |
| `threshold_transfer.R` | Exploratory drift and threshold transfer |
| `export.R` | Condition mapping and final asset assembly |

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
| `diagnostic_tables.R` | Aggregate five-rule diagnostics |
| `feature_tables.R` | Export equivalence/response tables |
| `spectral_figure.R` | Render spectral peaks and filter feasibility |
| `device_clips.R` | Assemble the hardware figure |
| `export_assets.R` | Export numbered assets and checksums |

`table_io.R` preserves numeric strings during CSV/LaTeX export; `figure_devices.R`
provides PNG/PDF graphics devices.

## Analysis inputs

| File | Content |
|---|---|
| `inputs.csv` | Master filenames, sizes and SHA-256 checksums |
| `feature_pipelines.csv` | Pipeline labels, units and response inputs |
| `condition_mapping.csv` | RPM, frequency, acceleration and behavioural analogues |
| `threshold_cutoffs.csv` | ENMO/MAD cutoffs and source references |
| `figures.csv`, `tables.csv` | Generated-asset mappings to article numbering |

## Paths and outputs

`config.R` defaults to `data/` for masters, `analysis/` for intermediate outputs
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
| `Rscript code/check_integration.R` | Metronautica API and numerical/statistical contracts |
| `Rscript code/check_tools.R` | Source syntax, input integrity and export formatting |
| `Rscript code/check_results.R` | Completed assets and study-result checks |
| `Rscript code/check_samples.R` | Catalogue and available sample checksums |
| `Rscript code/check_samples.R --require-all` | Require and verify all nine sample payloads |

`check_results.R` requires a completed workflow. Missing optional sample payloads
are permitted unless `--require-all` is supplied.

Maintainer: [Millen J. Theophilus](https://github.com/miltheo).
