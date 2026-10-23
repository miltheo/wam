# Reproduce the analyses

[Repository](../README.md) &middot; [Output index](OUTPUTS.md) &middot; [Master observations](../data/README.md)

Run commands from the WAM repository root. The workflow starts from master
observations; native recordings and 3D-printing software are not required.

## Installation

Install [Metronautica](https://github.com/miltheo/metronautica) and its runtime
dependencies following its installation guide. For a downloaded source directory,
run in R:

```r
install.packages("path/to/metronautica", repos = NULL, type = "source")
```

Install WAM dependencies in R; dplyr >= 1.1.0 is required:

```r
install.packages(c("dplyr", "tidyr", "tibble", "ggplot2", "nlme", "digest",
                   "ragg", "gridExtra", "scales", "ggrepel", "png"))
```

## Metronautica compatibility

Tested with **metronautica 0.1.0**, clean source commit
[`eae8c2f566ef51986df55e1d32d957d4162bd42d`](https://github.com/miltheo/metronautica/commit/eae8c2f566ef51986df55e1d32d957d4162bd42d).
WAM checks for metronautica >= 0.1.0 and the required public functions. Run setup
and integration checks after changing the installed engine.

The masters retain the observations used for the article. Installing an updated
parser, filter or count implementation does not recalculate those observations.
Package-level processing examples belong to Metronautica; WAM supplies the
cohort analyses and article-asset assembly.

## First run

In a terminal:

```sh
Rscript code/run.R --check
Rscript code/run.R --list
Rscript code/run.R static
```

`--check` verifies dependencies, the package interface, paths and master checksums
without running an analysis. The static command runs one independent stage.
For the complete workflow:

```sh
Rscript code/run.R all
Rscript code/checks/check_results.R
```

`all` executes the nine stages in order. Feature bootstrap analyses and figure
rendering can take longer than setup checks; allow the process to finish.

## Stages

| Stage command | Analysis | Required completed stages |
|---|---|---|
| `static` | Static group comparisons and figure | None |
| `dynamic` | Primary/secondary dynamic and processing comparisons | None |
| `features` | Response, equivalence, regional post hoc tests and figures | None |
| `diagnostics` | Signal, reference-timing, Allan and spectral summaries | None |
| `sensitivity` | Configuration, pairing and event-alignment sensitivity | `dynamic` |
| `frequency` | Supplementary frequency estimators and tracking | `features` |
| `activation` | Low-excitation activation threshold and deadband | None |
| `threshold_transfer` | Exploratory drift and threshold transfer | `static`, `features` |
| `export` | Numbered article assets and manifest | All analysis stages |

Use `Rscript code/run.R features` to run a named stage. Equivalent numbered entry
scripts are in `stages/`. Requested stages do not automatically run prerequisites.
Run `export` after the analyses to populate `results/`; a single analysis stage
normally writes intermediate outputs under `analysis/`.

Threshold transfer applies published ENMO, MAD and Fridolfsson laboratory wrist
SVMbp cutoffs from [`threshold_cutoffs.csv`](settings/threshold_cutoffs.csv).
Durations compare devices within each pipeline; cutoff sets are not matched
across metrics and do not validate behavioural estimates.

## Paths and outputs

| Location | Contents |
|---|---|
| `data/` | Input masters; filenames and SHA-256 digests in `settings/inputs.csv` |
| `analysis/` | Stage statistics, supporting tables, plots and provenance |
| `results/figures/` | Numbered figure PNGs and available PDF counterparts |
| `results/tables/` | Numbered CSV tables and LaTeX fragments |
| `results/assets.csv` | Registered final assets and SHA-256 digests |

The [output index](OUTPUTS.md) maps article numbers to producing stages and paths.
Only registered, code-generated assets are exported. Source registries are
`settings/figures.csv` and `settings/tables.csv`.

Override paths before launching R through `WAM_MASTER_ROOT`, `WAM_ANALYSIS_ROOT`
and `WAM_RESULTS_ROOT`. `WAM_R_LIBRARY` selects an additional R library. Keep
masters and both output directories separate. Outputs remain excluded from Git.
LaTeX fragments use `longtable`, `array`, `booktabs` and `pdflscape`.

Each completed stage writes `analysis/provenance/<stage>_session.txt`.
`analysis/provenance/metronautica_api.csv` records function hashes from the engine
used by the most recently completed stage. Retain these records with the outputs;
use a consistent engine environment for all stages of a reproduction run.

## Verification

| Terminal command | When to run | Verifies |
|---|---|---|
| `Rscript code/run.R --check` | Before analysis | Dependencies, API, paths and input checksums |
| `Rscript code/checks/check_integration.R` | After engine installation/update | Package API, known-signal and statistical contracts |
| `Rscript code/checks/check_tools.R` | Before analysis | R syntax, input integrity, registries and export formatting |
| `Rscript code/checks/check_results.R` | After the complete workflow | Exported assets, hashes and expected study findings |
| `Rscript code/checks/check_threshold_transfer.R` | After threshold transfer and export | Published cutoffs, master-derived features and illustrative durations |
| `Rscript code/checks/check_samples.R` | When using optional samples | Catalogue and available recording checksums |
| `Rscript code/checks/check_samples.R --require-all` | Before distributing all attachments | All nine compressed and restored recording checksums |

Checks do not refit the models. Missing optional sample payloads are permitted
unless `--require-all` is supplied.

## Troubleshooting

| Message or symptom | Action |
|---|---|
| Missing package or API | Install the listed dependencies and the documented Metronautica revision; rerun `--check` |
| Missing master/checksum mismatch | Restore the supplied master identified in `settings/inputs.csv`; check `WAM_MASTER_ROOT` |
| Missing intermediate output | Run the stage prerequisites before the requested stage |
| No numbered files after one analysis | Complete the analyses, then run `export` |
| Missing sample attachment | Follow the [sample guide](../samples/README.md); samples are independent of the master-based workflow |
| Graphics/font or numerical differences | Record `sessionInfo()` and stage provenance when reporting the difference |

## Source layout

`config.R` sets paths; `setup.R` checks the environment and dispatches stages.
`analysis/` holds statistical functions, `figures/` holds plot construction,
`utilities/` holds table/export/documentation helpers, `checks/` holds verification,
and `settings/` holds the input and asset registries. `run.R` is the command-line entry.

For interactive R use from the WAM root:

```r
source("code/config.R")
source("code/setup.R")
cfg <- wam_config()
wam_check_setup(cfg)
wam_load_code(cfg)
wam_static(cfg)
```

This final call computes static statistics; the stage command also renders its
figure. Measurement definitions remain in the article and Metronautica reference.

## Documentation maintenance

Regenerate `OUTPUTS.md` from the asset registries in R:

```r
source("code/utilities/documentation.R")
wam_write_output_index()
```

The index contains paths and stage mappings, not generated results. No model fits
or figure rendering are performed by this helper.

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
Code, observations and mounting assets: [GPL-3.0-only](../LICENSE).
