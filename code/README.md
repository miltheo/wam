# Reproduce the analyses

[Repository](../README.md) · [Workflow](../README.md#workflow) · [Output index](OUTPUTS.md)

Run commands from the WAM repository root. Inputs are the supplied master
observations; native sample recordings are optional.

## Installation

Requires R >= 4.1 and dplyr >= 1.1.0. Install
[Metronautica and its dependencies](https://github.com/miltheo/metronautica#installation).
For a downloaded package directory, run in **R**:

```r
install.packages("path/to/metronautica", repos = NULL, type = "source")
install.packages(c("dplyr", "tidyr", "tibble", "ggplot2", "nlme", "digest",
                   "ragg", "gridExtra", "scales", "ggrepel", "png"))
```

<a name="metronautica-compatibility"></a>

<details>
<summary>Metronautica compatibility</summary>

WAM requires **metronautica >= 0.1.0** and checks its public functions.
Tested source reference:
[`eae8c2f566ef51986df55e1d32d957d4162bd42d`](https://github.com/miltheo/metronautica/commit/eae8c2f566ef51986df55e1d32d957d4162bd42d).
Run the integration check after changing the installed engine.

Updating a parser, filter or count implementation does not recalculate the
stored master observations. Package processing examples and function definitions
remain in Metronautica; WAM performs the cohort analyses and asset assembly.

</details>

## Reproduce everything

In a **terminal**:

```sh
Rscript code/run.R --check
Rscript code/run.R all
Rscript code/checks/check_results.R
```

`--check` verifies dependencies, package API, paths and master checksums without
running analyses. `all` runs all nine stages, including export. The final command
checks completed outputs; it does not rerun analyses. Bootstrap calculations and
figure rendering can take longer than setup checks.

## Run a selected stage

```sh
Rscript code/run.R --list
Rscript code/run.R static
```

A named stage writes its intermediate outputs under `analysis/`; `static` includes
statistics and its figure. **Prerequisites are not run automatically.**

<a name="stages"></a>

| Stage | Analysis | Required completed stages |
|---|---|---|
| `static` | Static comparisons and figure | None |
| `dynamic` | Primary/secondary dynamic and processing comparisons | None |
| `features` | Feature response, equivalence and regional post hoc tests | None |
| `diagnostics` | Signal, timing, Allan and spectral summaries | None |
| `sensitivity` | Configuration, pairing and event-alignment sensitivity | `dynamic` |
| `frequency` | Supplementary frequency estimators and tracking | `features` |
| `activation` | Low-excitation activation threshold and deadband | None |
| `threshold_transfer` | Exploratory drift and threshold transfer | `static`, `features` |
| `export` | Numbered assets and manifest | All analysis stages |

Run `Rscript code/run.R export` after all analysis stages to populate `results/`.
Equivalent numbered entry scripts are in [stages/](stages/).

Threshold transfer uses the published ENMO, MAD and Fridolfsson laboratory wrist
SVMbp cutoffs in [threshold_cutoffs.csv](settings/threshold_cutoffs.csv).
Durations compare devices within each pipeline; cutoff sets are not matched
across metrics and do not validate behavioural estimates.

## Find the outputs

| Location | Contents |
|---|---|
| `analysis/` | Intermediate statistics, tables, plots and stage session records |
| `results/figures/` | Numbered figure PNGs and available PDFs |
| `results/tables/` | Numbered CSV tables and LaTeX fragments |
| `results/assets.csv` | Exported assets and SHA-256 digests |

The [output index](OUTPUTS.md) lists asset descriptions, producing stages and
filenames, with detailed intermediate paths available below its main tables.
Generated outputs remain excluded from Git.

## Verification

The full-run commands above cover setup and completed-result checks.

<details>
<summary>Additional checks</summary>

Run these in a terminal from the WAM root:

| Command | Purpose |
|---|---|
| `Rscript code/checks/check_integration.R` | Metronautica API, known-signal and statistical contracts |
| `Rscript code/checks/check_tools.R` | R syntax, master integrity, registries and export formatting |
| `Rscript code/checks/check_threshold_transfer.R` | Published cutoffs and illustrative durations after threshold transfer and export |
| `Rscript code/checks/check_samples.R` | Catalogue and available optional recording checksums |
| `Rscript code/checks/check_samples.R --require-all` | All nine included native recordings |

Checks do not refit models. Missing optional samples are permitted unless
`--require-all` is supplied.

</details>

## Additional options

<details>
<summary>Run a stage in R</summary>

From the WAM root:

```r
source("code/config.R")
source("code/setup.R")
cfg <- wam_config()
wam_run("static", cfg)
```

This runs the same stage, including its figure, as the terminal command.
Individual statistical functions are in [analysis/](analysis/).

</details>

<details>
<summary>Paths and runtime records</summary>

Set environment variables before launching R: `WAM_MASTER_ROOT` selects inputs,
`WAM_ANALYSIS_ROOT` intermediate outputs, `WAM_RESULTS_ROOT` exported assets and
`WAM_R_LIBRARY` an additional R library. Input and output directories must be
separate; analysis and results directories must also be non-nested.

Each stage writes `analysis/provenance/<stage>_session.txt`.
`analysis/provenance/metronautica_api.csv` records API hashes for the most recently
completed stage. Keep a consistent engine environment and retain these records
with the outputs. LaTeX fragments require `longtable`, `array`, `booktabs` and
`pdflscape`.

</details>

<details>
<summary>Troubleshooting</summary>

| Symptom | Action |
|---|---|
| Missing package or API | Install requirements; rerun `--check` and the integration check |
| Missing master or checksum mismatch | Check `WAM_MASTER_ROOT` and restore the file listed in [inputs.csv](settings/inputs.csv) |
| Missing intermediate output | Run the required stages first |
| No numbered assets after one stage | Complete the analyses, then run `export` |
| Missing sample attachment | Follow the [sample guide](../samples/README.md) |
| Graphics or numerical differences | Record the command, `sessionInfo()` and stage session record |

</details>

<details>
<summary>Source layout and documentation</summary>

`code/run.R` is the entry point; `config.R` resolves paths and `setup.R` dispatches
stages. Within `code/`, `analysis/` contains statistical functions, `figures/` plot
functions, `utilities/` table/export helpers, `checks/` verification and `settings/`
input/asset registries. These source folders are separate from generated outputs
in the repository-root `analysis/` and `results/` directories.

Regenerate the output index from its registries in R:

```r
source("code/utilities/documentation.R")
wam_write_output_index()
```

This updates documentation only; it runs no analyses or figure rendering.
Measurement definitions remain in the research article and Metronautica reference.

</details>

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
Code, observations and mounting assets: [GPL-3.0-only](../LICENSE).
