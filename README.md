# Wearable Accelerometers Metrology (WAM)

Version 0.1.0

Code and master observations for reproducing the statistical results and
code-generated manuscript figures and tables of the wearable accelerometer
metrology study. The companion **metronautica R package (version 0.1.0)** supplies
native decoding, signal diagnostics, clipping, static/dynamic metrology,
filtering and time-domain features. WAM supplies the study design, statistical
comparisons, exploratory scenarios and result presentation.

The workflow starts from the master sheets. It does not decode the complete raw
recordings or automatically regenerate the masters. Complete original recordings are
separate from this workflow.

## Install

Install metronautica 0.1.0 from its downloaded source folder (replace the path
below with that folder), then install the study dependencies:

```sh
R CMD INSTALL /path/to/metronautica
```

```r
install.packages(c("dplyr", "tidyr", "tibble", "ggplot2", "nlme", "digest",
                   "ragg", "gridExtra", "scales", "ggrepel"))
```

Figure assembly also uses Python:

```sh
python -m pip install -r requirements-rendering.txt
```

Set `WAM_PYTHON` if the intended Python executable is not named `python`.
No analysis function installs packages, changes the working directory or sources
a private Metronautica engine path.

## Run in parts

From the WAM root:

```sh
Rscript scripts/run.R --list
Rscript scripts/run.R --check
Rscript tests/check_integration.R
Rscript scripts/01_static.R
Rscript scripts/02_dynamic.R
Rscript scripts/03_features.R
Rscript scripts/04_diagnostics.R
Rscript scripts/05_sensitivity.R
Rscript scripts/06_frequency.R
Rscript scripts/07_activation.R
Rscript scripts/08_threshold_transfer.R
Rscript scripts/09_export.R
```

`--list`, `--check` and the integration checks do not run the study analyses.
`Rscript scripts/run.R all` explicitly runs the complete master-to-assets sequence.
After a complete run, `Rscript tests/check_results.R` checks the generated asset
hashes, stage provenance and key study findings.
The workflow guide lists prerequisites for individual stages. Models and the
5,000-replicate response bootstrap can take time; setup never launches them.

To work interactively:

```r
source("config.R")
source("R/setup.R")
cfg <- wam_config()
wam_check_setup(cfg)
wam_load_code(cfg)
wam_static(cfg)
```

## Repository structure

| Folder | Purpose |
|---|---|
| `R/` | Callable study analyses and figure/table assembly |
| `scripts/` | Numbered commands and explicit stage dispatcher |
| `config/` | Feature specifications, thresholds, input checksums and asset registries |
| `data/masters/` | Observational inputs; no fitted result objects or rendered figures |
| `data/samples/` | Sample catalogue and provenance; native payloads distributed as release attachments |
| `hardware/device-clips/` | Six mounting-clip models, previews and model dimensions |
| `tools/` | Python diagnostic aggregation and final asset assembly |
| `tests/` | Small integration and statistical-contract checks |
| `analysis/` | Generated intermediate results from the current run; initially empty |
| `results/` | Generated numbered result assets; initially empty |

Runtime directories are ignored by Git and are created when needed. Rerunning a
stage replaces its current outputs. `config/inputs.csv` verifies the selected masters before execution.
Completed stages record the R session and Metronautica version in
`analysis/provenance/`.

The export registry covers 21 generated figures (including the device clips) and
13 numerical result tables. Tables are exported as full-precision CSVs and LaTeX
fragments. Authored framework/setup artwork, manuscript prose and definition
tables remain with the manuscript; they are not copied into WAM.

## Scientific conventions

The decision criterion is the pre-established 10% feature-specific full-range
dynamic-span margin. Within-figure 5% and 15% comparisons remain descriptive.
Regional pairwise equivalence and full-range controlled response are separate
requirements. The post-hoc regional assessment combines both native device
comparisons and adjusts all 48 pipeline-region tests using Holm's procedure.

Repeated trials are averaged within physical units for device-level inference.
Exploratory static-drift projections and threshold transfer do not validate
behavioural classification. Scientific definitions are documented in
[the study definitions](docs/measurement_definitions.md).

See [the workflow guide](docs/workflow.md) and [input guide](data/README.md).

## Native examples for Metronautica

The sample-data release provides optional single-unit static and dynamic recordings
and the primary HBK reference. The compressed recordings are release attachments,
excluded from Git history. Their catalogue and provenance are in
`data/samples/native/`. They demonstrate Metronautica and are independent of WAM's
master-to-assets workflow. See [sample access](data/samples/README.md).

The [fixed study feature settings](config/feature_generation_settings.md) preserve
the MIMS variants, measurement spans and stationary Activity Index noise
parameters independently of the package defaults. The workflow continues to
read the supplied masters.


## Licence

The original WAM software is licensed under GNU GPL version 3 only
(SPDX: GPL-3.0-only); see [LICENSE](LICENSE). Existing third-party notices and
licences remain applicable to their respective components. The code licence
does not replace the separately documented data-distribution terms.
