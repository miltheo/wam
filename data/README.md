# Master observations

Inputs for the analyses in the research article under submission.
Byte counts and SHA-256 checksums: [inputs.csv](../code/settings/inputs.csv).
RDS files are read with `readRDS()`; CSV files with `read.csv()`.

| File | Observations |
|---|---|
| `static/static.rds` | Unit-level static outcomes |
| `static/static_allan_curves.csv` | Unit/axis Allan-deviation curves |
| `static/static_axis_diagnostics.csv` | Stationary axis diagnostics |
| `dynamic/dynamic_primary_overall.rds` | Primary trial-level dynamic outcomes |
| `dynamic/dynamic_primary_stages.rds` | Primary stage-level dynamic outcomes |
| `dynamic/dynamic_secondary_overall.rds` | Secondary trial-level dynamic outcomes |
| `dynamic/dynamic_secondary_stages.rds` | Secondary stage-level dynamic outcomes |
| `dynamic/configuration_overall.rds` | Trial-level acquisition-configuration outcomes |
| `dynamic/configuration_stages.rds` | Stage-level acquisition-configuration outcomes |
| `features/features_primary.rds` | Primary native/calibrated feature observations |
| `features/features_secondary_native.rds` | Secondary native feature observations |
| `features/features_secondary_calibrated.rds` | Secondary calibrated feature observations |
| `features/frequency_stages.csv` | Stage-level frequency estimates |
| `features/gravity_orientation_stages.csv` | Gravity-vector orientation observations |
| `dynamic/activation_stages.rds` | Low-excitation response observations |
| `diagnostics/signal_diagnostics.csv` | Recording/stage-level diagnostic outcomes |
| `diagnostics/spectral_audit.csv` | Axis-specific spectral observations |
| `diagnostics/timing_stages.csv` | Within-stage reference-timing diagnostics |
| `diagnostics/timing_pooled.csv` | Pooled reference-timing diagnostics |
| `diagnostics/pairing_trials.csv` | Trial outcomes under alternative reference pairing |
| `diagnostics/event_stage_outcomes.csv` | Stage outcomes under alternative event alignment |
| `diagnostics/event_trial_outcomes.csv` | Trial outcomes under alternative event alignment |

## Row structure and identifiers

| Master family | One row represents | Identifiers and interpretation |
|---|---|---|
| Static outcomes | One physical unit | `instrument`, `id`; axis-prefixed fields describe X/Y/Z or VM |
| Dynamic overall | One unit, trial and processing pipeline | `instrument`, `id`, `trial_start_utc`, `pipeline`; secondary/configuration files also retain trial identifiers |
| Dynamic stages | One unit, trial, pipeline and excitation stage | Overall identifiers plus `stage`; `rpm.x`/`rpm.y` are retained joined labels; configuration files additionally supply `rpm` |
| Feature outcomes | One unit, trial, stage, feature and signal pipeline | `device_id`, `trial_start_utc`, `stage_name`, `metric`, `signal_pipeline`; `metric_unit` specifies the value unit; inspect `was_computed`/`error_message` before use |
| Activation stages | One unit and low-excitation stage | `instrument`, `id`, `stage_index`, `nominal_rpm`; measured excitation and reference fields remain separate |
| Diagnostic and auxiliary CSVs | The recording, stage, axis or pairing named by their identifier columns | Retain those identifiers when joining; these files have different row structures |

Primary comparisons cover LEAP, AX3, GENEActiv and SENS motion; secondary
comparisons cover LEAP, wGT3X-BT and GT9X Link. `native` denotes the decoded signal
without the additional dynamic VM calibration; calibrated observations retain
their signal-pipeline and calibration fields. Each file stores observations,
not independent participants or already pooled device estimates.

For outcome definitions and units, use [Metronautica](https://github.com/miltheo/metronautica)'s installed
`help("measurement-definitions", package = "metronautica")` and linked function
references. Feature values follow `metric_unit`; suffixes such as `_g`, `_mg`,
`_hz` and `_percent` identify units where present. Unsuffixed legacy fields must
be interpreted using the corresponding function reference; a fraction is not
a percentage. Study-level definitions remain in the research article.

Set `WAM_MASTER_ROOT` to relocate these inputs. Optional
[native samples](../samples/README.md) are separate from the full-cohort masters.

The master observations are distributed under **GPL-3.0-only**, the same licence
as the WAM code; see [LICENSE](../LICENSE).

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
