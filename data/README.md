# Master observations

Inputs for the analyses in the research article under submission.
Byte counts and SHA-256 checksums: [inputs.csv](../code/inputs.csv).
RDS files are read with `readRDS()`; CSV files with `read.csv()`.

| File | Observations |
|---|---|
| `static.rds` | Unit-level static outcomes |
| `static_allan_curves.csv` | Unit/axis Allan-deviation curves |
| `static_axis_diagnostics.csv` | Stationary axis diagnostics |
| `dynamic_primary_overall.rds` | Primary trial-level dynamic outcomes |
| `dynamic_primary_stages.rds` | Primary stage-level dynamic outcomes |
| `dynamic_secondary_overall.rds` | Secondary trial-level dynamic outcomes |
| `dynamic_secondary_stages.rds` | Secondary stage-level dynamic outcomes |
| `configuration_overall.rds` | Trial-level acquisition-configuration outcomes |
| `configuration_stages.rds` | Stage-level acquisition-configuration outcomes |
| `features_primary.rds` | Primary native/calibrated feature observations |
| `features_secondary_native.rds` | Secondary native feature observations |
| `features_secondary_calibrated.rds` | Secondary calibrated feature observations |
| `frequency_stages.csv` | Stage-level frequency estimates |
| `gravity_orientation_stages.csv` | Gravity-vector orientation observations |
| `activation_stages.rds` | Low-excitation response observations |
| `signal_diagnostics.csv` | Recording/stage-level diagnostic outcomes |
| `spectral_audit.csv` | Axis-specific spectral observations |
| `timing_stages.csv` | Within-stage reference-timing diagnostics |
| `timing_pooled.csv` | Pooled reference-timing diagnostics |
| `pairing_trials.csv` | Trial outcomes under alternative reference pairing |
| `event_stage_outcomes.csv` | Stage outcomes under alternative event alignment |
| `event_trial_outcomes.csv` | Trial outcomes under alternative event alignment |

Set `WAM_MASTER_ROOT` to relocate these inputs. Definitions and study methods
are provided in the associated research article under submission; package-level
documentation is maintained with Metronautica (repository link pending).

Optional native recordings are documented in [samples](../samples/README.md).
They are independent of these full-cohort analysis inputs.

Data-distribution terms are separate from the [GPL-3 software licence](../LICENSE).
No data licence is assigned here.

Maintainer: [Millen J. Theophilus](https://github.com/miltheo).
