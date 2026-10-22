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

Set `WAM_MASTER_ROOT` to relocate these inputs. Definitions and study methods
are provided in the associated research article under submission; package-level
documentation is maintained with [Metronautica](https://github.com/miltheo/metronautica).

Optional native recordings are documented in [samples](../samples/README.md).
They are independent of these full-cohort analysis inputs.

The master observations are distributed under **GPL-3.0-only**, the same licence
as the WAM code; see [LICENSE](../LICENSE).

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
