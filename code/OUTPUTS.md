# Article output index

Generated from [figures.csv](settings/figures.csv) and [tables.csv](settings/tables.csv).
Run the [required stages](README.md#stages), then `export`.
Figures are written to `results/figures/`, tables to `results/tables/` and
checksums to `results/assets.csv`. Only registered assets are exported.

## Figures

| Asset | Description | Producing stage | Exported file |
|---|---|---|---|
| Figure 3 | Static characterisation | [`static`](stages/01_static.R) | `figure-3.png` |
| Figure 4 | Native dynamic response: primary comparison | [`dynamic`](stages/02_dynamic.R) | `figure-4.png` |
| Figure 5 | Composite error across processing pipelines | [`dynamic`](stages/02_dynamic.R) | `figure-5.png` |
| Figure 6 | Feature equivalence: primary comparison | [`features`](stages/03_features.R) | `figure-6.png` |
| Figure 7 | Feature response: primary comparison | [`features`](stages/03_features.R) | `figure-7.png` |
| Figure 8 | Exploratory drift and threshold transfer | [`threshold_transfer`](stages/08_threshold_transfer.R) | `figure-8.png` |
| Figure S1 | Device mounting clips | [`export`](stages/09_export.R) | `figure-S1.png` |
| Figure S2 | Representative Allan deviation | [`diagnostics`](stages/04_diagnostics.R) | `figure-S2.png` |
| Figure S3 | Native dynamic response: secondary comparison | [`dynamic`](stages/02_dynamic.R) | `figure-S3.png` |
| Figure S4 | Gravity-vector orientation | [`features`](stages/03_features.R) | `figure-S4.png` |
| Figure S5 | VM integral non-linearity across processing pipelines | [`dynamic`](stages/02_dynamic.R) | `figure-S5.png` |
| Figure S6 | Axis-specific spectral diagnostics | [`diagnostics`](stages/04_diagnostics.R) | `figure-S6.png` |
| Figure S7 | Pairwise feature equivalence: primary comparison | [`features`](stages/03_features.R) | `figure-S7.png` |
| Figure S8 | Feature-pipeline criteria: primary comparison | [`features`](stages/03_features.R) | `figure-S8.png` |
| Figure S9 | Feature equivalence: secondary comparison | [`features`](stages/03_features.R) | `figure-S9.png` |
| Figure S10 | Feature response: secondary comparison | [`features`](stages/03_features.R) | `figure-S10.png` |
| Figure S11 | Pairwise feature equivalence: secondary comparison | [`features`](stages/03_features.R) | `figure-S11.png` |
| Figure S12 | Exploratory MAD threshold transfer | [`threshold_transfer`](stages/08_threshold_transfer.R) | `figure-S12.png` |
| Figure S13 | Frequency tracking | [`frequency`](stages/06_frequency.R) | `figure-S13.png` |
| Figure S14 | Frequency-estimator comparison | [`frequency`](stages/06_frequency.R) | `figure-S14.png` |
| Figure S15 | Low-excitation response | [`activation`](stages/07_activation.R) | `figure-S15.png` |

Available PDFs are copied beside figure PNGs.

## Tables

| Asset | Description | Producing stage | Exported file |
|---|---|---|---|
| Table 2 | Device-level 24-hour static metrology | [`static`](stages/01_static.R) | `table-2.csv` |
| Table 3 | Native dynamic metrology in the primary comparison | [`dynamic`](stages/02_dynamic.R) | `table-3.csv` |
| Table 4 | Regional equivalence and full-range criteria in the primary comparison | [`export`](stages/09_export.R) | `table-4.csv` |
| Table S3 | Signal diagnostics in static and dynamic recordings | [`diagnostics`](stages/04_diagnostics.R) | `table-S3.csv` |
| Table S4 | Reference-timing diagnostics for native dynamic signals | [`diagnostics`](stages/04_diagnostics.R) | `table-S4.csv` |
| Table S5 | Sensitivity to reference pairing and apparent-event alignment | [`export`](stages/09_export.R) | `table-S5.csv` |
| Table S7 | Shaker conditions and behavioural analogues | [`features`](stages/03_features.R) | `table-S7.csv` |
| Table S8 | Low-excitation deadband and activation threshold | [`activation`](stages/07_activation.R) | `table-S8.csv` |
| Table S10 | SENS motion quantisation and Allan minima | [`diagnostics`](stages/04_diagnostics.R) | `table-S10.csv` |
| Table S11 | Native dynamic metrology in the secondary comparison | [`dynamic`](stages/02_dynamic.R) | `table-S11.csv` |
| Table S12 | Acquisition-configuration sensitivity | [`sensitivity`](stages/05_sensitivity.R) | `table-S12.csv` |
| Table S13 | Native feature criteria in the secondary comparison | [`export`](stages/09_export.R) | `table-S13.csv` |
| Table S14 | Post hoc regional equivalence across both native comparisons | [`features`](stages/03_features.R) | `table-S14.csv` |

Each CSV also has a `.tex` fragment.

<details>
<summary>Intermediate paths</summary>

Paths are relative to the repository root with default output settings.

| Asset | Intermediate source |
|---|---|
| Figure 3 | `analysis/static/figures/static_profile_300dpi.png` |
| Figure 4 | `analysis/dynamic_native/figures/native_dynamic_profile_300dpi.png` |
| Figure 5 | `analysis/dynamic_processing/figures/composite_error_300dpi.png` |
| Figure 6 | `analysis/features/primary_native/figures/equivalence_10.png` |
| Figure 7 | `analysis/features/primary_native/figures/response_10.png` |
| Figure 8 | `analysis/threshold_transfer/threshold-transfer.png` |
| Figure S1 | `analysis/hardware/device_clips.png` |
| Figure S2 | `analysis/diagnostics/figures/allan_representative.png` |
| Figure S3 | `analysis/secondary_native/figures/native_dynamic_profile_300dpi.png` |
| Figure S4 | `analysis/features/context/orientation_context.png` |
| Figure S5 | `analysis/dynamic_processing/figures/processing_linearity_300dpi.png` |
| Figure S6 | `analysis/diagnostics/figures/spectral_audit.png` |
| Figure S7 | `analysis/features/primary_native/figures/pairs_10.png` |
| Figure S8 | `analysis/features/primary_pipeline_10.png` |
| Figure S9 | `analysis/features/secondary_native/figures/equivalence_10.png` |
| Figure S10 | `analysis/features/secondary_native/figures/response_10.png` |
| Figure S11 | `analysis/features/secondary_native/figures/pairs_10.png` |
| Figure S12 | `analysis/threshold_transfer/mad-threshold-transfer.png` |
| Figure S13 | `analysis/frequency/figures/native_frequency_tracking.png` |
| Figure S14 | `analysis/frequency/figures/frequency_estimators_600dpi.png` |
| Figure S15 | `analysis/activation/figures/activation_response.png` |
| Table 2 | `analysis/static/static_manuscript_table.csv` |
| Table 3 | `analysis/dynamic_native/tables/native_dynamic_profile_manuscript_table.csv` |
| Table 4 | `analysis/features/tables/primary_native_10pct.csv` |
| Table S3 | `analysis/diagnostics/combined_results_table.csv` |
| Table S4 | `analysis/diagnostics/timing_summary.csv` |
| Table S5 | `analysis/timing_sensitivity/summary.csv` |
| Table S7 | `analysis/features/condition_mapping.csv` |
| Table S8 | `analysis/activation/tables/Table_activation_deadband_summary_wide.csv` |
| Table S10 | `analysis/diagnostics/static_axis_summary.csv` |
| Table S11 | `analysis/secondary_native/tables/secondary_native_dynamic_profile_manuscript_table.csv` |
| Table S12 | `analysis/configuration/tables/configuration_sensitivity_compact_table_with_p.csv` |
| Table S13 | `analysis/features/tables/secondary_native_10pct.csv` |
| Table S14 | `analysis/features/regional_posthoc/pipeline_results.csv` |

The export stage assembles feature tables, the clip preview figure and the
condensed timing-sensitivity summary from completed analyses.

</details>
