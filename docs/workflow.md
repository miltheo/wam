# Master-to-assets workflow

Install metronautica 0.1.0 or later with the current public functions. WAM checks
the required exports; it does not source or modify the package namespace.
Measurement calculations remain in Metronautica. Statistical models and the
supplementary frequency-method comparison remain in WAM.

| Command/stage | Required prior stages | Main work |
|---|---|---|
| `01_static.R` | None | Static unit-level comparisons and selected estimation figure |
| `02_dynamic.R` | None | Primary and secondary native models, within-stage post-hoc comparisons, processing comparisons and selected figures |
| `03_features.R` | None | Four feature arms, response bootstrap, 10% decisions, descriptive tolerances, regional Holm adjustment and feature figures |
| `04_diagnostics.R` | None | Five-rule diagnostic summaries, reference timing, Allan outcome plots and spectral filter-feasibility figure |
| `05_sensitivity.R` | Dynamic | Configuration comparisons and outcome-level pairing/event-alignment sensitivity models |
| `06_frequency.R` | Features | Supplementary estimator comparison and native dominant-frequency tracking |
| `07_activation.R` | None | Sustained deadband/activation crossings, summaries and low-excitation figure |
| `08_threshold_transfer.R` | Static, features | Exploratory drift and threshold-transfer scenarios |
| `09_export.R` | All above | Selected numbered figures, full-precision tables and LaTeX fragments |

Intermediate files are written to `analysis/`, with stable descriptive filenames.
Final assets are written to `results/` according to `config/figures.csv` and
`config/tables.csv`. The exporter fails if required products are missing; it never
substitutes saved publication figures or test-reference results.

To relocate data or outputs, set:

```text
WAM_MASTER_ROOT = directory containing the named master files
WAM_ANALYSIS_ROOT = directory for current-run intermediates
WAM_RESULTS_ROOT = directory for final result assets
WAM_PYTHON = Python executable with the rendering dependencies
WAM_R_LIBRARY = optional R library directory
```

These variables are optional. Input hashes must still match `config/inputs.csv`.
Input and output locations must be separate. Paths are resolved from the repository root or these environment variables.

The regional post-hoc results do not replace the unadjusted pair-stage counts
in the main equivalence plots. They answer a different, selection-adjusted
question and are exported separately. The default margin and full-range response
criterion are unchanged.

Validation before release should include a complete run and numerical/visual
comparison against the manuscript assets. Setup and contract checks
alone do not establish that full reproduction has passed.
Run `Rscript tests/check_results.R` after the complete workflow to check the
generated assets and key study results. This check reads existing outputs and
does not refit analyses.

Generated LaTeX fragments use `longtable`, `array`, `booktabs` and `pdflscape`.
The CSV exports retain full numerical precision; final manuscript typography is
checked separately during the complete reproduction review.
