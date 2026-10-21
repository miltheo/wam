# Fixed study feature settings

WAM reproduces results from the supplied feature masters; it does not rebuild
them from raw recordings. Package defaults must not be used to reinterpret
these observations. The study settings are:

| Setting | Study specification |
| --- | --- |
| Epoch | 5 seconds |
| MIMS_DEFAULT_02_05 | MIMSunit, butter, cutoffs c(0.2, 5) Hz |
| MIMS_ALTERED_02_10 | MIMSunit, butter, cutoffs c(0.2, 10) Hz |
| Other MIMS options | Extrapolation TRUE, filtering TRUE, combination sum, truncation TRUE |
| Measurement span | Use each observation's dynamic_range_g; peak-to-peak g, not half-range. No assumed range. |
| AI | Absolute scale; sum of one-second values; supply the stored ai_noise_var in g squared. |
| Stationary AI noise estimate | Mean axis variance from one-second windows within the middle 80% of the unit's stationary recording; computed separately on native and calibrated signals. |

For raw-feature regeneration, pass the cutoffs, range, noise variance,
scale, aggregation and epoch explicitly to metronautica. Do not estimate AI
noise from the dynamic recording's first 30 seconds. Preserve the existing
signal-specific noise estimates and preprocessing. This document records the
study configuration; it does not add a raw-to-master workflow.


## Fixed acquisition and processing choices

- Static assessment: explicitly request duration_h = 24.
- Primary/secondary stepped shake: n_stages = 10L, stage_min = 6,
  buffer_min = 2, rpm_start = 50, rpm_step = 50, trim_start_min = 1,
  trim_end_min = 1; retain each recording's existing schedule/time offsets.
- Study band-pass Nyquist behaviour: pass svmbp_nyquist_action = "clip"
  to the summary function, or nyquist_action = "clip" to the direct helper.
  This specifies the cutoff adjustment used in the study masters.
  The generic package default stops when a cutoff exceeds Nyquist.
- Dynamic errors: retain the stored test/reference peak-to-peak axis spans,
  five-second windows, existing index-based pairing and existing resampling.
- Study comparison-plot limit, where required: comp_plot_ylim = c(0, 10).
  Plot limits and labels do not alter observations or statistical results.

These settings specify raw-feature/metrology generation for the study.
The current workflow continues to reproduce results from unchanged masters.


ActiGraph-count observations in the feature masters are the study inputs.
WAM analyses those stored observations; it does not recompute counts.
Metronautica provides the separate `accel_metric_agcounts()` function, which
is not used by this workflow.
