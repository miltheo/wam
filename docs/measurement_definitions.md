# Study measurement definitions

The manuscript Methods and feature tables are the authority for this study.
This guide identifies the corresponding analysis quantities; it does not replace
those definitions or impose universal package settings.

## Feature processing

The study uses non-overlapping five-second epochs within trimmed steady-state
stages. Native and within-trial dynamic VM-calibrated signals are distinct
feature-processing pipelines. The original tables define formulas, units,
processing and references. The installed Metronautica reference manual (`help("measurement-definitions",
package = "metronautica")`) documents the shared arithmetic and generic alternatives.

ROCAM is the epoch mean Euclidean sample-to-sample triaxial acceleration change,
with a one-second running median applied to the difference series. Difference
calculation precedes median smoothing. No one-second resampling or time derivative
is calculated.

The study dominant-frequency feature uses five-second, demeaned, Hann-tapered
triaxial epochs, pooled axis spectral power and a search band from 0.3 Hz to
min(15 Hz, 0.45 times sampling frequency). The stage mode breaks ties by cumulative
peak power, then lower frequency. Dominant peak magnitude uses the same epoch
peak bin with Hann coherent-gain normalisation and one-sided amplitude conversion
to mg, then averages valid epoch magnitudes within stage. These are not the
engine's whole-stage VM FFT diagnostics or the separate filtered full-stage
shaker-stability estimate. Alternative published estimators retain their channel,
window, taper/filter, search-band and stage-aggregation specifications.

## Metrology and inference

| Quantity | Operational definition |
|---|---|
| Absolute VM bias | Absolute stage-mean test-minus-reference VM difference; not mean absolute sample-level difference |
| Scale-factor error | 100 times (test-on-reference response slope minus one), percent |
| VM integral non-linearity | Maximum absolute stage-specific fitted residual divided by the study VM normalising span, times 100 |
| Trial-level composite error | Maximum absolute pooled trial-fit residual divided by half the study VM normalising span, times 100 |
| Trial-to-trial VM repeatability CV | 100 SD / mean of stage-mean VM across repeated trials within unit, stage and pipeline |
| Feature response slope | Unit-specific feature-on-controlled-input slope across all ten stages |
| Feature response integral non-linearity | 100 maximum absolute feature-fit residual / fitted response span |
| Robust observed dynamic span | Median across device models of absolute 500-minus-50-RPM differences in unit-level, trial-averaged mean feature values |
| Practical equivalence margin | Plus/minus 10% of the feature- and comparison-specific robust observed full-range dynamic span; unchanged within regions |
| Full-range equivalence | Every required device-pair-by-stage 90% mean-difference confidence interval lies wholly within the margin |
| Regional equivalence | Every required pair-stage comparison passes within 50-200, 250-350 or 400-500 RPM, retaining the full-range margin |
| Adequate controlled response | In every model, the 95% physical-unit bootstrap interval for median slope is above zero and the upper 95% limit for median response integral non-linearity is no greater than 10% |
| Combined criterion | Both full-range equivalence (E) and adequate controlled response (R) are met; regional passing cannot override a full-range failure; R-squared is descriptive |
| Operational deadband endpoint | First tested excitation with at least 10% expected coherent response, sustained at every higher stage |
| Activation threshold | Analogous first sustained crossing of 50% expected coherent response; lowest-stage crossings are left-censored |

Response fits predict test VM from reference VM; within-trial dynamic VM
calibration predicts reference VM from test VM. Do not interchange their slopes.
The primary comparison uses three repeated trials and the secondary comparison
uses two, averaged within each physical unit for feature inference. There are
15 units per model. The comparisons remain separate; they do not test all pairs
among all six models. Bootstrap resampling is at physical-unit level.

## Interpretation

Use measurement error, precision, repeatability and measurement uncertainty for
their distinct metrological concepts. The reported components and statistical
intervals are not a complete downstream uncertainty budget. Percentage-difference
screening is not inferential equivalence; agreement is not behavioural validity.
The exploratory drift projection and threshold-transfer scenarios do not estimate
observed error in human behavioural durations. Refer to extrapolated cutoffs and
durations with their assumptions, and to time spent in behaviours when that is
the output. Device model and physical unit are distinct replication levels.
