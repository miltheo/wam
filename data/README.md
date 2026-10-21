# Study master observations

`masters/` contains the selected static, dynamic, feature and diagnostic inputs.
Their filenames are unversioned; `config/inputs.csv` records byte counts and
SHA-256 checksums. Relocating these files does not require changing the analyses:
set `WAM_MASTER_ROOT` to their new directory.

The masters include one row per relevant unit, trial, stage, feature or diagnostic
observation. Native recordings, fitted statistical models, publication images and
duplicated per-device exports are not included.

The static/dynamic metrology and feature masters retain the selected study
values. Timing sensitivity uses already-calculated trial/stage outcomes for
alternative pairing/alignment arms; WAM refits their statistical comparisons.
Allan curves are stored outcome observations from the existing unit calculations;
they are not reconstructed from the minimum values in the static summary.
The gravity-vector orientation master retains the existing per-unit, trial and
stage observations used for the supplementary mounting-stability diagnostic.

WAM distributes separate single-unit native examples as release attachments
and a primary HBK reference for the companion Metronautica package. Those examples demonstrate the package and do not
replace these full-cohort master inputs.

Data redistribution terms are separate from the GPL-3 software licence.
No additional data licence is assigned by this repository.

## Optional native examples

`samples/native/` contains the catalogue and provenance for single-unit native
examples distributed as release attachments, including the paired HBK reference. They are independent of `masters/` and are
not used by the WAM manuscript-reproduction workflow. See
[sample distribution](samples/README.md).
