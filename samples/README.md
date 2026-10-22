# Native recording samples

Optional recordings for [Metronautica](https://github.com/miltheo/metronautica)
examples. WAM analyses use the [master observations](../data/README.md) and do not
require these files.

Nine gzip attachments total 63.4 MB compressed, approximately 250.2 MB restored:
static and dynamic recordings for LEAP, AX3, SENS motion and GENEActiv, plus one
primary HBK reference. Static files cover the analysed 24 hours; dynamic files
retain all ten stages and intervening adjustments of the first primary trial.

[Catalogue and checksums](sample_index.csv) · [Record provenance](provenance.json) ·
[Release metadata](release.json).

## Obtain the recordings

Recording payloads are separate release attachments, excluded from Git history;
cloning this repository does not obtain them. Place the required `.gz` files in
`samples/` beside `sample_index.csv`.

For a dynamic example, obtain **both the wearable recording and
`dynamic_reference_primary.csv.gz`**: the loader includes the reference by default.
For example, LEAP requires `dynamic_leap.agsd.gz` plus that reference attachment.
Use `include_reference = FALSE` when only the wearable file is required.
Static retrieval requires only the selected static attachment.

No release URL is configured by default. For remote retrieval, supply the actual
published release-attachment directory as `base_url` or
`options(metronautica.sample_base_url = ...)`. The loader downloads only when
explicitly called; installation and ordinary package use require no WAM access.

## Load a sample

Run in R from the WAM root after obtaining the attachments:

```r
library(metronautica)
options(metronautica.sample_dir = "samples")
path <- m_load_sample_data(device = "LEAP", type = "dynamic")
info <- attr(path, "sample_info")
accel <- parse_raw_auto(path, return = "bundle", qc = TRUE,
    time_shift_s = info$time_shift_s,
    full_scale_range_g = info$full_scale_range_g)
accel_meta(accel)$processing$qc
```

Restored files and provenance sidecars go into `metronautica-samples/` by default,
which is excluded from Git. Compressed and restored SHA-256 digests are verified.
The reference path is `attr(path, "reference_file")`; its metadata is
`attr(path, "reference_info")`. Apply each file's own documented timestamp shift.

For complete static/dynamic processing and plots, use Metronautica's
[Getting Started guide](https://github.com/miltheo/metronautica/blob/main/docs/GETTING_STARTED.md)
and `help("m_load_sample_data", package = "metronautica")`.

## Verify and distribute

In a terminal from the WAM root:

```sh
Rscript code/checks/check_samples.R --require-all
```

Without `--require-all`, the check verifies available files and reports missing
attachments. When publishing the recordings, include the catalogue, provenance,
release metadata and repository licence with the attachments.

Native recordings and accompanying metadata are distributed under **GPL-3.0-only**,
the same licence as the WAM code; see [LICENSE](../LICENSE).

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
