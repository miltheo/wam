# Native recording samples

Nine gzip attachments (63.4 MB compressed;
approximately 250.2 MB restored). Payloads are excluded from Git history.

- LEAP, AX3, SENS motion and GENEActiv: one 24-hour static recording and one
  continuous first-primary-trial dynamic recording per device.
- Dynamic recordings include all ten stages and intervening adjustments.
- One matching primary HBK reference recording.

File settings and checksums: [sample_index.csv](sample_index.csv).
Record preservation and container edits: [provenance.json](provenance.json).
Distribution metadata: [release.json](release.json).

## Load a sample

Place the selected `.gz` attachment in `samples/`. From the WAM root:

```r
library(metronautica)
options(metronautica.sample_dir = "samples")
path <- m_load_sample_data(device = "LEAP", type = "dynamic")
info <- attr(path, "sample_info")
accel <- parse_raw_auto(path, return = "bundle",
    time_shift_s = info$time_shift_s,
    full_scale_range_g = info$full_scale_range_g)
```

For downloads, supply the fixed release-attachment directory through `base_url`
or `metronautica.sample_base_url`. Reference access: `attr(path, "reference_file")`
and `attr(path, "reference_info")`. See `help("m_load_sample_data", package = "metronautica")`.

```sh
Rscript code/checks/check_samples.R --require-all
```

Samples demonstrate Metronautica; WAM analyses use the [masters](../data/README.md).
Data terms: [data terms](../data/README.md).

Maintainer: [Millen J. Theophilus](https://github.com/miltheo).
