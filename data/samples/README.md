# Native examples for Metronautica

The `sample-data-v1.0.0` data release contains single-unit examples for LEAP,
AX3, SENS motion and GENEActiv: one 24-hour static recording and one continuous
first-primary-trial dynamic recording per device. Dynamic examples contain all
ten stages and intervening adjustments. A matching primary HBK reference is
included. The nine gzip files total 63.4 MB; restored recordings total approximately
250.2 MB. WAM's master-to-assets analyses do not use these files.

## Access

Compressed recordings are distributed as individual release attachments, outside
Git history. `native/sample_index.csv` identifies their filenames, settings,
analysis windows, timestamp shifts and compressed/restored SHA-256 checksums.
`native/provenance.json` documents record preservation and container bookkeeping.
The sample-data version is independent of WAM software version 0.1.0.

Download the selected `.gz` file and, if required, the reference attachment into
`data/samples/native/`. For local use:

```r
library(metronautica)
options(metronautica.sample_dir = "data/samples/native")
path <- m_load_sample_data(device = "LEAP", type = "dynamic")
info <- attr(path, "sample_info")
accel <- parse_raw_auto(path, return = "bundle",
    time_shift_s = info$time_shift_s,
    full_scale_range_g = info$full_scale_range_g)
```

`attr(path, "reference_file")` and `reference_info` provide the reference path
and its separate timestamp shift. For explicit downloads, set
`metronautica.sample_base_url` or the loader's `base_url` to the fixed HTTPS
release-attachment directory. The repository-specific URL is not configured in
this checkout; `release.json` records the data tag and catalogue locations.
The loader validates recordings against its packaged catalogue. Parsing and
calculations have no WAM installation or network dependency.

Run `python tests/check_samples.py` to verify locally available payloads.
Use `python tests/check_samples.py --require-all` to require all nine files.

## Distribution terms

Sample recordings have separate data-distribution terms. The GPL-3 software
licence does not assign a data licence or establish redistribution permission.
