# Native recording samples

Optional recordings for [Metronautica](https://github.com/miltheo/metronautica)
examples. WAM analyses use the [master observations](../data/README.md) and do not
require these files.

Nine gzip recordings total 63.4 MB compressed, approximately 250.2 MB restored:
static and dynamic recordings for LEAP, AX3, SENS motion and GENEActiv, plus one
primary HBK reference. Static files cover the analysed 24 hours; dynamic files
retain all ten stages and intervening adjustments of the first primary trial.

[Catalogue and checksums](sample_index.csv) &middot; [Record provenance](provenance.json) &middot;
[Release metadata](release.json).

## Obtain the recordings

Clone or download WAM. The nine `.gz` recordings and `sample_index.csv` are
included in `samples/`; no separate release download is required.

For a dynamic example, obtain **both the wearable recording and
`dynamic_reference_primary.csv.gz`**: the loader includes the reference by default.
For example, LEAP uses `dynamic_leap.agsd.gz` plus that reference recording.
Use `include_reference = FALSE` when only the wearable file is required.
Static retrieval requires only the selected static recording.

## Use with Metronautica

Follow [Getting Started](https://github.com/miltheo/metronautica/blob/main/docs/GETTING_STARTED.md#real-recording-examples)
for `m_load_sample_data()` configuration, parsing, metrology and plots.
[Loader parameters](https://github.com/miltheo/metronautica/blob/main/man/m_load_sample_data.Rd)
cover local/remote retrieval and checksum verification. The catalogue supplies
each recording's timestamp shift, measurement span and analysis bounds.

## Verify the recordings

In a terminal from the WAM root:

```sh
Rscript code/checks/check_samples.R --require-all
```

Without `--require-all`, the check verifies available files and reports missing
recordings. Keep the recordings, catalogue and provenance together.

Native recordings and accompanying metadata are distributed under **GPL-3.0-only**,
the same licence as the WAM code; see [LICENSE](../LICENSE).

Maintainer: [Millen James Theophilus](https://github.com/miltheo).
