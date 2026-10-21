# WAM

Wearable Accelerometers Metrology (WAM).
Statistical analyses and master observations supporting the associated research
article under submission. The workflow reproduces numerical results and
code-generated figures and tables from the supplied masters.

## Reproduction

Requires R >= 4.1, **metronautica** and the dependencies in the
[installation instructions](code/README.md#installation). Run from the repository root:

```sh
Rscript code/run.R --check
Rscript code/run.R all
Rscript code/checks/check_results.R
```

Individual stages and verification commands: [code](code/README.md).

## Repository structure

| Directory | Contents |
|---|---|
| [code](code/README.md) | Stages, analyses, figures, utilities, checks and settings |
| [data](data/README.md) | Static, dynamic, feature and diagnostic masters |
| [samples](samples/README.md) | Native-example catalogue and provenance; payloads distributed as release attachments |
| [3d-clips](3d-clips/README.md) | Mounting models and dimensions; thumbnails in `previews/` |

Generated `analysis/` and `results/` directories are local outputs, excluded from Git.

## Methods and documentation

Measurement definitions and study specifications: the associated research article
under submission. Package documentation: Metronautica repository (link pending).

## Citation

Cite WAM using [CITATION.cff](CITATION.cff), and the associated
research article under submission when referring to the study methods or findings.

## Maintainer

[Millen J. Theophilus](https://github.com/miltheo).

## Licence

Software: **GPL-3.0-only**; see [LICENSE](LICENSE).
Data-distribution terms are separate; see [data](data/README.md).
