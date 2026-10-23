# Verify threshold-transfer findings ----

source_path <- if (sys.nframe()) sys.frame(1)$ofile else NULL
script <- if (!is.null(source_path)) source_path else sub("^--file=", "",
    grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE))))
source(file.path(root, "code/config.R"))
cfg <- wam_config(root)
suppressPackageStartupMessages(library(dplyr))
out <- file.path(cfg$output, "threshold_transfer/data")
read_output <- function(name) read.csv(file.path(out, name), stringsAsFactors = FALSE)
cuts <- read_output("threshold_provenance.csv")
settings <- read.csv(file.path(root, "code/settings/threshold_cutoffs.csv"))
stopifnot(isTRUE(all.equal(cuts, settings, check.attributes = FALSE)),
    nrow(cuts) == 9L, !anyDuplicated(cuts[c("metric", "boundary")]),
    all(abs(cuts$cutoff_g - cuts$cutoff_mg/1000) < 1e-12))

# Check published cutoff provenance ----
svm <- read_output("fridolfsson_SVMbp_cutoffs.csv")
stopifnot(identical(svm$cutoff_mg, c(1L, 154L, 519L)),
    identical(svm$source_MET, c(1.5, 3, 6)),
    all(grepl("Table 3, wrist, laboratory calibration", svm$source, fixed = TRUE)),
    all(svm$source_url == "https://doi.org/10.3390/s21072333"),
    identical(cuts$cutoff_mg[cuts$metric == "ENMO"], c(44.8, 100.6, 428.8)),
    identical(cuts$cutoff_mg[cuts$metric == "MAD"], c(16.7, 91, 414)))
obsolete <- c("extrapolated_ENMO_to_SVMbp_cutoffs.csv", "ENMO_SVMbp_paired_classifications.csv",
    "ENMO_SVMbp_classification_agreement.csv", "ENMO_SVMbp_cohort_agreement.csv",
    "ENMO_SVMbp_paired_unit_minutes.csv")
stopifnot(!any(file.exists(file.path(out, obsolete))))

# Independently average master observations ----
inputs <- list(primary_native = c("features_primary", "Primary comparison"),
    secondary_native = c("features_secondary_native", "Secondary comparison"))
expected <- bind_rows(lapply(names(inputs), function(arm) {
    spec <- inputs[[arm]]
    devices <- if (arm == "primary_native") c("AX3", "GENEActiv", "LEAP", "SENS") else
        c("LEAP", "wGT3X-BT", "GT9X Link")
    trials <- if (arm == "primary_native") 3L else 2L
    z <- readRDS(file.path(cfg$masters, "features", paste0(spec[1], ".rds"))) |>
        filter(instrument %in% devices, signal_pipeline == "Native",
            metric %in% cuts$metric, was_computed, is.finite(mean_value),
            rpm %in% seq(50, 350, 50)) |>
        group_by(instrument, device_id, metric, rpm) |>
        summarise(value = mean(mean_value), n_trials = n_distinct(trial_start_utc),
            .groups = "drop") |>
        mutate(cohort = spec[2], unit_id = paste(instrument, device_id, sep = "_"),
            device = ifelse(instrument == "SENS", "SENS motion", instrument))
    stopifnot(all(z$n_trials == trials))
    z
}))
observed <- read_output("unit_stage_features.csv")
keys <- c("metric", "cohort", "device", "unit_id", "rpm")
paired <- inner_join(expected, observed, by = keys, suffix = c("_expected", "_observed"))
stopifnot(nrow(expected) == 2205L, nrow(observed) == 2205L,
    !anyDuplicated(observed[keys]), nrow(paired) == 2205L,
    max(abs(paired$value_expected - paired$value_observed)) < 1e-10)

# Reconstruct per-unit threshold durations ----
expected_minutes <- bind_rows(lapply(seq_len(nrow(cuts)), function(k) {
    expected |>
        filter(metric == cuts$metric[k]) |>
        group_by(metric, cohort, device, unit_id) |>
        summarise(stages_above = sum(value >= cuts$cutoff_mg[k]), .groups = "drop") |>
        mutate(cutoff_mg = cuts$cutoff_mg[k], minutes_above = stages_above * 60/7)
}))
minutes <- read_output("unit_minutes_all_cutoffs.csv")
paired <- inner_join(expected_minutes, minutes,
    by = c("metric", "cohort", "device", "unit_id", "cutoff_mg"),
    suffix = c("_expected", "_observed"))
stopifnot(nrow(minutes) == 945L, nrow(paired) == 945L,
    max(abs(paired$minutes_above_expected - paired$minutes_above_observed)) < 1e-10)
for (k in seq_len(nrow(svm))) {
    z <- expected_minutes[expected_minutes$metric == "SVMBP_069_10" &
        expected_minutes$cutoff_mg == svm$cutoff_mg[k], ]
    stopifnot(nrow(z) == 105L, all(z$stages_above == c(7L, 5L, 3L)[k]))
}
summary <- read_output("minutes_summary.csv")
reconstructed <- minutes |>
    group_by(metric, cohort, device, boundary, cutoff_mg) |>
    summarise(mean = mean(minutes_above), minimum = min(minutes_above),
        maximum = max(minutes_above), .groups = "drop")
stopifnot(nrow(summary) == 63L,
    isTRUE(all.equal(as.data.frame(reconstructed), summary, tolerance = 1e-12)))

# Check registered figure exports ----
registry <- read.csv(file.path(root, "code/settings/figures.csv"))
registry <- registry[registry$number %in% c("8", "S12"), ]
for (i in seq_len(nrow(registry))) {
    input <- file.path(cfg$output, registry$source[i])
    output <- file.path(cfg$results, "figures", paste0("figure-", registry$number[i], ".png"))
    stopifnot(file.exists(input), file.exists(output),
        identical(unname(tools::md5sum(input)), unname(tools::md5sum(output))))
}
cat("PASS: published thresholds, master-derived features, per-unit durations and figure exports.\n")
