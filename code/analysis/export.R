# Export selected article assets ----
# Input: completed analysis outputs.
# Output: numbered figures and tables.

wam_condition_mapping <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(tidyr)
    })
    x <- read.csv(file.path(cfg$output, "features/primary_native/unit_stage.csv")) |>
        filter(metric %in% c("ENMO", "MAD")) |>
        group_by(rpm, metric) |>
        summarise(observed_median_mg = median(value), .groups = "drop") |>
        pivot_wider(names_from = metric, values_from = observed_median_mg)
    design <- read.csv(file.path(cfg$root, "code/settings/condition_mapping.csv"))
    write.csv(left_join(design, x, by = "rpm"), file.path(cfg$output, "features/condition_mapping.csv"),
        row.names = FALSE)
}

wam_export <- function(cfg) {
    # Keep intermediates under analysis/.
    # Write final assets under results/.
    feature_out <- file.path(cfg$output, "features/tables")
    dir.create(feature_out, recursive = TRUE, showWarnings = FALSE)
    wam_feature_tables(file.path(cfg$output, "features"), feature_out)
    wam_device_clips(file.path(cfg$root, "3d-clips"), file.path(cfg$output, "hardware/device_clips.png"))
    # Summarise reference-timing sensitivity evidence.
    pairing <- read.csv(file.path(cfg$output, "timing_sensitivity/pairing/device_comparisons.csv"))
    events <- read.csv(file.path(cfg$output, "timing_sensitivity/event/comparison_changes.csv"))
    events <- events[events$baseline == "index_stage", ]
    row <- function(label, changed, direction) data.frame(assessment = label, comparisons = length(changed),
        decisions_retained = sum(!changed), directions_retained = sum(!direction))
    summary <- row("Pooled versus within-stage reference pairing", pairing$significance_changed, pairing$direction_changed)
    for (metric in unique(events$metric)) {
        d <- events[events$metric == metric, ]
        summary <- rbind(summary, row(paste("Apparent-event alignment:", metric), d$significance_changed,
            d$direction_changed))
    }
    write.csv(summary, file.path(cfg$output, "timing_sensitivity/summary.csv"), row.names = FALSE)
    wam_export_assets(cfg)
}
