# Excitation and feature context ----
# Input: study conditions and summaries.
# Output: contextual comparison figures.

wam_frequency_tracking <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(ggplot2)
        library(gridExtra)
    })
    colours <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", LEAP = "#CC79A7", `SENS motion` = "#E69F00",
        `wGT3X-BT` = "#F0E442", `GT9X Link` = "#D55E00")
    panels <- lapply(c("primary_native", "secondary_native"), function(arm) {

        # Read figure input tables ----
        x <- read.csv(file.path(cfg$output, "features", arm, "unit_stage.csv")) |>
            filter(metric == "DOMFREQ_CLASS_5S") |>
            group_by(device, rpm, input_value) |>
            summarise(mean = mean(value), se = sd(value)/sqrt(n()), .groups = "drop")
        # Separate overlapping traces visually.
        # Preserve frequencies and confidence intervals.
        devices <- sort(unique(x$device))
        offsets <- setNames(seq(-0.06, 0.06, length.out = length(devices)), devices)
        x$display_frequency <- x$input_value + offsets[x$device]

        # Construct comparison panels ----
        ggplot(x, aes(display_frequency, mean, colour = device)) +
            geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = "grey55") +
            geom_errorbar(aes(ymin = mean - qt(0.975, 14) *
                se, ymax = mean + qt(0.975, 14) * se), width = 0.08) +
            geom_line() +
            geom_point() +
            scale_colour_manual(values = colours) +
            coord_equal(xlim = c(0.45, 8.75), ylim = c(0.45, 8.75)) +
            labs(title = if (arm == "primary_native")
                "A" else "B", x = "Programmed frequency (Hz)", y = "Dominant frequency (Hz)", colour = NULL) +
            theme_bw(base_size = 11) +
            theme(legend.position = "bottom")
    })
    out <- file.path(cfg$output, "frequency/figures")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    p <- gridExtra::arrangeGrob(grobs = panels, ncol = 2)

    # Save figure outputs ----
    ggsave(file.path(out, "native_frequency_tracking.png"), p, width = 12, height = 6, dpi = 300, device = ragg::agg_png,
        bg = "white")
    ggsave(file.path(out, "native_frequency_tracking.pdf"), p, width = 12, height = 6, device = grDevices::cairo_pdf,
        bg = "white")
}

wam_orientation_figure <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(ggplot2)
    })
    columns <- c("instrument", "device_id", "signal_pipeline", "metric", "was_computed", "mean_value",
        "rpm")
    observations <- bind_rows(readRDS(file.path(cfg$masters, "features/features_primary.rds")) |>
        select(all_of(columns)) |>
        mutate(device_id = as.character(device_id)), read.csv(file.path(cfg$masters, "features/gravity_orientation_stages.csv")) |>
        select(all_of(columns)) |>
        mutate(device_id = as.character(device_id)))
    x <- observations |>
        filter(signal_pipeline == "Native", grepl("ANGLE|ORIENT", metric), was_computed, is.finite(mean_value)) |>
        group_by(instrument, device_id, metric, rpm) |>
        summarise(value = mean(mean_value), .groups = "drop") |>
        mutate(device = ifelse(instrument == "SENS", "SENS motion", instrument)) |>
        group_by(device, metric, rpm) |>
        summarise(n = n(), mean = mean(value), se = sd(value)/sqrt(n()), lo = mean - qt(0.975, n - 1) *
            se, hi = mean + qt(0.975, n - 1) * se, .groups = "drop")
    if (!nrow(x))
        stop("No orientation features in the master.")
    stopifnot(setequal(unique(x$metric), c("ZANGLE", "ZANGLE_CHANGE", "GRAV_ZANGLE")), all(x$n == 15L),
        nrow(x) == 120L)
    x$metric <- factor(x$metric, levels = c("ZANGLE", "ZANGLE_CHANGE", "GRAV_ZANGLE"), labels = c("Full-vector Z-angle (degrees)",
        "Full-vector Z-angle change (degrees per sample change)", "Gravity-vector Z-angle (degrees)"))
    cols <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", LEAP = "#CC79A7", `SENS motion` = "#E69F00")
    p <- ggplot(x, aes(rpm, mean, colour = device)) +
        geom_errorbar(aes(ymin = lo, ymax = hi), width = 7) +
        geom_line() +
        geom_point(size = 1.8) +
        facet_wrap(~metric, scales = "free_y", ncol = 1) +
        scale_colour_manual(values = cols) +
        labs(x = "Orbital shaker speed (RPM)", y = "Metric value", colour = "Device") +
        theme_minimal(base_family = "Arial",
            base_size = 11) +
        theme(legend.position = "bottom")
    out <- file.path(cfg$output, "features/context")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    write.csv(x, file.path(out, "orientation_summary.csv"), row.names = FALSE)
    ggsave(file.path(out, "orientation_context.png"), p, width = 8.8, height = 10.2, dpi = 300, device = ragg::agg_png,
        bg = "white")
    ggsave(file.path(out, "orientation_context.pdf"), p, width = 8.8, height = 10.2, device = grDevices::cairo_pdf,
        bg = "white")
}
