# Summaries of diagnostic observations already calculated from the study signals.
# These are not new clock corrections or alternative metrology outcomes.
wam_diagnostics <- function(cfg) {
    suppressPackageStartupMessages(library(dplyr))
    out <- file.path(cfg$output, "diagnostics")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    records <- read.csv(file.path(cfg$masters, "signal_diagnostics.csv"))
    records$instrument[records$instrument == "SENS"] <- "SENS motion"
    stopifnot(nrow(records) == 2790L, sum(records$family == "static") == 90L)
    write.csv(records, file.path(out, "record_diagnostics.csv"), row.names = FALSE)
    stages <- read.csv(file.path(cfg$masters, "timing_stages.csv"))
    pooled <- read.csv(file.path(cfg$masters, "timing_pooled.csv"))
    unit <- stages |> group_by(device, id) |> summarise(stages = n(),
        estimated = sum(is.finite(lag_s)), lag_median = median(lag_s, na.rm = TRUE),
        stage_p95_median = median(pairing_p95_ms), .groups = "drop")
    summary <- stages |> group_by(device) |> summarise(units = n_distinct(id), stages = n(),
        estimated = sum(is.finite(lag_s)), stage_p95_median_ms = median(pairing_p95_ms),
        onset_median_s = median(lag_s, na.rm = TRUE), onset_min_s = min(lag_s, na.rm = TRUE),
        onset_max_s = max(lag_s, na.rm = TRUE), .groups = "drop") |>
        left_join(pooled |> group_by(device) |> summarise(pooled_p95_median_ms = median(pairing_p95_ms),
                                                        .groups = "drop"), by = "device") |>
        left_join(unit |> group_by(device) |> summarise(unit_median_lag_min_s = min(lag_median, na.rm = TRUE),
            unit_median_lag_max_s = max(lag_median, na.rm = TRUE), .groups = "drop"), by = "device")
    write.csv(unit, file.path(out, "timing_units.csv"), row.names = FALSE)
    write.csv(summary, file.path(out, "timing_summary.csv"), row.names = FALSE)
    status <- system2(cfg$python, c(shQuote(file.path(cfg$root,"tools/diagnostic_tables.py")),
        shQuote(file.path(cfg$masters,"signal_diagnostics.csv")), shQuote(out)))
    if(status != 0L) stop("Signal diagnostic table aggregation failed.")
    axes <- read.csv(file.path(cfg$masters,"static_axis_diagnostics.csv"))
    curves <- read.csv(file.path(cfg$masters,"static_allan_curves.csv")) |>
        filter(device == "SENS motion") |> group_by(id,axis) |>
        summarise(allan_min=min(adev), boundary=which.min(adev)%in%c(1L,n()),.groups="drop")
    axes <- axes |> left_join(curves,by=c("id","axis")) |> group_by(axis) |>
        summarise(units=n(), constant_axis=sum(n_unique==1), one_or_two_values=sum(n_unique<=2),
                  zero_allan_minimum=sum(allan_min==0), boundary_minimum=sum(boundary),.groups="drop")
    write.csv(axes,file.path(out,"static_axis_summary.csv"),row.names=FALSE)
}

wam_allan_figures <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(gridExtra)})
    curves <- read.csv(file.path(cfg$masters, "static_allan_curves.csv"))
    models <- c("LEAP", "AX3", "GENEActiv", "SENS motion")
    plots <- lapply(seq_along(models), function(i) {
        d <- curves[curves$device == models[i], ]
        id <- sort(unique(d$id))[1]
        d <- d[d$id == id, c("tau", "adev", "axis")]
        # Use the public package plotter directly; the input is an Allan outcome
        # table, not another copy of the metrology calculation implementation.
        metronautica::plot_static_allan(d, title_prefix = paste0(LETTERS[i], ". ", models[i]))
    })
    out <- file.path(cfg$output, "diagnostics/figures")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    p <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
    ggplot2::ggsave(file.path(out, "allan_representative.png"), p, width = 11, height = 8,
                   dpi = 300, device = ragg::agg_png, bg = "white")
    ggplot2::ggsave(file.path(out, "allan_representative.pdf"), p, width = 11, height = 8,
                   device = grDevices::cairo_pdf, bg = "white")
}

wam_spectral_figure <- function(cfg) {
    out <- file.path(cfg$output, "diagnostics")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    csv <- file.path(out, "spectral_observations.csv")
    file.copy(file.path(cfg$masters, "spectral_audit.csv"), csv, overwrite = TRUE)
    status <- system2(cfg$python, c(shQuote(file.path(cfg$root, "tools/spectral_figure.py")),
        shQuote(csv), shQuote(file.path(out, "figures/spectral_audit.png"))))
    if (status != 0L) stop("Spectral figure rendering failed; check Python dependencies.")
}
