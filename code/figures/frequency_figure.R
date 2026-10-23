# Frequency-estimator tracking figure ----
# Input: frequency analysis summaries.
# Output: estimator tracking panels.

wam_frequency_figure <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(gridExtra); library(ragg)})
    freq <- file.path(cfg$output,"frequency/tables")
    out <- file.path(cfg$output,"frequency/figures")
    dir.create(out,recursive=TRUE,showWarnings=FALSE)
    audit <- file.path(cfg$output,"frequency/figure_data")
    dir.create(audit, recursive = TRUE, showWarnings = FALSE)

    # Read figure input tables ----
    defs <- read.csv(file.path(freq, "method_definitions.csv"))
    labels <- c("Study: 5-s pooled X/Y/Z, Hann", "4-min pooled X/Y/Z, BP 0.2-10 Hz",
      "Preece: 2-s X", "Preece: 2-s Y", "Preece: 2-s Z",
      "Zhang: 12.8-s VM f625, 0.6-2.5 Hz",
      "Mannini: 12.8-s VM, 15-Hz low-pass", "Ellis: 60-s VM, unrestricted",
      "Ellis: 60-s VM, 0.3-3 Hz", "Staudenmayer: 15-s VM",
      "Ahmadi: 15-s VM, 0.25-5 Hz", "Coyle-Asbil: 30-s filtered X",
      "Coyle-Asbil: 30-s filtered Y", "Coyle-Asbil: 30-s filtered Z",
      "Jeong: 5-s Hamming X", "Jeong: 5-s Hamming Y", "Jeong: 5-s Hamming Z",
      "4-min VM, assigned half-spectrum grid", "4-min VM, standard DFT grid")
    names(labels) <- defs$method
    stopifnot(length(labels) == 19L, !anyNA(labels))
    excluded_methods <- c("STUDY_4MIN_XYZ_BP0210", "METRONAUTICA_4MIN_VM_EXACT",
      "METRONAUTICA_4MIN_VM_CORRECTGRID")
    included_methods <- setdiff(defs$method, excluded_methods)
    stopifnot(length(included_methods) == 16L)
    summary <- read.csv(file.path(freq, "method_comparison_summary.csv")) |>
      filter(signal_pipeline == "Native", method %in% included_methods) |> arrange(mean_abs_error_hz, method) |>
      mutate(rank = row_number(), publication_label = unname(labels[method]))
    stopifnot(nrow(summary) == 16L, !anyNA(summary$publication_label))
    method_order <- rev(summary$method)
    study_method <- "STUDY_5S_XYZ_HANN"
    study_y <- match(study_method, method_order)
    factor_method <- function(d) mutate(d, method = factor(method, levels = method_order))
    label_method <- function(x) as.expression(lapply(x, function(method) {
      value <- unname(labels[method])
      if (method == study_method) bquote(bold(.(value))) else value
    }))
    device_colours <- c("AX3" = "#56B4E9", "GENEActiv" = "#009E73",
      "LEAP" = "#CC79A7", "SENS motion" = "#E69F00")
    device_shapes <- c("AX3" = 16, "GENEActiv" = 17, "LEAP" = 15, "SENS motion" = 18)
    margin_levels <- c("+/-5%", "+/-10%", "+/-15%")
    margin_colours <- c("+/-5%" = "#0072B2", "+/-10%" = "#E69F00", "+/-15%" = "#009E73")
    pub <- theme_minimal(base_family = "Arial", base_size = 11.75) +
        theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 10.5),
            plot.title = element_text(face = "bold", size = 12.25),
            plot.subtitle = element_text(size = 10), legend.position = "bottom",
            legend.text = element_text(size = 10), legend.title = element_text(size = 10, face = "bold"),
            plot.margin = margin(10, 8, 8, 8))
    highlight <- function(xmin = -Inf, xmax = Inf) annotate("rect", xmin = xmin, xmax = xmax,
      ymin = study_y - .48, ymax = study_y + .48, fill = "#FFF1BD", alpha = .65)
    y_scale <- function() scale_y_discrete(limits = method_order, labels = label_method,
      drop = FALSE, expand = expansion(add = .6))

    by_device <- read.csv(file.path(freq, "method_by_device_performance.csv")) |>
      filter(signal_pipeline == "Native", method %in% included_methods) |>
      mutate(instrument = factor(ifelse(instrument == "SENS", "SENS motion", instrument),
        levels = names(device_colours))) |> factor_method()
    stopifnot(nrow(by_device) == 64L, all(by_device$n_stage_trials == 450L))

    # Construct comparison panels ----
    p_a <- ggplot(by_device, aes(mean_abs_error_hz, method)) + highlight(.005, 25) +
        geom_vline(xintercept = .25, linetype = "dashed", colour = "grey55", linewidth = .4) +
        geom_point(aes(colour = instrument, shape = instrument),
            position = position_dodge(width = .45, orientation = "y"), size = 2.2) +
        scale_x_log10(limits = c(.005, 25), breaks = c(.01, .1, 1, 10),
            labels = c("0.01", "0.1", "1", "10")) + y_scale() +
        scale_colour_manual(values = device_colours) +
        scale_shape_manual(values = device_shapes) +
        labs(title = "A. Frequency error by device", subtitle = "450 unit-trial-stage values per device",
            x = "Mean absolute error (Hz; log scale)", y = NULL, colour = "Device", shape = "Device") +
        guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE)) + pub

    by_rpm <- read.csv(file.path(freq, "method_by_device_rpm_summary.csv")) |>
      filter(signal_pipeline == "Native", method %in% included_methods) |>
      group_by(method, rpm) |>
      summarise(mean_abs_error_hz = weighted.mean(mean_abs_error_hz, n_stage_trials),
        n = sum(n_stage_trials), .groups = "drop") |> factor_method() |>
      mutate(rpm_bin = factor(rpm, levels = seq(50, 500, 50)),
        band_limit = case_when(as.character(method) == "ZHANG_12_8S_VM_F625" ~ 2.5,
          as.character(method) == "ELLIS_60S_VM_BAND03_3" ~ 3,
          as.character(method) == "AHMADI_15S_VM_BAND025_5" ~ 5, TRUE ~ Inf),
        outside_band = rpm / 60 > band_limit,
        value_label = ifelse(mean_abs_error_hz < 1, sprintf("%.2f", mean_abs_error_hz),
          sprintf("%.1f", mean_abs_error_hz)))
    check <- by_rpm |> group_by(method) |> summarise(mae = mean(mean_abs_error_hz), .groups = "drop") |>
      mutate(method = as.character(method)) |> left_join(summary, by = "method")
    stopifnot(nrow(by_rpm) == 160L, all(by_rpm$n == 180L),
      max(abs(check$mae - check$mean_abs_error_hz)) < 1e-10)
# Mark out-of-band fundamentals with crosses.
# Distinguish restrictions from missing values.
    p_b <- ggplot(by_rpm, aes(rpm_bin, method, fill = mean_abs_error_hz)) +
        geom_tile(colour = "white", linewidth = .35) +
        geom_rect(data = data.frame(y = study_y), aes(xmin = .5, xmax = 10.5,
            ymin = y - .48, ymax = y + .48), inherit.aes = FALSE,
            fill = NA, colour = "#C49413", linewidth = .8) +
        geom_text(aes(label = ifelse(outside_band, "\u00d7", value_label),
            colour = mean_abs_error_hz > 2), size = 2.9, family = "Arial", show.legend = FALSE) +
        scale_colour_manual(values = c("FALSE" = "#253746", "TRUE" = "white"), guide = "none") +
        scale_fill_gradientn(colours = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
            trans = scales::pseudo_log_trans(sigma = .05), limits = c(0, max(by_rpm$mean_abs_error_hz)),
            breaks = c(0, .1, 1, 10), labels = c("0", "0.1", "1", "10")) +
        scale_x_discrete(labels = sprintf("%d\n%.2f", seq(50, 500, 50), seq(50, 500, 50) / 60)) +
        y_scale() +
        labs(title = "B. Frequency error across RPM", subtitle = "180 unit-trial-stage values per condition",
            x = "Condition: RPM / programmed Hz", y = NULL, fill = "Mean absolute error (Hz)") + pub +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 9.5, lineheight = .95)) +
        guides(fill = guide_colourbar(direction = "horizontal", barwidth = grid::unit(2.5, "in"),
            barheight = grid::unit(.10, "in"), title.position = "top"))

    equivalence <- read.csv(file.path(freq, "equivalence_summary.csv")) |>
      filter(signal_pipeline == "Native", method %in% included_methods, margin_pct %in% c(5, 10, 15)) |>
      mutate(margin = factor(paste0("+/-", margin_pct, "%"), levels = margin_levels),
        pct_passing = pairs_passing / pairs_total * 100) |> factor_method()
    stopifnot(nrow(equivalence) == 48L, all(equivalence$pairs_total == 60L))
    p_c <- ggplot(equivalence, aes(pct_passing, method)) + highlight() +
        geom_vline(xintercept = 100, linetype = "dashed", colour = "grey55", linewidth = .4) +
        # Outline coincident markers clearly.
        geom_point(aes(colour = margin, shape = margin, size = margin), stroke = .7) +
        scale_colour_manual(values = margin_colours, labels = c("\u00b15%", "\u00b110%", "\u00b115%")) +
        scale_shape_manual(values = c("+/-5%" = 16, "+/-10%" = 2, "+/-15%" = 0),
            labels = c("\u00b15%", "\u00b110%", "\u00b115%")) +
        scale_size_manual(values = c("+/-5%" = 1.7, "+/-10%" = 3.4, "+/-15%" = 4.6), guide = "none") +
        scale_x_continuous(limits = c(-3, 104), breaks = c(0, 25, 50, 75, 100)) + y_scale() +
        labs(title = "C. Inter-device equivalence", subtitle = "Six device pairs \u00d7 ten conditions",
            x = "Pair-by-stage comparisons passing (%)", y = NULL, colour = "Tolerance", shape = "Tolerance") + pub +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        guides(colour = guide_legend(nrow = 1, override.aes = list(size = c(1.7, 3.4, 4.6))),
            shape = guide_legend(nrow = 1))

# Use consistent raster text metrics.
# Align all three panel rows.
    metrics <- file.path(audit, "text_metrics.png")
    ragg::agg_png(metrics, width = 15.5, height = 11.7, units = "in", res = 100)
    grobs <- lapply(list(p_a, p_b, p_c), ggplotGrob)
    stopifnot(length(unique(vapply(grobs, function(g) length(g$heights), integer(1)))) == 1L)
    common_heights <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$heights))
    grobs <- lapply(grobs, function(g) { g$heights <- common_heights; g })
    panels <- arrangeGrob(grobs = grobs, ncol = 3, widths = c(6.5, 5, 4))
    header <- grid::textGrob("Dominant-frequency estimator comparison",
      x = .01, hjust = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 17, fontface = "bold"))
    subhead <- grid::textGrob("Native primary recordings; rows ordered by overall mean absolute error, lowest to highest",
      x = .01, hjust = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 11.75))
    foot <- grid::textGrob(paste0("Highlighted row: study's five-second pooled-axis estimator. VM: time-domain vector magnitude.\n",
      "\u00d7: programmed frequency outside the estimator's search band. Dashed lines: 0.25 Hz (A) and 100% passing (C)."),
      x = .01, hjust = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 10.5, lineheight = 1.2))
    figure <- arrangeGrob(header, subhead, panels, foot, ncol = 1, heights = c(.40, .31, 10.24, .75))
    name <- "frequency_estimators"

    # Save figure outputs ----
    ggsave(file.path(out, paste0(name, "_600dpi.png")), figure, width = 15.5, height = 11.7,
      units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
    ggsave(file.path(out, paste0(name, ".pdf")), figure, width = 15.5, height = 11.7,
      units = "in", device = grDevices::cairo_pdf, bg = "white")
    grDevices::dev.off()
    unlink(metrics)
    write.csv(summary, file.path(audit, "performance_order.csv"), row.names = FALSE)
    write.csv(by_device, file.path(audit, "panel_A_device_errors.csv"), row.names = FALSE)
    write.csv(by_rpm, file.path(audit, "panel_B_condition_errors.csv"), row.names = FALSE)
    write.csv(equivalence, file.path(audit, "panel_C_equivalence.csv"), row.names = FALSE)
}
