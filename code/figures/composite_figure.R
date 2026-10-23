# Composite-error comparison figure ----
# Input: processing model summaries.
# Output: composite-error comparison panels.

wam_composite_figure <- function(composite_summary, composite_trials, fig_dir, tab_dir) {
    pipeline_order <- c("Native", "Native + calibration", "LP10", "LP10 + calibration")
    composite_summary <- composite_summary |>
        mutate(pipeline_label = factor(pipeline_label, levels = pipeline_order))
    composite_trials <- composite_trials |>
        mutate(pipeline_label = factor(pipeline_label, levels = pipeline_order))
    pipeline_order <- c("Native", "Native + calibration", "LP10", "LP10 + calibration")
    processed_pipeline_order <- c("Native + calibration", "LP10", "LP10 + calibration")
    pipeline_cols <- c(Native = "#000000", `Native + calibration` = "#1B9E77", LP10 = "#D95F02", `LP10 + calibration` = "#7570B3")
    pipeline_offsets <- c(Native = -0.3, `Native + calibration` = -0.1, LP10 = 0.1, `LP10 + calibration` = 0.3)
    pipeline_legend_labels <- c(Native = "Native", `Native + calibration` = "Dynamic calibration", LP10 = "10-Hz low-pass",
        `LP10 + calibration` = "Low-pass + calibration")

    theme_dynamic <- function(base_size = 11) {
        theme_bw(base_size = base_size, base_family = "Arial") +
            theme(panel.grid.major = element_line(linewidth = 0.25,
                colour = "grey88"), panel.grid.minor = element_blank(), panel.border = element_rect(linewidth = 0.45,
                colour = "grey35"), strip.background = element_rect(fill = "grey94", colour = "grey50", linewidth = 0.35),
                strip.text = element_text(face = "bold", size = base_size + 0.5, margin = margin(5, 5, 5,
                5)), legend.position = "bottom", legend.title = element_text(face = "bold"), axis.title = element_text(face = "bold"),
                axis.text = element_text(colour = "grey25"), plot.title = element_text(face = "bold", hjust = 0),
                plot.caption = element_text(hjust = 0, colour = "grey25", size = rel(0.86)), plot.margin = margin(8,
                12, 8, 8))
    }

    # Order by native composite error.
    composite_device_order <- composite_summary |>
        filter(pipeline_label == "Native") |>
        arrange(mean) |>
        pull(instrument) |>
        as.character()

    composite_summary <- composite_summary |>
        mutate(instrument = factor(as.character(instrument), levels = composite_device_order))
    composite_trials <- composite_trials |>
        mutate(instrument = factor(as.character(instrument), levels = composite_device_order))

    composite_summary_plot <- composite_summary |>
        mutate(instrument_index = as.numeric(instrument), pipeline_index = as.numeric(pipeline_label),
            x_pos = instrument_index + unname(pipeline_offsets[as.character(pipeline_label)]), label_y = upper +
                max(upper, na.rm = TRUE) * 0.055)
    composite_trials_plot <- composite_trials |>
        mutate(instrument_index = as.numeric(instrument), x_pos = instrument_index + unname(pipeline_offsets[as.character(pipeline_label)]))

    # Construct comparison panels ----
    p_composite_one_panel <- ggplot() +
        geom_vline(xintercept = seq(1.5, 3.5, by = 1), colour = "grey88",
            linewidth = 0.35) +
        geom_line(data = composite_summary_plot, aes(x = x_pos, y = mean, group = instrument),
            colour = "grey55", linewidth = 0.42, alpha = 0.75) +
        geom_point(data = composite_trials_plot,
            aes(x = x_pos, y = composite_error, colour = pipeline_label), size = 0.85, alpha = 0.18, show.legend = FALSE) +
        geom_errorbar(data = composite_summary_plot, aes(x = x_pos, ymin = lower, ymax = upper, colour = pipeline_label),
            width = 0.09, linewidth = 0.62) +
        geom_point(data = composite_summary_plot, aes(x = x_pos,
            y = mean, fill = pipeline_label), shape = 21, size = 2.95, stroke = 0.58, colour = "black") +
        geom_label(data = composite_summary_plot, aes(x = x_pos, y = label_y, label = letters, colour = pipeline_label),
            fontface = "bold", size = 5, fill = "white", label.padding = unit(0.11, "lines"), label.r = unit(0.08,
            "lines"), linewidth = 0.24, show.legend = FALSE) +
        scale_x_continuous(breaks = seq_along(composite_device_order),
            labels = composite_device_order, limits = c(0.55, 4.45), expand = expansion(mult = c(0.01, 0.01))) +
        scale_colour_manual(values = pipeline_cols, name = "Processing pipeline", labels = pipeline_legend_labels[pipeline_order],
            drop = FALSE) +
        scale_fill_manual(values = pipeline_cols, name = "Processing pipeline", labels = pipeline_legend_labels[pipeline_order],
            drop = FALSE) +
        labs(title = "VM composite error by device and processing pipeline", subtitle = NULL,
            x = NULL, y = expression("VM composite error (% 1/2 FSR"["VM"] * ")")) +
        coord_cartesian(ylim = c(0,
            78), clip = "off") +
        guides(colour = guide_legend(override.aes = list(linewidth = 0.7, alpha = 1,
            size = 2.8), nrow = 1, byrow = TRUE), fill = "none") +
        theme_dynamic(14) +
        theme(legend.position = "bottom",
            axis.text.x = element_text(size = 14, face = "bold"), axis.text.y = element_text(size = 12.5),
            axis.title.y = element_text(face = "bold", size = 16), legend.title = element_text(size = 15,
            face = "bold"), legend.text = element_text(size = 13), legend.key.width = unit(0.38, "cm"),
            plot.title = element_text(size = 15), plot.subtitle = element_blank(), legend.margin = margin(t = 10,
            r = 0, b = 5, l = 0))

    write.csv(composite_summary_plot |>
        select(pipeline_label, instrument, mean, lower, upper, letters, x_pos, label_y), file.path(tab_dir,
        "composite_one_panel_plot_summary.csv"), row.names = FALSE)
    if (isTRUE(getOption("wam.render_figures", TRUE))) {
        for (dpi in c(300, 600)) {

            # Save figure outputs ----
            ggsave(file.path(fig_dir, paste0("composite_error_", dpi, "dpi.png")), p_composite_one_panel,
                width = 10, height = 6.4, dpi = dpi, bg = "white", device = ragg::agg_png)
        }
    }
    invisible(p_composite_one_panel)
}
