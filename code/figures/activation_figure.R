# Low-excitation response figure ----
# Input: activation analysis outputs.
# Output: low-excitation response panels.

wam_activation_figure <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(ggplot2)
    })
    tab_dir <- file.path(cfg$output, "activation/tables")
    instrument_order <- c("AX3", "GENEActiv", "LEAP", "SENS motion")
    okabe <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", LEAP = "#CC79A7", `SENS motion` = "#E69F00")

    # Read figure input tables ----
    stage_summary <- read.csv(file.path(tab_dir, "activation_deadband_stage_response_summary.csv")) |>
        filter(instrument %in% instrument_order) |>
        mutate(instrument = factor(instrument, levels = instrument_order))
    stage_points <- read.csv(file.path(tab_dir, "activation_deadband_stage_response_master.csv")) |>
        filter(instrument %in% instrument_order) |>
        mutate(instrument = factor(instrument, levels = instrument_order))
    conditions <- read.csv(file.path(tab_dir, "Table_activation_conditions_rpm_frequency_acceleration.csv")) |>
        arrange(nominal_rpm)
    stopifnot(identical(conditions$nominal_rpm, seq(15L, 100L, 5L)))

    # Construct comparison panels ----
    response_plot <- ggplot(stage_summary, aes(x = nominal_rpm, y = coherent_response_percent_mean, colour = instrument,
            fill = instrument)) +
        geom_hline(yintercept = 10, linetype = "dashed", colour = "grey35", linewidth = 0.45) +
        geom_hline(yintercept = 50, linetype = "solid", colour = "grey35", linewidth = 0.45) +
        geom_ribbon(aes(ymin = coherent_response_percent_lower,
            ymax = coherent_response_percent_upper), colour = NA, alpha = 0.16) +
        geom_point(data = stage_points,
            aes(x = nominal_rpm, y = coherent_response_percent, colour = instrument), inherit.aes = FALSE,
            alpha = 0.24, size = 1.35, position = position_identity()) +
        geom_errorbar(aes(ymin = coherent_response_percent_lower,
            ymax = coherent_response_percent_upper), width = 1, linewidth = 0.38, alpha = 0.88) +
        geom_line(linewidth = 0.95) +
        geom_point(shape = 21, colour = "black", stroke = 0.35, size = 2.4) +
        annotate("text", x = 101.5,
            y = 10, label = "10%", hjust = 0, vjust = -0.25, size = 3.6, colour = "grey25") +
        annotate("text",
            x = 101.5, y = 50, label = "50%", hjust = 0, vjust = -0.25, size = 3.6, colour = "grey25") +
        scale_colour_manual(values = okabe, drop = FALSE) +
        scale_fill_manual(values = okabe, drop = FALSE) +
        scale_x_continuous(breaks = seq(15, 100, by = 5), limits = c(14, 106), expand = expansion(mult = c(0,
            0))) +
        scale_y_continuous(breaks = seq(0, 170, by = 20), expand = expansion(mult = c(0, 0.02))) +
        coord_cartesian(ylim = c(0, 170)) +
        labs(title = "Low-intensity activation response", x = "Orbital shaker speed (RPM)",
            y = "Response (% predicted)", colour = "Instrument", fill = "Instrument") +
        theme_bw(base_size = 15) +
        theme(plot.title = element_text(face = "bold", size = 17), panel.grid.minor = element_blank(),
            legend.position = "bottom", legend.title = element_text(face = "bold"), axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 8)))

    original_plot <- response_plot
    condition_labels <- sprintf("%d RPM\n%.2f Hz\n%.4f g", conditions$nominal_rpm, conditions$frequency_hz,
        conditions$centripetal_g)
    names(condition_labels) <- as.character(conditions$nominal_rpm)
    response_plot <- response_plot +
        scale_x_continuous(breaks = conditions$nominal_rpm, labels = function(x) unname(condition_labels[as.character(x)]),
            limits = c(14, 106), expand = expansion(mult = c(0, 0))) +
        labs(x = expression(paste("Condition: RPM / frequency (Hz) / predicted acceleration (",
            italic(g), ")")))
    stopifnot(identical(original_plot$theme, response_plot$theme), identical(ggplot_build(original_plot)$data,
        ggplot_build(response_plot)$data))

    # Preserve relative text sizing.
    font_scale <- 1.1
    for (name in names(response_plot$theme)) {
        element <- response_plot$theme[[name]]
        if (inherits(element, "element_text") && is.numeric(element$size) && !inherits(element$size,
            "rel")) {
            element$size <- element$size * font_scale
            response_plot$theme[[name]] <- element
        }
    }
    for (i in seq_along(response_plot$layers)) {
        if (inherits(response_plot$layers[[i]]$geom, "GeomText") && is.numeric(response_plot$layers[[i]]$aes_params$size)) {
            response_plot$layers[[i]]$aes_params$size <- response_plot$layers[[i]]$aes_params$size *
                font_scale
        }
    }
    # Preserve scaled baseline gaps.
    figure_grob <- ggplotGrob(response_plot)
    axis_index <- which(figure_grob$layout$name == "axis-b")
    axis_grob <- figure_grob$grobs[[axis_index]]
    axis_table <- axis_grob$children$axis
    title_index <- which(vapply(axis_table$grobs, inherits, logical(1), "titleGrob"))
    tick_title <- axis_table$grobs[[title_index]]
    tick_text <- tick_title$children[[1]]
    label_gap <- grid::unit(16.711 * font_scale, "bigpts")
    label_rows <- list(as.expression(as.list(sprintf("%d RPM", conditions$nominal_rpm))), as.expression(as.list(sprintf("%.2f Hz",
        conditions$frequency_hz))), as.expression(lapply(conditions$centripetal_g, function(value) {
        value_text <- sprintf("%.4f ", value)
        bquote(paste(.(value_text), italic(g)))
    })))
    row_grobs <- lapply(seq_along(label_rows), function(i) {
        row <- grid::textGrob(label_rows[[i]], x = tick_text$x, y = grid::unit(0, "npc"), hjust = 0.5,
            vjust = 0, gp = tick_text$gp, name = paste0("condition-label-row-", i))
        row$y <- grid::unit(1, "npc") - tick_title$heights[1] - grid::grobAscent(grid::textGrob("100 RPM",
            gp = tick_text$gp)) - (i - 1) * label_gap - grid::grobDescent(grid::textGrob(label_rows[[i]][1],
            gp = tick_text$gp))
        row
    })
    tick_title <- grid::setChildren(tick_title, do.call(grid::gList, row_grobs))
    # Reserve RPM-to-Hz label spacing.
    tick_title$heights[2] <- grid::grobAscent(row_grobs[[1]]) + 2 * label_gap + grid::grobDescent(grid::textGrob(label_rows[[3]][1],
        gp = tick_text$gp)) + grid::unit(3, "points")
    axis_table$grobs[[title_index]] <- tick_title
    axis_table$heights[axis_table$layout$t[title_index]] <- sum(tick_title$heights)
    axis_grob$children$axis <- axis_table
    axis_grob$height <- sum(axis_table$heights)
    axis_grob$vp$height <- axis_grob$height
    figure_grob$grobs[[axis_index]] <- axis_grob
    figure_grob$heights[figure_grob$layout$t[axis_index]] <- axis_grob$height

    out <- file.path(cfg$output, "activation/figures")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)

    # Save figure outputs ----
    ggsave(file.path(out, "activation_response.png"), figure_grob, width = 16.5, height = 9, units = "in",
        dpi = 600, bg = "white")
    ggsave(file.path(out, "activation_response.pdf"), figure_grob, width = 16.5, height = 9, units = "in",
        device = grDevices::cairo_pdf, bg = "white")
    # Preserve recorded RPM positions.
    points <- ggplot_build(response_plot)$data[[4]]
    stopifnot(isTRUE(all.equal(points$x, as.numeric(stage_points$nominal_rpm))))
    message("Activation figure rendered without jitter; source summaries unchanged.")
}
