# Static metrology profile figure ----
# Input: static analysis summaries.
# Output: static outcome comparison panels.

wam_static_figure <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(tidyr)
        library(tibble)
        library(ggplot2)
        library(grid)
    })
    set.seed(240612)

    figure_family <- "Arial"
    analysis_out <- file.path(cfg$output, "static")
    figure_out <- file.path(analysis_out, "figures")
    dir.create(figure_out, recursive = TRUE, showWarnings = FALSE)
    device_order <- c("AX3", "GENEActiv", "GT3X-BT", "GT9X Link", "LEAP", "SENS motion")

    # Use consistent device colours ----
    device_cols <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", `GT3X-BT` = "#D4C100", `GT9X Link` = "#D55E00",
        LEAP = "#CC79A7", `SENS motion` = "#E69F00")
    metric_labels <- c(static_noise_floor_mg = "Static noise floor (mg)", vm_offset_mg = "1-g VM offset bias (mg)",
        vm_drift_mg_h = "VM drift rate (mg/h)", allan_max_ug = "Max-axis Allan minimum (ug)")
    metric_order <- unname(metric_labels[c("static_noise_floor_mg", "vm_offset_mg", "vm_drift_mg_h",
        "allan_max_ug")])
    metric_performance_direction <- c(static_noise_floor_mg = "lower", vm_offset_mg = "absolute_lower",
        vm_drift_mg_h = "absolute_lower", allan_max_ug = "lower")
    save_static_c_figure <- function(plot, stem, width, height) {
        title_label <- "Static metrology profile"
        subtitle_label <- "Faint points show individual devices; black whiskers show device means and 95% CIs."

        draw_figure <- function() {
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 1, heights = unit.c(unit(0.42,
                "in"), unit(0.3, "in"), unit(1, "null")))))

            grid.text(title_label, x = unit(0.01, "npc"), y = unit(0.5, "npc"), just = c("left", "center"),
                gp = gpar(fontsize = 18, fontface = "bold", fontfamily = figure_family), vp = viewport(layout.pos.row = 1,
                  layout.pos.col = 1))

            grid.text(subtitle_label, x = unit(0.01, "npc"), y = unit(0.55, "npc"), just = c("left",
                "center"), gp = gpar(fontsize = 12.5, col = "grey25", fontfamily = figure_family), vp = viewport(layout.pos.row = 2,
                layout.pos.col = 1))

            print(plot, vp = viewport(layout.pos.row = 3, layout.pos.col = 1), newpage = FALSE)
            popViewport()
        }

        png_path <- file.path(figure_out, paste0(stem, ".png"))
        png_300_path <- file.path(figure_out, paste0(stem, "_300dpi.png"))
        png_600_path <- file.path(figure_out, paste0(stem, "_600dpi.png"))
        ragg::agg_png(png_path, width = width, height = height, units = "in", res = 600, background = "white")
        draw_figure()
        grDevices::dev.off()

        ragg::agg_png(png_300_path, width = width, height = height, units = "in", res = 300, background = "white")
        draw_figure()
        grDevices::dev.off()

        ragg::agg_png(png_600_path, width = width, height = height, units = "in", res = 600, background = "white")
        draw_figure()
        grDevices::dev.off()

        invisible(c(png = png_path, png_300 = png_300_path, png_600 = png_600_path))
    }
    base_theme <- function(base_size = 10) {
        theme_bw(base_size = base_size, base_family = figure_family) +
            theme(panel.grid.major = element_line(linewidth = 0.25,
                colour = "grey86"), panel.grid.minor = element_blank(), panel.border = element_rect(linewidth = 0.45,
                colour = "grey35"), strip.background = element_rect(fill = "grey94", colour = "grey50", linewidth = 0.35),
                strip.text = element_text(face = "bold", margin = margin(4, 4, 4, 4)), legend.key = element_blank(),
                legend.title = element_text(face = "bold"), axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold",
                hjust = 0), plot.subtitle = element_text(hjust = 0, colour = "grey25"), plot.caption = element_text(hjust = 0,
                colour = "grey25", size = rel(0.85)), plot.margin = margin(8, 12, 8, 8))
    }
    dat <- readRDS(file.path(analysis_out, "static_clean_master.rds")) |>
        mutate(device = factor(as.character(device), levels = device_order), vm_offset_mg = vm_offset_g_5min *
            1000, vm_drift_mg_h = vm_drift_g_per_h * 1000, allan_max_ug = pmax(x_allan_min_g, y_allan_min_g,
            z_allan_min_g, na.rm = TRUE) * 1e+06)

    # Read figure input tables ----
    summary_tbl <- read.csv(file.path(analysis_out, "static_summary_by_device.csv"), stringsAsFactors = FALSE) |>
        mutate(device = factor(device, levels = device_order), metric_label = factor(unname(metric_labels[var]),
            levels = metric_order))
    metric_device_order <- summary_tbl |>
        mutate(performance_value = dplyr::case_when(metric_performance_direction[var] == "absolute_lower" ~
            abs(mean), TRUE ~ mean), device_chr = as.character(device)) |>
        arrange(metric_label, performance_value, match(device_chr, device_order)) |>
        group_by(metric_label) |>
        mutate(performance_rank = row_number(), device_metric = paste(as.character(metric_label), device_chr,
            sep = "___")) |>
        ungroup()
    metric_device_levels <- metric_device_order |>
        group_by(metric_label) |>
        summarise(levels = list(rev(device_metric)), .groups = "drop") |>
        arrange(match(as.character(metric_label), metric_order)) |>
        pull(levels) |>
        unlist(use.names = FALSE)
    metric_device_axis_labels <- setNames(sub("^.*___", "", metric_device_levels), metric_device_levels)
    metric_device_axis_labels[paste(metric_labels[["allan_max_ug"]], "SENS motion", sep = "___")] <- "SENS motion*"
    summary_long <- summary_tbl |>
        left_join(metric_device_order |>
            select(var, device, device_metric, performance_rank), by = c("var", "device")) |>
        mutate(device_plot = factor(device_metric, levels = metric_device_levels), letter_x = upper)
    zero_lines <- tibble(metric_label = factor(unname(metric_labels[c("vm_offset_mg", "vm_drift_mg_h")]),
        levels = metric_order), xintercept = 0)
    raw_long <- dat |>
        select(device, id, static_noise_floor_mg, vm_offset_mg, vm_drift_mg_h, allan_max_ug) |>
        pivot_longer(cols = c(static_noise_floor_mg, vm_offset_mg, vm_drift_mg_h, allan_max_ug), names_to = "var",
            values_to = "value") |>
        mutate(metric_label = factor(unname(metric_labels[var]), levels = metric_order), device = factor(as.character(device),
            levels = device_order)) |>
        left_join(metric_device_order |>
            select(var, device, device_metric, performance_rank), by = c("var", "device")) |>
        mutate(device_plot = factor(device_metric, levels = metric_device_levels))
    axis_anchor <- bind_rows(summary_long |>
        transmute(metric_label, x = lower), summary_long |>
        transmute(metric_label, x = upper), raw_long |>
        transmute(metric_label, x = value), zero_lines |>
        transmute(metric_label, x = xintercept)) |>
        filter(is.finite(x)) |>
        group_by(metric_label) |>
        summarise(min_x = min(x), max_x = max(x), .groups = "drop") |>
        mutate(span = pmax(max_x - min_x, .Machine$double.eps), signed_metric = metric_label %in% unname(metric_labels[c("vm_offset_mg",
            "vm_drift_mg_h")]), lower = min_x - 0.08 * span, lower = ifelse(signed_metric, lower, pmax(0,
            lower)), upper = max_x + 0.32 * span) |>
        select(metric_label, lower, upper) |>
        pivot_longer(cols = c(lower, upper), names_to = "anchor", values_to = "x") |>
        left_join(metric_device_order |>
            group_by(metric_label) |>
            slice_max(performance_rank, n = 1, with_ties = FALSE) |>
            ungroup() |>
            select(metric_label, device_metric), by = "metric_label") |>
        mutate(device_plot = factor(device_metric, levels = metric_device_levels))
    summary_for_raw <- summary_long |>
        select(device, device_plot, var, metric_label, mean, lower, upper, letters)
    panel_labels <- c(static_noise_floor_mg = "A. Static noise floor (mg)", vm_offset_mg = "B. 1-g VM offset bias (mg)",
        vm_drift_mg_h = "C. VM drift rate (mg/h)", allan_max_ug = "D. Max-axis Allan minimum (ug)")
    panel_order <- unname(panel_labels[c("static_noise_floor_mg", "vm_offset_mg", "vm_drift_mg_h", "allan_max_ug")])
    panel_plotmath_labels <- c(`A. Static noise floor (mg)` = "bold('A. Static noise floor (m')*bolditalic(g)*bold(')')",
        `B. 1-g VM offset bias (mg)` = "bold('B. 1-')*bolditalic(g)*bold(' VM offset bias (m')*bolditalic(g)*bold(')')",
        `C. VM drift rate (mg/h)` = "bold('C. VM drift rate (m')*bolditalic(g)*bold('/h)')", `D. Max-axis Allan minimum (ug)` = "bold('D. Max-axis Allan minimum (')*bold(mu)*bolditalic(g)*bold(')')")
    panel_labeller <- as_labeller(panel_plotmath_labels, default = label_parsed)
    panel_lookup <- setNames(panel_order, metric_order)
    raw_long_c <- raw_long |>
        mutate(panel_label = factor(panel_lookup[as.character(metric_label)], levels = panel_order))
    summary_for_raw_c <- summary_for_raw |>
        mutate(panel_label = factor(panel_lookup[as.character(metric_label)], levels = panel_order))
    axis_anchor_c <- axis_anchor |>
        mutate(panel_label = factor(panel_lookup[as.character(metric_label)], levels = panel_order))
    zero_lines_c <- zero_lines |>
        mutate(panel_label = factor(panel_lookup[as.character(metric_label)], levels = panel_order))
    who_noise_band <- tibble(panel_label = factor(panel_labels[["static_noise_floor_mg"]], levels = panel_order),
        xmin = 1, xmax = 5, ymin = -Inf, ymax = Inf)
    caption_c <- paste0("n = 15 per device. Shared letters indicate no significant Games-Howell pairwise difference within an outcome.",
        "\nShaded region in Panel A indicates the WHO noise-floor specification range.", "\n* SENS motion (Panel D): quantisation-limited, predominantly boundary minima; not evidence of superior stability.")

    # Construct comparison panels ----
    fig_c <- ggplot(raw_long_c, aes(y = device_plot, x = value, colour = device, fill = device)) +
        geom_blank(data = axis_anchor_c,
            aes(x = x, y = device_plot), inherit.aes = FALSE) +
        geom_rect(data = who_noise_band, aes(xmin = xmin,
            xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = "grey70", alpha = 0.22) +
        geom_vline(data = zero_lines_c, aes(xintercept = xintercept), linetype = "dashed", linewidth = 0.4,
            colour = "grey45") +
        geom_point(size = 1.9, alpha = 0.35, shape = 16, show.legend = FALSE) +
        geom_errorbar(data = summary_for_raw_c, orientation = "y", aes(y = device_plot, xmin = lower,
            xmax = upper), inherit.aes = FALSE, width = 0.16, linewidth = 0.75, colour = "black") +
        geom_point(data = summary_for_raw_c,
            aes(x = mean, y = device_plot, fill = device), inherit.aes = FALSE, shape = 21, size = 3.5, stroke = 0.55,
            colour = "black") +
        geom_text(data = summary_for_raw_c, aes(x = upper, y = device_plot, label = letters),
            inherit.aes = FALSE, hjust = -0.35, size = 3.6, fontface = "bold", colour = "black") +
        facet_wrap(~panel_label,
            scales = "free", ncol = 2, labeller = labeller(panel_label = panel_labeller)) +
        scale_colour_manual(values = device_cols,
            guide = "none") +
        scale_fill_manual(values = device_cols, name = "Device") +
        scale_x_continuous(expand = expansion(mult = c(0,
            0))) +
        scale_y_discrete(labels = metric_device_axis_labels) +
        guides(fill = guide_legend(nrow = 1,
            override.aes = list(shape = 21, size = 4.6, colour = "black", alpha = 1))) +
        labs(x = NULL, y = NULL,
            caption = caption_c) + base_theme(13) +
        theme(panel.grid.major = element_line(linewidth = 0.3,
            colour = "grey90"), panel.grid.major.y = element_blank(), strip.background = element_blank(),
            strip.text = element_text(face = "bold", hjust = 0, size = 12.5, margin = margin(3, 0, 4, 0)),
            axis.text = element_text(size = 11, colour = "black"), axis.text.y = element_text(size = 11.5,
            colour = "black"), legend.position = "bottom", legend.title = element_text(face = "bold",
            size = 12.5), legend.text = element_text(size = 11.5), legend.margin = margin(0, 0, 0, 0),
            legend.box.margin = margin(0, 0, 0, 0), plot.caption = element_text(size = 10.5, hjust = 0, colour = "black"),
            plot.margin = margin(5, 8, 6, 5))
    save_static_c_figure(fig_c, "static_profile", width = 11.4, height = 8.4)
}
