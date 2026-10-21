# Study analysis and presentation; paths supplied by config.R.
wam_dynamic_figure <- function(cfg, cohort = "primary") {
    # Selected manuscript native dynamic figure; original option J design.
    suppressPackageStartupMessages({library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(grid)})
    
    in_root <- cfg$output
    native_root <- file.path(in_root, if(cohort == "primary") "dynamic_native" else "secondary_native")
    native_tab <- file.path(native_root, "tables")
    out_root <- native_root
    fig_dir <- file.path(out_root, "figures")
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    display_instrument <- function(x) {
        dplyr::recode(as.character(x), SENS = "SENS motion", .default = as.character(x))
    }
    ci_summary <- function(x) {
        x <- x[is.finite(x)]
        n <- length(x)
        if (!n) return(c(n = 0, mean = NA_real_, sd = NA_real_, lower = NA_real_, upper = NA_real_))
        m <- mean(x)
        if (n < 2) return(c(n = n, mean = m, sd = NA_real_, lower = m, upper = m))
        s <- stats::sd(x)
        se <- s / sqrt(n)
        tcrit <- stats::qt(0.975, df = n - 1)
        c(n = n, mean = m, sd = s, lower = m - tcrit * se, upper = m + tcrit * se)
    }
    fmt <- function(x, digits = 1) {
        ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "NA")
    }
    figure_family <- "Arial"
    theme_native <- function(base_size = 11) {
        theme_bw(base_size = base_size, base_family = figure_family) +
            theme(
                plot.title = element_text(face = "bold", hjust = 0, size = base_size + 1),
                plot.subtitle = element_text(hjust = 0, size = base_size - 1),
                axis.title.x = element_text(margin = margin(t = 10)),
                axis.title.y = element_blank(),
                axis.text = element_text(colour = "black"),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.30),
                legend.position = "bottom",
                legend.title = element_text(face = "bold"),
                strip.background = element_rect(fill = "grey90", colour = "grey45", linewidth = 0.35),
                strip.text = element_text(face = "bold"),
                plot.caption = element_text(hjust = 0, size = base_size - 2)
            )
    }
    okabe <- c(
        blue = "#0072B2",
        sky = "#56B4E9",
        green = "#009E73",
        yellow = "#F0E442",
        orange = "#E69F00",
        vermillion = "#D55E00",
        purple = "#CC79A7",
        black = "#000000"
    )
    instrument_pal <- c(
        AX3 = unname(okabe["sky"]),
        GENEActiv = unname(okabe["green"]),
        LEAP = unname(okabe["purple"]),
        `SENS motion` = unname(okabe["orange"]),
        `wGT3X-BT` = unname(okabe["yellow"]),
        `GT9X Link` = unname(okabe["vermillion"])
    )
    rpm_values <- seq(50, 500, by = 50)
    condition_map <- tibble(
        rpm_numeric = rpm_values,
        hz = c(0.8333, 1.6666, 2.5000, 3.3333, 4.1666, 5.0000, 5.8333, 6.6666, 7.5000, 8.3333),
        predicted_g = c(0.0266, 0.1061, 0.2388, 0.4245, 0.6633, 0.9551, 1.3000, 1.6980, 2.1490, 2.6531),
        behaviour_short = c(
            "lie",
            "sit",
            "v.light",
            "light",
            "mod",
            "h.mod",
            "vig",
            "v.vig",
            "v.vig",
            "ext"
        )
    )
    math_italic_g <- intToUtf8(0x1D454)
    condition_axis_labels <- setNames(
        paste0(
            condition_map$rpm_numeric, " RPM\n",
            fmt(condition_map$hz, 2), " Hz\n",
            fmt(condition_map$predicted_g, 2), " ", math_italic_g, "\n",
            condition_map$behaviour_short
        ),
        condition_map$rpm_numeric
    )
    instrument_order <- if(cohort == "primary") c("LEAP", "AX3", "SENS motion", "GENEActiv") else c("LEAP", "wGT3X-BT", "GT9X Link")
    stage_offsets <- setNames(seq(-12, 12, length.out=length(instrument_order)), instrument_order)
    y_map <- tibble(
        instrument_display = instrument_order,
        y_base = rev(seq_along(instrument_order))
    )
    rpm_offset <- function(rpm) {
        ((as.numeric(rpm) - mean(rpm_values)) / diff(range(rpm_values))) * 0.58
    }
    unit_stage <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "device_level_means.csv")), check.names = FALSE) |>
        mutate(
            instrument_display = display_instrument(instrument),
            rpm_numeric = as.numeric(rpm_numeric)
        ) |>
        filter(instrument_display %in% instrument_order, rpm_numeric %in% rpm_values) |>
        left_join(y_map, by = "instrument_display")
    repeat_unit <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "repeatability_device_level.csv")), check.names = FALSE) |>
        mutate(
            instrument_display = display_instrument(instrument),
            rpm_numeric = as.numeric(rpm_numeric)
        ) |>
        filter(instrument_display %in% instrument_order, rpm_numeric %in% rpm_values) |>
        left_join(y_map, by = "instrument_display")
    overall_trial <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "overall_rows.csv")), check.names = FALSE) |>
        mutate(
            instrument_display = display_instrument(instrument),
            signed_scale_factor_error_pct = 100 * as.numeric(scale_factor_error),
            composite_error_pct_half_fsr = as.numeric(composite_error)
        ) |>
        filter(instrument_display %in% instrument_order) |>
        left_join(y_map, by = "instrument_display")
    stage_long <- bind_rows(
        unit_stage |>
            transmute(
                source = "stage",
                metric = "VM integral non-linearity",
                panel = "A. VM integral non-linearity",
                instrument_display,
                device_uid,
                rpm_numeric,
                y_base,
                value = as.numeric(linearity_error_vs_reference),
                x_label = "% FSR[VM]",
                lower_bound = 0
            ),
        unit_stage |>
            transmute(
                source = "stage",
                metric = "Absolute VM bias",
                panel = "B. Absolute VM bias",
                instrument_display,
                device_uid,
                rpm_numeric,
                y_base,
                value = as.numeric(abs_vm_bias_vs_reference),
                x_label = "g",
                lower_bound = 0
            ),
        repeat_unit |>
            transmute(
                source = "stage",
                metric = "Trial-to-trial VM repeatability CV",
                panel = "C. Trial-to-trial VM repeatability CV",
                instrument_display,
                device_uid,
                rpm_numeric,
                y_base,
                value = as.numeric(cv_vm_percent),
                x_label = "%",
                lower_bound = 0
            )
    )
    stage_summary <- stage_long |>
        group_by(metric, panel, instrument_display, rpm_numeric, y_base, x_label, lower_bound) |>
        summarise(as_tibble_row(as.list(ci_summary(value))), .groups = "drop") |>
        mutate(
            y = y_base + rpm_offset(rpm_numeric),
            rpm_label = paste0(rpm_numeric, " RPM")
        )
    games_howell_pairwise <- function(dat, group_col = "instrument_display", value_col = "value") {
        dat <- dat[is.finite(dat[[value_col]]) & !is.na(dat[[group_col]]), , drop = FALSE]
        groups <- sort(unique(as.character(dat[[group_col]])))
        stats <- lapply(groups, function(g) {
            x <- dat[[value_col]][as.character(dat[[group_col]]) == g]
            x <- x[is.finite(x)]
            data.frame(group = g, n = length(x), mean = mean(x), var = stats::var(x))
        }) |>
            bind_rows()
        combs <- utils::combn(groups, 2, simplify = FALSE)
        lapply(combs, function(pair) {
            a <- stats |> filter(group == pair[1])
            b <- stats |> filter(group == pair[2])
            se2 <- a$var / a$n + b$var / b$n
            df <- (se2^2) / (((a$var / a$n)^2 / (a$n - 1)) + ((b$var / b$n)^2 / (b$n - 1)))
            q_stat <- abs(a$mean - b$mean) / sqrt(se2 / 2)
            data.frame(
                group1 = pair[1],
                group2 = pair[2],
                p_adj = stats::ptukey(q_stat, nmeans = length(groups), df = df, lower.tail = FALSE)
            )
        }) |>
            bind_rows()
    }
    compact_letters_from_pairwise <- function(pairwise, means, alpha = 0.05) {
        groups <- names(sort(means, decreasing = FALSE))
        n <- length(groups)
        sig <- matrix(FALSE, n, n, dimnames = list(groups, groups))
        if (nrow(pairwise)) {
            for (i in seq_len(nrow(pairwise))) {
                g1 <- as.character(pairwise$group1[i])
                g2 <- as.character(pairwise$group2[i])
                is_sig <- is.finite(pairwise$p_adj[i]) && pairwise$p_adj[i] < alpha
                sig[g1, g2] <- is_sig
                sig[g2, g1] <- is_sig
            }
        }
        subsets <- list()
        for (mask in seq_len(2^n - 1)) {
            idx <- which(as.logical(intToBits(mask)[seq_len(n)]))
            ok <- TRUE
            if (length(idx) > 1) {
                pairs <- utils::combn(idx, 2)
                ok <- !any(mapply(function(a, b) sig[groups[a], groups[b]], pairs[1, ], pairs[2, ]))
            }
            if (ok) subsets[[length(subsets) + 1]] <- groups[idx]
        }
        nonsig_pairs <- lapply(seq_len(n), function(i) groups[i])
        if (n > 1) {
            pair_idx <- utils::combn(seq_len(n), 2)
            for (j in seq_len(ncol(pair_idx))) {
                a <- pair_idx[1, j]
                b <- pair_idx[2, j]
                if (!sig[groups[a], groups[b]]) nonsig_pairs[[length(nonsig_pairs) + 1]] <- groups[c(a, b)]
            }
        }
        covers_all <- function(selected) {
            all(vapply(nonsig_pairs, function(target) {
                any(vapply(selected, function(s) all(target %in% s), logical(1)))
            }, logical(1)))
        }
        lex_less <- function(a, b) {
            for (i in seq_along(a)) {
                if (a[i] < b[i]) return(TRUE)
                if (a[i] > b[i]) return(FALSE)
            }
            FALSE
        }
        best <- NULL
        best_score <- c(Inf, Inf, Inf)
        for (mask in seq_len(2^length(subsets) - 1)) {
            idx <- which(as.logical(intToBits(mask)[seq_along(subsets)]))
            selected <- subsets[idx]
            if (!covers_all(selected)) next
            score <- c(length(selected), -sum(lengths(selected)), sum(vapply(selected, function(s) min(match(s, groups)), integer(1))))
            if (is.null(best) || lex_less(score, best_score)) {
                best <- selected
                best_score <- score
            }
        }
        best <- best[order(vapply(best, function(s) min(match(s, groups)), integer(1)))]
        letter_pool <- letters
        out <- setNames(rep("", n), groups)
        for (i in seq_along(best)) {
            for (g in best[[i]]) out[g] <- paste0(out[g], letter_pool[i])
        }
        tibble(instrument_display = names(out), letters = unname(out))
    }
    repeatability_letters <- repeat_unit |>
        transmute(
            instrument_display = display_instrument(instrument),
            rpm_numeric = as.numeric(rpm_numeric),
            value = as.numeric(cv_vm_percent)
        ) |>
        group_by(rpm_numeric) |>
        group_modify(function(.x, .y) {
            means <- tapply(.x$value, .x$instrument_display, mean, na.rm = TRUE)
            pw <- games_howell_pairwise(.x)
            compact_letters_from_pairwise(pw, means)
        }) |>
        ungroup() |>
        mutate(metric = "Trial-to-trial VM repeatability CV")
    linearity_letters <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "linearity_device_summary_with_letters.csv")), check.names = FALSE) |>
        transmute(
            metric = "VM integral non-linearity",
            instrument_display = display_instrument(instrument),
            rpm_numeric = as.numeric(rpm_numeric),
            letters = as.character(letters)
        )
    bias_letters <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "absolute_vm_bias_device_summary_with_letters.csv")), check.names = FALSE) |>
        transmute(
            metric = "Absolute VM bias",
            instrument_display = display_instrument(instrument),
            rpm_numeric = as.numeric(rpm_numeric),
            letters = as.character(letters)
        )
    stage_letters <- bind_rows(linearity_letters, bias_letters, repeatability_letters)
    stage_summary <- stage_summary |>
        left_join(stage_letters, by = c("metric", "instrument_display", "rpm_numeric")) |>
        mutate(letters = ifelse(is.na(letters) | letters == "", "-", letters))
    stage_points <- stage_long |>
        mutate(
            y = y_base + rpm_offset(rpm_numeric),
            rpm_label = paste0(rpm_numeric, " RPM")
        )
    overall_long <- bind_rows(
        overall_trial |>
            transmute(
                metric = "VM composite error",
                panel = "C. VM composite error",
                instrument_display,
                device_uid,
                trial_start_utc,
                y_base,
                value = composite_error_pct_half_fsr,
                x_label = "% 1/2 FSR[VM]",
                lower_bound = 0
            ),
        overall_trial |>
            transmute(
                metric = "Signed scale-factor error",
                panel = "D. Signed scale-factor error",
                instrument_display,
                device_uid,
                trial_start_utc,
                y_base,
                value = signed_scale_factor_error_pct,
                x_label = "%",
                lower_bound = NA_real_
            )
    )
    overall_summary <- overall_long |>
        group_by(metric, panel, instrument_display, device_uid, y_base, x_label, lower_bound) |>
        summarise(value = mean(value), .groups = "drop") |>
        group_by(metric, panel, instrument_display, y_base, x_label, lower_bound) |>
        summarise(as_tibble_row(as.list(ci_summary(value))), .groups = "drop")
    overall_letters <- read.csv(file.path(native_tab, paste0(if(cohort == "primary") "native_" else "secondary_native_", "dynamic_profile_long_summary.csv")), check.names = FALSE) |>
        transmute(
            metric = case_when(
                grepl("^VM composite error", metric) ~ "VM composite error",
                grepl("^Signed scale factor error", metric) ~ "Signed scale-factor error",
                TRUE ~ NA_character_
            ),
            instrument_display = display_instrument(instrument),
            letters = as.character(letters)
        ) |>
        filter(!is.na(metric), instrument_display %in% instrument_order)
    overall_summary <- overall_summary |>
        left_join(overall_letters, by = c("metric", "instrument_display")) |>
        mutate(letters = ifelse(is.na(letters) | letters == "", "-", letters))
    plot_overall_panel <- function(metric_name, title, x_lab, x_limits = NULL, zero_line = FALSE,
                                   reference_lines = NULL, show_legend = FALSE, show_letters = TRUE,
                                   letter_offset = NULL, letter_size = 4.0,
                                   letter_position = c("right", "above")) {
        letter_position <- match.arg(letter_position)
        pts <- overall_long |> filter(metric == metric_name)
        sm <- overall_summary |> filter(metric == metric_name)
        if (is.null(letter_offset)) {
            xr <- range(c(pts$value, sm$lower, sm$upper), finite = TRUE)
            letter_offset <- 0.040 * diff(xr)
        }
        sm <- if (letter_position == "above") {
            sm |> mutate(letter_x = mean, letter_y = y_base + 0.22)
        } else {
            sm |> mutate(letter_x = upper + letter_offset, letter_y = y_base)
        }
    
        p <- ggplot() +
            geom_point(
                data = pts,
                aes(x = value, y = y_base, colour = instrument_display),
                alpha = 0.28,
                size = 1.25
            ) +
            geom_segment(
                data = sm,
                aes(x = lower, xend = upper, y = y_base, yend = y_base),
                colour = "black",
                linewidth = 0.70
            ) +
            geom_segment(
                data = sm,
                aes(x = lower, xend = lower, y = y_base - 0.10, yend = y_base + 0.10),
                colour = "black",
                linewidth = 0.70
            ) +
            geom_segment(
                data = sm,
                aes(x = upper, xend = upper, y = y_base - 0.10, yend = y_base + 0.10),
                colour = "black",
                linewidth = 0.70
            ) +
            geom_point(
                data = sm,
                aes(x = mean, y = y_base, fill = instrument_display),
                shape = 21,
                colour = "black",
                size = 2.9,
                stroke = 0.35
            )
    
        if (isTRUE(show_letters)) {
            p <- p +
                geom_text(
                    data = sm,
                    aes(x = letter_x, y = letter_y, label = letters),
                    inherit.aes = FALSE,
                    hjust = if (letter_position == "above") 0.5 else 0,
                    vjust = if (letter_position == "above") 0 else 0.5,
                    size = letter_size,
                    fontface = "bold",
                    colour = "black"
                )
        }
    
        p <- p +
            scale_y_continuous(
                limits = c(0.35, max(y_map$y_base) + 0.65),
                breaks = y_map$y_base,
                labels = y_map$instrument_display,
                expand = expansion(mult = c(0, 0))
            ) +
            scale_colour_manual(values = instrument_pal, drop = FALSE, name = "Device") +
            scale_fill_manual(values = instrument_pal, drop = FALSE, name = "Device") +
            labs(title = title, x = x_lab, y = NULL) +
            theme_native(base_size = 11)
    
        if (isTRUE(zero_line)) {
            p <- p + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35)
        }
        if (!is.null(reference_lines)) {
            p <- p + geom_vline(
                xintercept = reference_lines,
                linetype = "dashed",
                colour = "grey45",
                linewidth = 0.35
            )
        }
        if (!is.null(x_limits)) {
            p <- p + coord_cartesian(xlim = x_limits)
        }
        if (!show_legend) {
            p <- p + theme(legend.position = "none")
        }
        p
    }
    draw_instrument_legend <- function(labels = if (cohort == "primary") c("AX3", "GENEActiv", "LEAP", "SENS motion") else instrument_order,
                                       shade_note = FALSE) {
        grid.text("Device", x = unit(0.26, "npc"), y = unit(0.50, "npc"),
                  just = "right", gp = gpar(fontface = "bold", fontsize = 12, fontfamily = figure_family))
        x0 <- 0.30
        step <- 0.135
        for (i in seq_along(labels)) {
            lab <- labels[[i]]
            x <- x0 + (i - 1) * step
            grid.points(
                x = unit(x, "npc"),
                y = unit(0.50, "npc"),
                pch = 21,
                size = unit(0.17, "in"),
                gp = gpar(fill = instrument_pal[[lab]], col = "black", lwd = 0.8)
            )
            grid.text(lab, x = unit(x + 0.018, "npc"), y = unit(0.50, "npc"),
                      just = "left", gp = gpar(fontsize = 12, fontfamily = figure_family))
        }
        if (isTRUE(shade_note)) {
            grid.text("Heat-map shade = metric value; device identity = row label",
                      x = unit(0.98, "npc"), y = unit(0.50, "npc"),
                      just = "right", gp = gpar(fontsize = 11, fontface = "italic", fontfamily = figure_family))
        }
    }
    draw_wrapped_caption <- function(caption, fontsize = 8.5, wrap_width = 180) {
        if (is.null(caption)) return(invisible(NULL))
        wrapped <- paste(strwrap(caption, width = wrap_width), collapse = "\n")
        grid.text(wrapped, x = unit(0.02, "npc"), y = unit(0.50, "npc"),
                  just = "left", gp = gpar(fontsize = fontsize, lineheight = 0.95, fontfamily = figure_family))
    }
    plot_stage_interval_panel <- function(metric_name, title, y_lab, y_limits = NULL,
                                          label_digits = 1, base_size = 10.5,
                                          show_x_title = TRUE, show_x_axis = TRUE) {
        pts <- stage_points |>
            filter(metric == metric_name) |>
            mutate(
                instrument_display = factor(instrument_display, levels = instrument_order),
                x_plot = rpm_numeric + unname(stage_offsets[as.character(instrument_display)])
            )
        sm <- stage_summary |>
            filter(metric == metric_name) |>
            mutate(
                instrument_display = factor(instrument_display, levels = instrument_order),
                x_plot = rpm_numeric + unname(stage_offsets[as.character(instrument_display)])
            )
        stopifnot(all(is.finite(pts$x_plot)), all(is.finite(sm$x_plot)),
                  n_distinct(pts$instrument_display)==length(instrument_order),
                  nrow(sm)==10L*length(instrument_order))
        y_range <- range(c(sm$lower, sm$upper), finite = TRUE)
        label_reference_range <- if (is.null(y_limits)) y_range else y_limits
        label_offset <- 0.12 * diff(label_reference_range)
        if (!is.finite(label_offset) || label_offset == 0) label_offset <- 0.04
    
        p <- ggplot() +
            geom_point(
                data = pts,
                aes(x = x_plot, y = value, colour = instrument_display),
                alpha = 0.16,
                size = 0.95
            ) +
            geom_errorbar(
                data = sm,
                aes(x = x_plot, ymin = lower, ymax = upper),
                inherit.aes = FALSE,
                colour = "black",
                width = 6.8,
                linewidth = 0.78
            ) +
            geom_point(
                data = sm,
                aes(x = x_plot, y = mean, fill = instrument_display),
                shape = 21,
                colour = "black",
                size = 2.35,
                stroke = 0.30
            ) +
            geom_text(
                data = sm,
                aes(x = x_plot, y = upper + label_offset, label = letters),
                inherit.aes = FALSE,
                colour = "black",
                size = 3.75,
                fontface = "bold",
                show.legend = FALSE
            ) +
            scale_x_continuous(
                # Retain common breaks in A-C so vertical RPM grid lines align even
                # where the shared condition labels are intentionally suppressed.
                breaks = rpm_values,
                labels = if (isTRUE(show_x_axis)) condition_axis_labels else rep("", length(rpm_values)),
                limits = c(28, 522),
                expand = expansion(mult = c(0, 0))
            ) +
            scale_colour_manual(values = instrument_pal, drop = FALSE, name = "Device") +
            scale_fill_manual(values = instrument_pal, drop = FALSE, name = "Device") +
            labs(
                title = title,
                x = if (isTRUE(show_x_title)) {
                    expression(paste("Condition: RPM / frequency (Hz) / predicted ", italic(g), " / behaviour analogue"))
                } else {
                    NULL
                },
                y = y_lab
            ) +
            theme_bw(base_size = base_size, base_family = figure_family) +
            theme(
                plot.title = element_text(face = "bold", hjust = 0, size = base_size + 1),
                axis.title.x = if (isTRUE(show_x_title)) element_text(margin = margin(t = 10), size = base_size - 0.1) else element_blank(),
                axis.title.y = element_text(margin = margin(r = 8), size = base_size),
                axis.text.x = if (isTRUE(show_x_axis)) element_text(size = 12.0, lineheight = 1.15, margin = margin(t = 5), colour = "black") else element_blank(),
                axis.ticks.x = if (isTRUE(show_x_axis)) element_line(colour = "black") else element_blank(),
                axis.text.y = element_text(colour = "black"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.30),
                panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.30),
                legend.position = "none",
                plot.margin = margin(t = 4, r = 8, b = if (isTRUE(show_x_axis)) 4 else 0, l = 6)
            )
    
        if (!is.null(y_limits)) {
            p <- p + coord_cartesian(ylim = y_limits)
        }
        p
    }
    save_stage_interval_shared_axis_five_panel <- function(file, plots, width = 11.4, height = 13.2,
                                                           dpi = 300, title = NULL, caption = NULL) {
        # Measure text on the actual renderer, including when called directly.
        ragg::agg_png(file, width = width, height = height, units = "in", res = dpi, background = "white")
        device <- grDevices::dev.cur()
        on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
        stage_grobs <- lapply(plots[1:3], ggplotGrob)
        shared_widths <- do.call(grid::unit.pmax, lapply(stage_grobs, `[[`, "widths"))
        stage_grobs <- lapply(stage_grobs, function(g) {
            g$widths <- shared_widths
            g
        })
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(
            7,
            2,
            heights = unit(c(0.03, 0.19, 0.19, 0.27, 0.245, 0.035, 0.04), "null")
        )))
        if (!is.null(title)) {
            grid.text(title, x = unit(0.02, "npc"), y = unit(0.60, "npc"),
                      just = "left", gp = gpar(fontface = "bold", fontsize = 15, fontfamily = figure_family))
        }
        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2)); grid.draw(stage_grobs[[1]]); popViewport()
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2)); grid.draw(stage_grobs[[2]]); popViewport()
        pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1:2)); grid.draw(stage_grobs[[3]]); popViewport()
        print(plots[[4]], vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
        print(plots[[5]], vp = viewport(layout.pos.row = 5, layout.pos.col = 2))
        pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 1:2))
        draw_instrument_legend()
        popViewport()
        if (!is.null(caption)) {
            pushViewport(viewport(layout.pos.row = 7, layout.pos.col = 1:2))
            draw_wrapped_caption(caption, fontsize = 8.2, wrap_width = 160)
            popViewport()
        }
        popViewport()
        invisible(dev.off())
    }
    pH4 <- plot_overall_panel(
        "VM composite error",
        "D. Overall VM composite error",
        expression("% 1/2 FSR"["VM"]),
        x_limits = if (cohort == "primary") c(0, 80) else c(-1, 26),
        reference_lines = c(0, 5),
        show_legend = FALSE,
        show_letters = TRUE,
        letter_offset = 2.20,
        letter_size = 4.4,
        letter_position = "above"
    )
    pH5 <- plot_overall_panel(
        "Signed scale-factor error",
        "E. Signed scale-factor error",
        "%",
        x_limits = if (cohort == "primary") c(-12, 55) else c(-30, 9),
        zero_line = TRUE,
        show_legend = FALSE,
        show_letters = TRUE,
        letter_offset = 1.90,
        letter_size = 4.4,
        letter_position = "above"
    )
    pJ1 <- plot_stage_interval_panel(
        "VM integral non-linearity",
        "A. Stage-wise VM integral non-linearity",
        expression("VM integral non-linearity (% FSR"["VM"]*")"),
        y_limits = if (cohort == "primary") c(0, 38) else c(0, 10),
        base_size = 11.0,
        show_x_title = FALSE,
        show_x_axis = FALSE
    )
    pJ2 <- plot_stage_interval_panel(
        "Absolute VM bias",
        "B. Stage-wise absolute VM bias",
        expression(paste("Absolute VM bias (", italic(g), ")")),
        y_limits = if (cohort == "primary") c(0, 1.65) else c(0, .55),
        base_size = 11.0,
        show_x_title = FALSE,
        show_x_axis = FALSE
    )
    pJ3 <- plot_stage_interval_panel(
        "Trial-to-trial VM repeatability CV",
        "C. Stage-wise trial-to-trial VM repeatability",
        "Repeatability CV (%)",
        y_limits = if (cohort == "primary") c(0, 5.8) else c(0, 2.4),
        base_size = 11.0,
        show_x_title = TRUE,
        show_x_axis = TRUE
    )
    pJ4 <- pH4
    pJ5 <- pH5
    caption_shared_axis_alt <- paste(
        "Panels A-C share the condition axis displayed below Panel C. Stage-wise panels show individual device-unit values, device means, 95% CIs, and compact-letter groups at each RPM condition.",
        "Shared letters indicate no statistically significant pairwise difference; letters do not encode performance rank. Overall panels show trial points; means, 95% CIs and Games-Howell letters use 15 unit means."
    )
    save_stage_interval_shared_axis_five_panel(
        file.path(fig_dir, "native_dynamic_profile_300dpi.png"),
        list(pJ1, pJ2, pJ3, pJ4, pJ5),
        width = 11.4,
        height = 13.2,
        dpi = 300,
        title = "Native dynamic metrology profile",
        caption = caption_shared_axis_alt
    )
    save_stage_interval_shared_axis_five_panel(
        file.path(fig_dir, "native_dynamic_profile_600dpi.png"),
        list(pJ1, pJ2, pJ3, pJ4, pJ5),
        width = 11.4,
        height = 13.2,
        dpi = 600,
        title = "Native dynamic metrology profile",
        caption = caption_shared_axis_alt
    )
}
