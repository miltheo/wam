# Primary native dynamic comparisons ----
# Input: primary dynamic outcome masters.
# Output: comparisons and summary tables.

wam_dynamic_native <- function(cfg) {

    project_root <- cfg$root
    analysis_root <- cfg$masters
    out_root <- file.path(cfg$output, "dynamic_native")
    tab_dir <- file.path(out_root, "tables")
    fig_dir <- file.path(out_root, "figures")
    supp_fig_dir <- file.path(fig_dir, "supplementary")

    dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(supp_fig_dir, recursive = TRUE, showWarnings = FALSE)

    required_pkgs <- c("dplyr", "tidyr", "tibble", "ggplot2", "nlme")
    missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_pkgs)) {
        stop("Missing required R packages: ", paste(missing_pkgs, collapse = ", "))
    }

    library(dplyr)
    library(tidyr)
    library(tibble)
    library(ggplot2)
    library(nlme)
    library(grid)

    device_order <- c("AX3", "GENEActiv", "LEAP", "SENS motion")
    device_cols <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", LEAP = "#CC79A7", `SENS motion` = "#E69F00")

    native_pipeline <- "UC_testNative_refNative26_5s"
    rpm_breaks <- seq(50, 500, 50)
    rpm_xlim <- c(25, 525)

    fmt_p <- function(p) {
        ifelse(!is.finite(p), NA_character_, ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p)))
    }

    sig_bin <- function(p) {
        dplyr::case_when(!is.finite(p) ~ "Not estimable", p < 0.001 ~ "p < 0.001", p < 0.01 ~ "p < 0.01",
            p < 0.05 ~ "p < 0.05", TRUE ~ "p >= 0.05")
    }

    mean_ci <- function(x, alpha = 0.05) {
        x <- x[is.finite(x)]
        n <- length(x)
        if (n == 0L) {
            return(tibble(mean = NA_real_, lower = NA_real_, upper = NA_real_, n = 0L))
        }
        m <- mean(x)
        if (n < 2L) {
            return(tibble(mean = m, lower = NA_real_, upper = NA_real_, n = n))
        }
        se <- stats::sd(x)/sqrt(n)
        tcrit <- stats::qt(1 - alpha/2, df = n - 1)
        tibble(mean = m, lower = m - tcrit * se, upper = m + tcrit * se, n = n)
    }

    write_csv_base <- function(x, path) {
        utils::write.csv(x, path, row.names = FALSE, na = "")
    }

    theme_native <- function(base_size = 11) {
        theme_bw(base_size = base_size) +
            theme(panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
                panel.grid.minor = element_blank(), panel.border = element_rect(linewidth = 0.45, colour = "grey35"),
                strip.background = element_rect(fill = "grey94", colour = "grey50", linewidth = 0.35), strip.text = element_text(face = "bold",
                size = base_size + 0.5, margin = margin(5, 5, 5, 5)), legend.position = "bottom", legend.title = element_text(face = "bold"),
                legend.text = element_text(size = base_size - 0.5), axis.title = element_text(face = "bold"),
                axis.text = element_text(colour = "grey25"), plot.title = element_text(face = "bold", hjust = 0),
                plot.subtitle = element_text(hjust = 0, colour = "grey25"), plot.caption = element_text(hjust = 0,
                colour = "grey25", size = rel(0.86)), plot.margin = margin(8, 12, 8, 8))
    }

    save_plot_set <- function(plot, stem, width, height, dir = fig_dir) {
        if (!isTRUE(getOption("wam.render_figures", FALSE)) || identical(dir, supp_fig_dir))
            return(invisible(NULL))
        paths <- c(png = file.path(dir, paste0(stem, ".png")), png_300 = file.path(dir, paste0(stem,
            "_300dpi.png")), png_600 = file.path(dir, paste0(stem, "_600dpi.png")), pdf = file.path(dir,
            paste0(stem, ".pdf")))
        ggsave(paths[["png"]], plot, width = width, height = height, dpi = 600, bg = "white")
        ggsave(paths[["png_300"]], plot, width = width, height = height, dpi = 300, bg = "white")
        ggsave(paths[["png_600"]], plot, width = width, height = height, dpi = 600, bg = "white")
        ggsave(paths[["pdf"]], plot, width = width, height = height, bg = "white")
        invisible(paths)
    }

    save_combined_plot_set <- function(top_plot, bottom_plot, stem, width, height, top_height = 4.8,
        bottom_height = 1.55, dir = fig_dir) {
        if (!isTRUE(getOption("wam.render_figures", FALSE)) || identical(dir, supp_fig_dir))
            return(invisible(NULL))
        top_grob <- ggplotGrob(top_plot)
        bottom_grob <- ggplotGrob(bottom_plot)
        if (length(top_grob$widths) == length(bottom_grob$widths)) {
            aligned_widths <- grid::unit.pmax(top_grob$widths, bottom_grob$widths)
            top_grob$widths <- aligned_widths
            bottom_grob$widths <- aligned_widths
        }

        draw <- function() {
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, heights = unit.c(unit(top_height,
                "null"), unit(bottom_height, "null")))))
            pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
            grid.draw(top_grob)
            popViewport()
            pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
            grid.draw(bottom_grob)
            popViewport()
            popViewport()
        }

        paths <- c(png = file.path(dir, paste0(stem, ".png")), png_300 = file.path(dir, paste0(stem,
            "_300dpi.png")), png_600 = file.path(dir, paste0(stem, "_600dpi.png")), pdf = file.path(dir,
            paste0(stem, ".pdf")))

        grDevices::png(paths[["png"]], width = width, height = height, units = "in", res = 600, bg = "white")
        draw()
        grDevices::dev.off()

        grDevices::png(paths[["png_300"]], width = width, height = height, units = "in", res = 300, bg = "white")
        draw()
        grDevices::dev.off()

        grDevices::png(paths[["png_600"]], width = width, height = height, units = "in", res = 600, bg = "white")
        draw()
        grDevices::dev.off()

        grDevices::pdf(paths[["pdf"]], width = width, height = height)
        draw()
        grDevices::dev.off()

        invisible(paths)
    }

    summarise_device_metric <- function(device_data, metric_var) {
        device_data |>
            group_by(instrument, rpm_factor, rpm_numeric) |>
            summarise(mean = mean(.data[[metric_var]], na.rm = TRUE), lower = mean_ci(.data[[metric_var]])$lower,
                upper = mean_ci(.data[[metric_var]])$upper, sd = stats::sd(.data[[metric_var]], na.rm = TRUE),
                n_devices = n_distinct(device_uid), .groups = "drop")
    }

    # Find compatible comparison groups ----
    valid_letter_sets <- function(groups, sig_mat) {
        n <- length(groups)
        candidates <- list()
        for (mask in seq_len(2^n - 1L)) {
            idx <- which(as.logical(intToBits(mask)[seq_len(n)]))
            valid <- TRUE
            if (length(idx) > 1L) {
                combs <- utils::combn(idx, 2)
                valid <- all(!sig_mat[cbind(combs[1, ], combs[2, ])])
            }
            if (valid) {
                candidates[[length(candidates) + 1L]] <- groups[idx]
            }
        }
        candidates
    }

    # Encode pairwise significance groups ----
    compact_letters_from_pairs <- function(emm_tbl, pairwise_tbl, alpha = 0.05, lower_better = TRUE) {
        groups <- as.character(emm_tbl$instrument)
        values <- emm_tbl$response
        order_idx <- if (isTRUE(lower_better))
            order(values, match(groups, device_order)) else order(-values, match(groups, device_order))
        groups_ordered <- groups[order_idx]

        sig_mat <- matrix(FALSE, nrow = length(groups_ordered), ncol = length(groups_ordered), dimnames = list(groups_ordered,
            groups_ordered))

        if (nrow(pairwise_tbl) > 0L) {
            for (i in seq_len(nrow(pairwise_tbl))) {
                g1 <- as.character(pairwise_tbl$group1[i])
                g2 <- as.character(pairwise_tbl$group2[i])
                p <- pairwise_tbl$p_value[i]
                if (g1 %in% groups_ordered && g2 %in% groups_ordered && is.finite(p) && p < alpha) {
                  sig_mat[g1, g2] <- TRUE
                  sig_mat[g2, g1] <- TRUE
                }
            }
        }

        candidates <- valid_letter_sets(groups_ordered, sig_mat)

        nonsig_pairs <- list()
        for (i in seq_along(groups_ordered)) {
            for (j in seq_along(groups_ordered)) {
                if (j > i && !sig_mat[i, j]) {
                  nonsig_pairs[[length(nonsig_pairs) + 1L]] <- sort(c(groups_ordered[i], groups_ordered[j]))
                }
            }
        }

        covers_all <- function(set_combo) {
            covered_groups <- unique(unlist(set_combo, use.names = FALSE))
            if (!setequal(covered_groups, groups_ordered))
                return(FALSE)
            if (!length(nonsig_pairs))
                return(TRUE)
            all(vapply(nonsig_pairs, function(pair) {
                any(vapply(set_combo, function(s) all(pair %in% s), logical(1)))
            }, logical(1)))
        }

        selected <- NULL
        for (k in seq_along(candidates)) {
            combos <- utils::combn(seq_along(candidates), k, simplify = FALSE)
            ok <- vapply(combos, function(idx) covers_all(candidates[idx]), logical(1))
            if (any(ok)) {
                selected <- candidates[combos[[which(ok)[1L]]]]
                break
            }
        }
        if (is.null(selected))
            selected <- as.list(groups_ordered)

        selected <- selected[order(vapply(selected, function(s) min(match(s, groups_ordered)), integer(1)),
            -vapply(selected, length, integer(1)))]
        alphabet <- letters
        letter_map <- setNames(rep("", length(groups_ordered)), groups_ordered)
        for (i in seq_along(selected)) {
            for (g in selected[[i]]) {
                letter_map[g] <- paste0(letter_map[g], alphabet[i])
            }
        }

        tibble(instrument = factor(names(letter_map), levels = device_order), letters = unname(letter_map))
    }

    games_howell_metric <- function(data, metric_var, metric_label, lower_better = TRUE) {
        # Average trials within physical units.
        metric_data <- data |>
            group_by(instrument, device_uid) |>
            summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") |>
            filter(!is.na(instrument), is.finite(value))

        summary_tbl <- metric_data |>
            group_by(instrument) |>
            summarise(n = n(), mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), lower = mean_ci(value)$lower,
                upper = mean_ci(value)$upper, median = median(value, na.rm = TRUE), q1 = quantile(value,
                  0.25, na.rm = TRUE), q3 = quantile(value, 0.75, na.rm = TRUE), min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE), variance = stats::var(value, na.rm = TRUE), .groups = "drop") |>
            mutate(instrument = factor(as.character(instrument), levels = device_order))

        welch <- stats::oneway.test(value ~ instrument, data = metric_data, var.equal = FALSE)
        groups <- summary_tbl |>
            arrange(match(instrument, device_order))
        pairwise_tbl <- bind_rows(lapply(seq_len(nrow(groups) - 1L), function(i) {
            bind_rows(lapply((i + 1L):nrow(groups), function(j) {
                g1 <- groups[i, ]
                g2 <- groups[j, ]
                se <- sqrt(g1$variance/g1$n + g2$variance/g2$n)
                df <- (g1$variance/g1$n + g2$variance/g2$n)^2/((g1$variance/g1$n)^2/(g1$n - 1) + (g2$variance/g2$n)^2/(g2$n -
                  1))
                q_stat <- sqrt(2) * abs(g1$mean - g2$mean)/se
                p <- stats::ptukey(q_stat, nmeans = nrow(groups), df = df, lower.tail = FALSE)
                tibble(metric = metric_label, group1 = as.character(g1$instrument), group2 = as.character(g2$instrument),
                  contrast = paste(as.character(g1$instrument), "-", as.character(g2$instrument)), estimate = g1$mean -
                    g2$mean, SE = se, df = df, q = q_stat, p_value = p, p_text = fmt_p(p), significance = sig_bin(p),
                  contrast_label = paste(as.character(g1$instrument), "vs", as.character(g2$instrument)))
            }))
        }))

        letters_tbl <- compact_letters_from_pairs(summary_tbl |>
            transmute(instrument, response = mean), pairwise_tbl, lower_better = lower_better)

        list(summary = summary_tbl |>
            select(-variance) |>
            left_join(letters_tbl, by = "instrument") |>
            mutate(metric = metric_label, .before = instrument), test = tibble(metric = metric_label,
            test = "Welch ANOVA", df1 = unname(welch$parameter[1]), df2 = unname(welch$parameter[2]),
            statistic = unname(welch$statistic), p = welch$p.value, p_text = fmt_p(welch$p.value)), pairwise = pairwise_tbl)
    }

    fmt_estimate_letters <- function(mean, lower, upper, letters, digits) {
        sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f] %s"), mean, lower, upper,
            letters)
    }

    # Fit native repeated-measures models ----
    run_native_metric_analysis <- function(stage_data, device_data, metric_var, model_var, metric_slug,
        metric_name, y_label, lower_better = TRUE) {
        model_data <- stage_data |>
            filter(is.finite(.data[[model_var]]), is.finite(rpm_numeric), !is.na(instrument))

        full_formula <- as.formula(paste0(model_var, " ~ instrument * rpm_factor"))
        reduced_formula <- as.formula(paste0(model_var, " ~ instrument + rpm_factor"))

        # Fit full interaction model ----
        full <- nlme::lme(fixed = full_formula, random = ~1 | device_uid/trial_uid, data = model_data,
            method = "ML", control = nlme::lmeControl(maxIter = 200, msMaxIter = 200, opt = "optim"))

        # Fit additive comparison model ----
        reduced <- nlme::lme(fixed = reduced_formula, random = ~1 | device_uid/trial_uid, data = model_data,
            method = "ML", control = nlme::lmeControl(maxIter = 200, msMaxIter = 200, opt = "optim"))

        lrt_raw <- as.data.frame(anova(reduced, full)) |>
            rownames_to_column("model")

        lrt <- lrt_raw |>
            mutate(metric = metric_name, test = "Instrument x RPM interaction", chisq = L.Ratio, df = df,
                p = `p-value`, p_text = fmt_p(p), full_singular = NA, reduced_singular = NA)

        # Construct model-prediction contrast grid ----
        emm_grid <- expand.grid(instrument = factor(device_order, levels = device_order), rpm_factor = factor(levels(model_data$rpm_factor),
            levels = levels(model_data$rpm_factor)), KEEP.OUT.ATTRS = FALSE) |>
            mutate(rpm_numeric = as.numeric(as.character(rpm_factor))) |>
            arrange(rpm_numeric, instrument)

        fixed_terms <- delete.response(terms(full))
        xmat <- model.matrix(fixed_terms, data = emm_grid)
        beta <- nlme::fixef(full)
        vc <- vcov(full)
        xmat <- xmat[, names(beta), drop = FALSE]
        pred <- as.numeric(xmat %*% beta)
        pred_se <- sqrt(diag(xmat %*% vc %*% t(xmat)))
        df_pred <- min(full$fixDF$X, na.rm = TRUE)
        if (!is.finite(df_pred))
            df_pred <- nrow(model_data) - length(beta)
        tcrit <- stats::qt(0.975, df = df_pred)

        emm_tbl <- emm_grid |>
            mutate(emmean = pred, SE = pred_se, df = df_pred, metric = metric_name, response = exp(emmean) -
                1, response_lower = exp(emmean - tcrit * SE) - 1, response_upper = exp(emmean + tcrit *
                SE) - 1, instrument = factor(as.character(instrument), levels = device_order))

        pairwise_tbl <- bind_rows(lapply(sort(unique(emm_grid$rpm_numeric)), function(rpm_i) {
            idx <- which(emm_grid$rpm_numeric == rpm_i)
            combos <- utils::combn(idx, 2)
            bind_rows(lapply(seq_len(ncol(combos)), function(j) {
                i1 <- combos[1, j]
                i2 <- combos[2, j]
                contrast_vec <- xmat[i1, ] - xmat[i2, ]
                estimate <- as.numeric(contrast_vec %*% beta)
                se <- sqrt(as.numeric(contrast_vec %*% vc %*% contrast_vec))
                t_ratio <- estimate/se
                p_raw <- 2 * stats::pt(abs(t_ratio), df = df_pred, lower.tail = FALSE)
                tibble(rpm_factor = emm_grid$rpm_factor[i1], rpm_numeric = rpm_i, group1 = as.character(emm_grid$instrument[i1]),
                  group2 = as.character(emm_grid$instrument[i2]), contrast = paste(group1, "-", group2),
                  estimate = estimate, SE = se, df = df_pred, t.ratio = t_ratio, p_raw = p_raw)
            }))
        })) |>
            group_by(rpm_numeric) |>

            # Adjust within comparison family ----
            mutate(p_value = p.adjust(p_raw, method = "holm")) |>
            ungroup() |>
            mutate(metric = metric_name, p_holm_within_rpm = p_value, p_text = fmt_p(p_value), p_holm_within_rpm_text = p_text,
                p_global_holm = p.adjust(p_value, method = "holm"), p_global_holm_text = fmt_p(p_global_holm),
                percent_difference = (exp(estimate) - 1) * 100, percent_lower = (exp(estimate - 1.96 *
                  SE) - 1) * 100, percent_upper = (exp(estimate + 1.96 * SE) - 1) * 100, significance = sig_bin(p_value),
                contrast_label = paste(group1, "vs", group2)) |>
            arrange(rpm_numeric, match(group1, device_order), match(group2, device_order))

        letters_tbl <- bind_rows(lapply(sort(unique(emm_tbl$rpm_numeric)), function(rpm_i) {
            emm_i <- emm_tbl |>
                filter(rpm_numeric == rpm_i)
            pair_i <- pairwise_tbl |>
                filter(rpm_numeric == rpm_i)
            compact_letters_from_pairs(emm_i, pair_i, lower_better = lower_better) |>
                mutate(metric = metric_name, rpm_numeric = rpm_i)
        })) |>
            select(metric, rpm_numeric, instrument, letters)

        observed_summary <- summarise_device_metric(device_data, metric_var) |>
            mutate(metric = metric_name) |>
            left_join(letters_tbl, by = c("metric", "rpm_numeric", "instrument")) |>
            arrange(rpm_numeric, instrument)

        list(full_model = full, reduced_model = reduced, lrt = lrt, emm = emm_tbl, pairwise = pairwise_tbl,
            letters = letters_tbl, observed_summary = observed_summary, metric_slug = metric_slug, metric_name = metric_name,
            y_label = y_label, lower_better = lower_better)
    }

    make_label_positions <- function(summary_tbl, metric_var = "mean", label_pad_frac = 0.045, label_gap_frac = 0.035) {
        y_range <- range(c(summary_tbl$lower, summary_tbl$upper), na.rm = TRUE)
        span <- diff(y_range)
        if (!is.finite(span) || span <= 0)
            span <- max(abs(y_range), na.rm = TRUE)
        pad <- span * label_pad_frac
        gap <- span * label_gap_frac

        summary_tbl |>
            group_by(rpm_numeric) |>
            arrange(desc(.data[[metric_var]]), .by_group = TRUE) |>
            mutate(label_rank = row_number(), label_y = max(upper, na.rm = TRUE) + pad + gap * (n() -
                label_rank)) |>
            ungroup()
    }

    plot_native_metric_curve <- function(device_data, summary_tbl, y_label, caption = NULL, show_letters = FALSE,
        title = NULL) {
        plot_tbl <- if (isTRUE(show_letters))
            make_label_positions(summary_tbl) else summary_tbl
        y_values <- c(device_data$value, summary_tbl$lower, summary_tbl$upper)
        if (isTRUE(show_letters))
            y_values <- c(y_values, plot_tbl$label_y)
        y_top <- max(y_values, na.rm = TRUE)
        y_bottom <- min(y_values, na.rm = TRUE)
        if (!is.finite(y_top) || y_top <= 0)
            y_top <- 1
        if (!is.finite(y_bottom) || y_bottom >= 0)
            y_bottom <- 0
        y_pad <- diff(range(c(y_bottom, y_top), na.rm = TRUE)) * ifelse(isTRUE(show_letters), 0.06, 0.08)
        if (!is.finite(y_pad) || y_pad <= 0)
            y_pad <- y_top * 0.08

        p <- ggplot() +
            geom_point(data = device_data, aes(x = rpm_numeric, y = value, colour = instrument),
                position = position_jitter(width = 4, height = 0, seed = 270626), size = 1.35, alpha = 0.32,
                show.legend = FALSE) +
            geom_ribbon(data = summary_tbl, aes(x = rpm_numeric, ymin = lower,
                ymax = upper, fill = instrument, group = instrument), alpha = 0.12, colour = NA) +
            geom_errorbar(data = summary_tbl,
                aes(x = rpm_numeric, ymin = lower, ymax = upper, colour = instrument, group = instrument),
                width = 8, linewidth = 0.42, alpha = 0.78) +
            geom_line(data = summary_tbl, aes(x = rpm_numeric,
                y = mean, colour = instrument, group = instrument), linewidth = 0.58) +
            geom_point(data = summary_tbl,
                aes(x = rpm_numeric, y = mean, fill = instrument), shape = 21, size = 2.45, stroke = 0.45,
                colour = "black") +
            scale_colour_manual(values = device_cols, name = "Instrument", drop = FALSE) +
            scale_fill_manual(values = device_cols, name = "Instrument", drop = FALSE) +
            scale_x_continuous(breaks = rpm_breaks,
                limits = rpm_xlim, expand = expansion(mult = c(0, 0))) +
            labs(title = title, x = "Orbital shaker speed (RPM)",
                y = y_label, caption = caption) +
            guides(colour = guide_legend(override.aes = list(linewidth = 0.7,
                alpha = 1, size = 2.8), nrow = 1), fill = "none") +
            theme_native(12) +
            theme(legend.position = "bottom",
                plot.caption = element_text(size = 10.4), axis.title = element_text(size = 12.6, face = "bold"))

        if (isTRUE(show_letters)) {
            p <- p +
                geom_text(data = plot_tbl, aes(x = rpm_numeric, y = label_y, label = letters, colour = instrument),
                    fontface = "bold", size = 3.4, show.legend = FALSE) +
                coord_cartesian(ylim = c(y_bottom,
                    y_top + y_pad), clip = "off")
        } else {
            p <- p +
                coord_cartesian(ylim = c(y_bottom, y_top + y_pad), clip = "off")
        }

        p
    }

    plot_cld_panel <- function(letters_tbl, summary_tbl, panel_title) {
        cld_tbl <- letters_tbl |>
            left_join(summary_tbl |>
                select(rpm_numeric, instrument, mean), by = c("rpm_numeric", "instrument")) |>
            mutate(instrument = factor(as.character(instrument), levels = rev(device_order)))

        ggplot(cld_tbl, aes(x = rpm_numeric, y = instrument)) +
            geom_tile(width = 50, height = 0.92,
                fill = "white", colour = "grey82", linewidth = 0.28) +
            geom_text(aes(label = letters, colour = instrument),
                fontface = "bold", size = 3.8) +
            scale_x_continuous(breaks = rpm_breaks, limits = rpm_xlim,
                expand = expansion(mult = c(0, 0))) +
            scale_colour_manual(values = device_cols, guide = "none") +
            labs(title = panel_title, x = "Orbital shaker speed (RPM)", y = NULL, caption = "Shared letters indicate no significant Holm-adjusted pairwise difference within RPM; 'a' denotes the lowest-error group.") +
            theme_native(10) +
            theme(legend.position = "none", plot.title = element_text(face = "bold",
                size = 11.2, hjust = 0), plot.caption = element_text(size = 8.6, hjust = 0, colour = "grey25"),
                axis.title.x = element_text(face = "bold", size = 10.5), axis.text.x = element_text(size = 9.3),
                axis.text.y = element_text(size = 9.5), plot.margin = margin(2, 12, 5, 8))
    }

    plot_pairwise_heatmap <- function(pairwise_tbl, title) {
        heat_tbl <- pairwise_tbl |>
            mutate(rpm_factor_plot = factor(rpm_numeric, levels = sort(unique(rpm_numeric))), contrast_label = factor(contrast_label,
                levels = rev(unique(contrast_label))), sig_group = factor(significance, levels = c("p < 0.001",
                "p < 0.01", "p < 0.05", "p >= 0.05", "Not estimable")), p_short = case_when(p_value <
                0.001 ~ "<.001", is.finite(p_value) ~ sprintf("%.3f", p_value), TRUE ~ ""))

        ggplot(heat_tbl, aes(x = rpm_factor_plot, y = contrast_label, fill = sig_group)) +
            geom_tile(colour = "white",
                linewidth = 0.28) +
            geom_text(aes(label = p_short), size = 2.8, colour = "grey10") +
            scale_fill_manual(values = c(`p < 0.001` = "#2166AC",
                `p < 0.01` = "#67A9CF", `p < 0.05` = "#D1E5F0", `p >= 0.05` = "grey92", `Not estimable` = "grey70"),
                drop = FALSE, name = "Holm-adjusted p") +
            labs(title = title, x = "Orbital shaker speed (RPM)",
                y = "Instrument contrast", caption = "Cells show Holm-adjusted pairwise p values within each RPM-specific instrument-comparison family.") +
            theme_native(10) +
            theme(legend.position = "bottom", panel.grid = element_blank(), plot.title = element_text(face = "bold",
                size = 12), axis.text.x = element_text(size = 9.5), axis.text.y = element_text(size = 9.5))
    }

    plot_native_repeatability <- function(device_data) {
        rep_tbl <- device_data |>
            group_by(instrument, device_uid, rpm_numeric, rpm_factor) |>
            summarise(mean_vm = mean(vm_mean, na.rm = TRUE), sd_vm = sd(vm_mean_trial, na.rm = TRUE),
                cv_vm_percent = 100 * sd_vm/mean_vm, n_trials = first(n_trials), .groups = "drop") |>
            filter(is.finite(cv_vm_percent), n_trials >= 2)

        rep_summary <- rep_tbl |>
            group_by(instrument, rpm_numeric, rpm_factor) |>
            summarise(median = median(cv_vm_percent, na.rm = TRUE), q1 = quantile(cv_vm_percent, 0.25,
                na.rm = TRUE), q3 = quantile(cv_vm_percent, 0.75, na.rm = TRUE), n_devices = n_distinct(device_uid),
                .groups = "drop")

        write_csv_base(rep_tbl, file.path(tab_dir, "native_repeatability_device_level.csv"))
        write_csv_base(rep_summary, file.path(tab_dir, "native_repeatability_summary.csv"))

        p <- ggplot(rep_summary, aes(x = rpm_numeric, y = median, colour = instrument, fill = instrument,
                group = instrument)) +
            geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.12, colour = NA) +
            geom_line(linewidth = 0.55) +
            geom_point(shape = 21, size = 2.25, stroke = 0.4, colour = "black") +
            scale_colour_manual(values = device_cols, name = "Instrument", drop = FALSE) +
            scale_fill_manual(values = device_cols,
                name = "Instrument", drop = FALSE) +
            scale_x_continuous(breaks = rpm_breaks, limits = rpm_xlim,
                expand = expansion(mult = c(0, 0))) +
            labs(x = "Orbital shaker speed (RPM)", y = "Within-device CV of stage mean VM across trials (%)",
                caption = "Lines are median device-level CVs; ribbons show the interquartile range across n = 15 devices per instrument.") +
            theme_native(11) +
            guides(fill = "none")

        save_plot_set(p, "FigureS_native_repeatability_cv", width = 8.2, height = 5.3, dir = supp_fig_dir)
        invisible(list(device = rep_tbl, summary = rep_summary, plot = p))
    }

    # Data preparation ----

    # Read stored dynamic outcome masters ----
    stage_master <- readRDS(file.path(analysis_root, "dynamic/dynamic_primary_stages.rds"))
    overall_master <- readRDS(file.path(analysis_root, "dynamic/dynamic_primary_overall.rds"))

    native_stage <- stage_master |>
        filter(pipeline == native_pipeline) |>
        mutate(instrument = recode(as.character(instrument), SENS = "SENS motion", `SENs motion` = "SENS motion"),
            instrument = factor(instrument, levels = device_order), rpm = rpm.x, rpm_numeric = as.numeric(rpm),
            rpm_factor = factor(rpm_numeric, levels = sort(unique(rpm_numeric))), id = factor(id), trial_key = as.character(trial_start_utc),
            trial = factor(trial_key), device_uid = interaction(instrument, id, drop = TRUE), trial_uid = interaction(instrument,
                id, trial, drop = TRUE), abs_vm_bias_vs_reference = abs(vm_bias_vs_reference), log_linearity_error = log1p(linearity_error_vs_reference),
            log_abs_vm_bias = log1p(abs_vm_bias_vs_reference)) |>
        filter(!is.na(instrument), instrument %in% device_order, is.finite(rpm_numeric))

    native_overall <- overall_master |>
        filter(pipeline == native_pipeline) |>
        mutate(instrument = recode(as.character(instrument), SENS = "SENS motion", `SENs motion` = "SENS motion"),
            instrument = factor(instrument, levels = device_order), id = factor(id), trial_key = as.character(trial_start_utc),
            trial = factor(trial_key), device_uid = interaction(instrument, id, drop = TRUE)) |>
        filter(!is.na(instrument), instrument %in% device_order)

    device_level <- native_stage |>
        group_by(instrument, id, device_uid, rpm_factor, rpm_numeric) |>
        summarise(n_trials = n_distinct(trial_uid), linearity_error_vs_reference = mean(linearity_error_vs_reference,
            na.rm = TRUE), abs_vm_bias_vs_reference = mean(abs_vm_bias_vs_reference, na.rm = TRUE), vm_bias_vs_reference = mean(vm_bias_vs_reference,
            na.rm = TRUE), vm_mean = mean(vm_mean, na.rm = TRUE), vm_mean_trial = list(vm_mean), scale_factor_error = mean(scale_factor_error,
            na.rm = TRUE), vm_standard_deviation = mean(vm_standard_deviation, na.rm = TRUE), snr_time_domain = mean(snr_time_domain,
            na.rm = TRUE), snr_frequency_domain = mean(snr_frequency_domain, na.rm = TRUE), dominant_frequency = mean(dominant_frequency,
            na.rm = TRUE), .groups = "drop") |>
        mutate(vm_mean_trial = vapply(vm_mean_trial, function(x) paste(signif(x, 8), collapse = ";"),
            character(1)))

    repeatability_source <- native_stage |>
        group_by(instrument, id, device_uid, rpm_factor, rpm_numeric) |>
        summarise(n_trials = n_distinct(trial_uid), mean_vm = mean(vm_mean, na.rm = TRUE), sd_vm = sd(vm_mean,
            na.rm = TRUE), cv_vm_percent = 100 * sd_vm/mean_vm, .groups = "drop")

    stage_trial_profile <- native_stage |>
        group_by(instrument, id, device_uid, trial_key) |>
        summarise(n_rpm = n_distinct(rpm_numeric), mean_linearity_error_pct_fsr_vm = mean(linearity_error_vs_reference,
            na.rm = TRUE), mean_abs_vm_bias_g = mean(abs(vm_bias_vs_reference), na.rm = TRUE), .groups = "drop")

    repeatability_unit_profile <- repeatability_source |>
        group_by(instrument, id, device_uid) |>
        summarise(n_rpm = n_distinct(rpm_numeric), repeatability_cv_percent = median(cv_vm_percent, na.rm = TRUE),
            .groups = "drop")

    native_dynamic_profile_trials <- native_overall |>
        select(instrument, id, device_uid, trial_key, response_slope, scale_factor_error, composite_error) |>
        mutate(scale_factor_error_percent = 100 * scale_factor_error) |>
        left_join(stage_trial_profile, by = c("instrument", "id", "device_uid", "trial_key"))

    audit_stage <- native_stage |>
        group_by(instrument, rpm_numeric) |>
        summarise(n_stage_rows = n(), n_devices = n_distinct(device_uid), n_trials = n_distinct(trial_uid),
            .groups = "drop")

    audit_instrument <- native_stage |>
        group_by(instrument) |>
        summarise(n_stage_rows = n(), n_devices = n_distinct(device_uid), n_trials = n_distinct(trial_uid),
            n_rpm = n_distinct(rpm_numeric), .groups = "drop")

    write_csv_base(audit_stage, file.path(tab_dir, "native_stage_audit_by_rpm.csv"))
    write_csv_base(audit_instrument, file.path(tab_dir, "native_stage_audit_by_instrument.csv"))
    write_csv_base(device_level, file.path(tab_dir, "native_device_level_means.csv"))
    write_csv_base(native_overall, file.path(tab_dir, "native_overall_rows.csv"))
    write_csv_base(repeatability_source, file.path(tab_dir, "native_repeatability_device_level.csv"))
    write_csv_base(native_dynamic_profile_trials, file.path(tab_dir, "native_dynamic_profile_trial_level.csv"))
    write_csv_base(repeatability_unit_profile, file.path(tab_dir, "native_repeatability_unit_level_for_profile.csv"))

    overall_metric_specs <- tibble::tribble(~metric_var, ~metric_label, ~digits, ~lower_better, ~source,
        "mean_linearity_error_pct_fsr_vm", "Mean VM integral non-linearity, % FSR_VM", 2L, TRUE, "trial",
        "mean_abs_vm_bias_g", "Mean absolute VM bias, g", 3L, TRUE, "trial", "response_slope", "Mean response slope",
        4L, TRUE, "trial", "scale_factor_error_percent", "Signed scale factor error, %", 2L, TRUE, "trial",
        "composite_error", "VM composite error, % 1/2 FSR_VM", 2L, TRUE, "trial", "repeatability_cv_percent",
        "Trial-to-trial VM repeatability CV, %", 3L, TRUE, "unit")

    overall_metric_results <- lapply(seq_len(nrow(overall_metric_specs)), function(i) {
        games_howell_metric(if (overall_metric_specs$source[i] == "unit")
            repeatability_unit_profile else native_dynamic_profile_trials, metric_var = overall_metric_specs$metric_var[i], metric_label = overall_metric_specs$metric_label[i],
            lower_better = overall_metric_specs$lower_better[i])
    })

    native_dynamic_profile_long <- bind_rows(lapply(overall_metric_results, `[[`, "summary")) |>
        left_join(overall_metric_specs, by = c(metric = "metric_label")) |>
        mutate(mean_ci_letters = mapply(fmt_estimate_letters, mean, lower, upper, letters, digits), mean_sd_text = sprintf(paste0("%.",
            digits, "f +/- %.", digits, "f"), mean, sd), median_iqr_text = sprintf(paste0("%.", digits,
            "f [%.", digits, "f, %.", digits, "f]"), median, q1, q3), range_text = sprintf(paste0("%.",
            digits, "f to %.", digits, "f"), min, max)) |>
        select(metric, metric_var, instrument, n, mean, sd, lower, upper, letters, mean_ci_letters, mean_sd_text,
            median, q1, q3, median_iqr_text, min, max, range_text)

    native_dynamic_profile_tests <- bind_rows(lapply(overall_metric_results, `[[`, "test"))
    native_dynamic_profile_pairwise <- bind_rows(lapply(overall_metric_results, `[[`, "pairwise"))

    n_trials_lookup <- native_dynamic_profile_trials |>
        count(instrument, name = "n_trials")
    n_units_lookup <- repeatability_unit_profile |>
        count(instrument, name = "n_units")

    native_dynamic_profile_wide <- native_dynamic_profile_long |>
        select(instrument, metric, mean_ci_letters) |>
        tidyr::pivot_wider(names_from = metric, values_from = mean_ci_letters) |>
        arrange(match(instrument, device_order)) |>
        left_join(n_trials_lookup, by = "instrument") |>
        left_join(n_units_lookup, by = "instrument") |>
        select(instrument, n_trials, n_units, everything())

    write_csv_base(native_dynamic_profile_wide, file.path(tab_dir, "native_dynamic_profile_manuscript_table.csv"))
    write_csv_base(native_dynamic_profile_long, file.path(tab_dir, "native_dynamic_profile_long_summary.csv"))
    write_csv_base(native_dynamic_profile_tests, file.path(tab_dir, "native_dynamic_profile_welch_tests.csv"))
    write_csv_base(native_dynamic_profile_pairwise, file.path(tab_dir, "native_dynamic_profile_games_howell_pairwise.csv"))

    # Primary native analyses ----

    linearity <- run_native_metric_analysis(stage_data = native_stage, device_data = device_level, metric_var = "linearity_error_vs_reference",
        model_var = "log_linearity_error", metric_slug = "linearity", metric_name = "Native VM integral non-linearity",
        y_label = expression("VM integral non-linearity (% FSR"["VM"] * ")"), lower_better = TRUE)

    bias <- run_native_metric_analysis(stage_data = native_stage, device_data = device_level, metric_var = "abs_vm_bias_vs_reference",
        model_var = "log_abs_vm_bias", metric_slug = "absolute_vm_bias", metric_name = "Native absolute VM bias",
        y_label = expression("Absolute VM bias vs reference (" * italic(g) * ")"), lower_better = TRUE)

    write_metric_outputs <- function(analysis) {
        slug <- analysis$metric_slug
        write_csv_base(analysis$lrt, file.path(tab_dir, paste0("native_", slug, "_interaction_lrt.csv")))
        write_csv_base(analysis$emm, file.path(tab_dir, paste0("native_", slug, "_emmeans_by_rpm.csv")))
        write_csv_base(analysis$pairwise, file.path(tab_dir, paste0("native_", slug, "_instrument_pairwise_by_rpm.csv")))
        write_csv_base(analysis$letters, file.path(tab_dir, paste0("native_", slug, "_compact_letters_by_rpm.csv")))
        write_csv_base(analysis$observed_summary, file.path(tab_dir, paste0("native_", slug, "_device_summary_with_letters.csv")))
    }

    write_metric_outputs(linearity)
    write_metric_outputs(bias)

    # Main and supplementary figures ----

    linearity_device_plot_data <- device_level |>
        transmute(instrument, device_uid, rpm_numeric, rpm_factor, value = linearity_error_vs_reference)

    bias_device_plot_data <- device_level |>
        transmute(instrument, device_uid, rpm_numeric, rpm_factor, value = abs_vm_bias_vs_reference)

    linearity_caption <- "Faint points are device-level means; lines, whiskers, and ribbons show instrument means and 95% CIs across n = 15 devices. Letters compare instruments within RPM."

    bias_caption <- "Faint points are device-level means; lines, whiskers, and ribbons show instrument means and 95% CIs across n = 15 devices. Letters compare instruments within RPM."

    p_linearity_clean <- plot_native_metric_curve(linearity_device_plot_data, linearity$observed_summary,
        linearity$y_label, caption = "Faint points are device-level means across three repeated trials; lines, whiskers, and ribbons show instrument means and 95% CIs across n = 15 devices.",
        show_letters = FALSE)

    p_linearity_letters <- plot_native_metric_curve(linearity_device_plot_data, linearity$observed_summary,
        linearity$y_label, caption = linearity_caption, show_letters = TRUE)

    p_linearity_cld <- plot_cld_panel(linearity$letters, linearity$observed_summary, "B. RPM-specific instrument groupings for VM non-linearity")

    p_bias_letters <- plot_native_metric_curve(bias_device_plot_data, bias$observed_summary, bias$y_label,
        caption = bias_caption, show_letters = TRUE)

    p_bias_clean <- plot_native_metric_curve(bias_device_plot_data, bias$observed_summary, bias$y_label,
        caption = "Faint points are device-level means across three repeated trials; lines, whiskers, and ribbons show instrument means and 95% CIs across n = 15 devices.",
        show_letters = FALSE)

    p_bias_cld <- plot_cld_panel(bias$letters, bias$observed_summary, "B. RPM-specific instrument groupings for absolute VM bias")

    p_linearity_heatmap <- plot_pairwise_heatmap(linearity$pairwise, "RPM-specific pairwise instrument contrasts for native VM non-linearity")

    p_bias_heatmap <- plot_pairwise_heatmap(bias$pairwise, "RPM-specific pairwise instrument contrasts for native absolute VM bias")

    save_plot_set(p_linearity_clean, "Figure1_native_linearity_estimation_clean", width = 8.8, height = 5.8)
    save_plot_set(p_linearity_letters, "Figure1_native_linearity_estimation_posthoc_letters", width = 9.2,
        height = 6.2)
    save_combined_plot_set(p_linearity_clean +
        labs(title = "A. Native VM integral non-linearity", caption = NULL),
        p_linearity_cld, "Figure1_native_linearity_estimation_with_posthoc_panel", width = 9.2, height = 7.6)

    save_plot_set(p_bias_clean, "FigureS_native_absolute_vm_bias_clean", width = 8.8, height = 5.8, dir = supp_fig_dir)
    save_plot_set(p_bias_letters, "FigureS_native_absolute_vm_bias_posthoc_letters", width = 9.2, height = 6.2,
        dir = supp_fig_dir)
    save_combined_plot_set(p_bias_clean +
        labs(title = "A. Native absolute VM bias", caption = NULL),
        p_bias_cld, "FigureS_native_absolute_vm_bias_with_posthoc_panel", width = 9.2, height = 7.6,
        dir = supp_fig_dir)

    save_plot_set(p_linearity_heatmap, "FigureS_native_linearity_pairwise_heatmap", width = 9.4, height = 4.6,
        dir = supp_fig_dir)
    save_plot_set(p_bias_heatmap, "FigureS_native_absolute_vm_bias_pairwise_heatmap", width = 9.4, height = 4.6,
        dir = supp_fig_dir)

    # Native VM repeatability

    rep_summary <- repeatability_source |>
        filter(is.finite(cv_vm_percent), n_trials >= 2) |>
        group_by(instrument, rpm_numeric, rpm_factor) |>
        summarise(median = median(cv_vm_percent, na.rm = TRUE), q1 = quantile(cv_vm_percent, 0.25, na.rm = TRUE),
            q3 = quantile(cv_vm_percent, 0.75, na.rm = TRUE), n_devices = n_distinct(device_uid), .groups = "drop")

    write_csv_base(rep_summary, file.path(tab_dir, "native_repeatability_summary.csv"))

    p_repeatability <- ggplot(rep_summary, aes(x = rpm_numeric, y = median, colour = instrument, fill = instrument,
            group = instrument)) +
        geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.12, colour = NA) +
        geom_line(linewidth = 0.55) +
        geom_point(shape = 21, size = 2.25, stroke = 0.4, colour = "black") +
        scale_colour_manual(values = device_cols,
            name = "Instrument", drop = FALSE) +
        scale_fill_manual(values = device_cols, name = "Instrument",
            drop = FALSE) +
        scale_x_continuous(breaks = rpm_breaks, limits = rpm_xlim, expand = expansion(mult = c(0,
            0))) +
        labs(x = "Orbital shaker speed (RPM)", y = "Within-device CV of stage mean VM across trials (%)",
            caption = "Lines are median device-level CVs; ribbons show the interquartile range across n = 15 devices per instrument.") +
        theme_native(11) +
        guides(fill = "none")

    save_plot_set(p_repeatability, "FigureS_native_repeatability_cv", width = 8.2, height = 5.3, dir = supp_fig_dir)

    # Manuscript summary tables ----

    selected_rpm <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)

    manuscript_linearity_summary <- linearity$observed_summary |>
        filter(rpm_numeric %in% selected_rpm) |>
        mutate(mean_ci_text = sprintf("%.2f [%.2f, %.2f]", mean, lower, upper)) |>
        select(rpm_numeric, instrument, mean_ci_text, letters)

    manuscript_bias_summary <- bias$observed_summary |>
        filter(rpm_numeric %in% selected_rpm) |>
        mutate(mean_ci_text = sprintf("%.3f [%.3f, %.3f]", mean, lower, upper)) |>
        select(rpm_numeric, instrument, mean_ci_text, letters)

    write_csv_base(manuscript_linearity_summary, file.path(tab_dir, "native_linearity_manuscript_summary.csv"))
    write_csv_base(manuscript_bias_summary, file.path(tab_dir, "native_absolute_vm_bias_manuscript_summary.csv"))

    analysis_index <- tibble(artifact = c("Figure1_native_linearity_estimation_with_posthoc_panel", "Figure1_native_linearity_estimation_posthoc_letters",
        "Figure1_native_linearity_estimation_clean", "FigureS_native_linearity_pairwise_heatmap", "FigureS_native_absolute_vm_bias_clean",
        "FigureS_native_absolute_vm_bias_with_posthoc_panel", "FigureS_native_absolute_vm_bias_pairwise_heatmap",
        "FigureS_native_repeatability_cv"), purpose = c("Recommended main native dynamic metrology figure: clean trajectory plus RPM-specific compact-letter post hoc panel.",
        "Single-panel trajectory figure with compact letters placed over RPM-specific means.", "Clean native trajectory figure without post hoc labels.",
        "Supplementary heatmap of all RPM-specific pairwise instrument contrasts for VM non-linearity.",
        "Clean supplementary native absolute VM bias trajectory without post hoc labels.", "Supplementary native absolute VM bias figure plus compact-letter post hoc panel.",
        "Supplementary heatmap of all RPM-specific pairwise instrument contrasts for absolute VM bias.",
        "Supplementary repeatability figure for native stage mean VM."))

    write_csv_base(analysis_index, file.path(out_root, "native_unprocessed_analysis_index.csv"))

    message("Native unprocessed dynamic analysis complete.")
    message("Output folder: ", out_root)
    print(audit_instrument)
    print(linearity$lrt)
    print(bias$lrt)
    print(analysis_index)
}
