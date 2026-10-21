# Study analysis and presentation; paths supplied by config.R.
wam_configuration <- function(cfg) {
    out_root<-file.path(cfg$output,"configuration")
    tab_dir<-file.path(out_root,"tables")
    fig_dir<-file.path(out_root,"figures")
    native_tab_dir<-file.path(cfg$output,"dynamic_native/tables")
    dir.create(tab_dir,recursive=TRUE,showWarnings=FALSE)
    dir.create(fig_dir,recursive=TRUE,showWarnings=FALSE)
    required_pkgs <- c("dplyr", "tidyr", "tibble", "ggplot2")
    missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_pkgs)) {
        stop("Missing required R packages: ", paste(missing_pkgs, collapse = ", "))
    }
    
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(ggplot2)
    
    primary_instruments <- c("AX3", "GENEActiv", "LEAP", "SENS motion")
    
    config_lookup <- tibble::tribble(
        ~instrument,    ~primary_configuration,            ~secondary_configuration,
        "AX3",          "100 Hz, +/-8 g (FSR 16 g)",      "100 Hz, +/-4 g (FSR 8 g)",
        "GENEActiv",    "100 Hz, +/-8 g (FSR 16 g)",      "50 Hz, +/-8 g (FSR 16 g)",
        "LEAP",         "128 Hz, +/-8 g (FSR 16 g)",      "128 Hz, +/-8 g (FSR 16 g)",
        "SENS motion",  "25 Hz, +/-4 g (FSR 8 g)",        "25 Hz, +/-4 g (FSR 8 g)"
    )
    
    condition_levels <- c("Primary configuration", "Secondary configuration")
    condition_colours <- c("Primary configuration" = "#0072B2", "Secondary configuration" = "#D55E00")
    condition_fills <- condition_colours
    
    mean_ci <- function(x) {
        x <- x[is.finite(x)]
        n <- length(x)
        m <- mean(x)
        sdv <- stats::sd(x)
        se <- sdv / sqrt(n)
        moe <- if (n > 1) stats::qt(0.975, df = n - 1) * se else NA_real_
        tibble(n = n, mean = m, sd = sdv, se = se, ci_low = m - moe, ci_high = m + moe)
    }
    
    fmt_ci <- function(mean, low, high, digits) {
        sprintf(
            paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
            mean, low, high
        )
    }
    
    fmt_num <- function(x, digits) {
        sprintf(paste0("%.", digits, "f"), x)
    }
    
    fmt_p <- function(p) {
        dplyr::case_when(
            !is.finite(p) ~ NA_character_,
            p < 0.001 ~ "<0.001",
            TRUE ~ sprintf("%.3f", p)
        )
    }
    
    normalise_unit_id <- function(instrument, id) {
        id <- as.character(id)
        out <- id
        sens <- instrument == "SENS motion"
        out[sens] <- sub("_acc-3ax.*$", "", out[sens])
        out
    }
    
    primary_stage <- read.csv(
        file.path(native_tab_dir, "native_device_level_means.csv"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    ) |>
        filter(instrument %in% primary_instruments) |>
        transmute(
            instrument,
            id,
            unit_id = normalise_unit_id(instrument, id),
            device_uid,
            rpm = as.numeric(rpm_numeric),
            condition = "Primary configuration",
            linearity_error_vs_reference = as.numeric(linearity_error_vs_reference),
            abs_vm_bias_vs_reference = as.numeric(abs_vm_bias_vs_reference),
            source_n_trials = as.numeric(n_trials)
        )
    
    secondary_stage <- readRDS(file.path(cfg$masters,"configuration_stages.rds")) |>
        filter(instrument %in% primary_instruments) |>
        mutate(
            rpm = suppressWarnings(as.numeric(rpm)),
            abs_vm_bias_vs_reference = abs(vm_bias_vs_reference)
        ) |>
        transmute(
            instrument,
            id,
            unit_id = normalise_unit_id(instrument, id),
            device_uid,
            rpm,
            condition = "Secondary configuration",
            linearity_error_vs_reference = as.numeric(linearity_error_vs_reference),
            abs_vm_bias_vs_reference = as.numeric(abs_vm_bias_vs_reference),
            source_n_trials = 1
        )
    
    stage_points <- bind_rows(primary_stage, secondary_stage) |>
        mutate(
            condition = factor(condition, levels = condition_levels),
            instrument = factor(instrument, levels = primary_instruments)
        )
    
    stage_long_points <- stage_points |>
        pivot_longer(
            cols = c(linearity_error_vs_reference, abs_vm_bias_vs_reference),
            names_to = "metric_key",
            values_to = "value"
        ) |>
        mutate(
            metric_label = dplyr::case_when(
                metric_key == "linearity_error_vs_reference" ~ "A. VM integral\nnon-linearity\n(% FSRVM)",
                metric_key == "abs_vm_bias_vs_reference" ~ "B. Absolute\nVM bias\n(g)",
                TRUE ~ metric_key
            ),
            metric_label = factor(
                metric_label,
                levels = c("A. VM integral\nnon-linearity\n(% FSRVM)", "B. Absolute\nVM bias\n(g)")
            ),
            x_pos = rpm + ifelse(condition == "Primary configuration", -4, 4)
        )
    
    set.seed(19082025)
    stage_long_points <- stage_long_points |>
        mutate(x_jitter = x_pos + stats::runif(n(), min = -1.4, max = 1.4))
    
    stage_summary <- stage_long_points |>
        group_by(instrument, condition, metric_key, metric_label, rpm) |>
        summarise(mean_ci(value), .groups = "drop") |>
        mutate(
            x_pos = rpm + ifelse(condition == "Primary configuration", -4, 4)
        )
    
    write.csv(stage_points, file.path(tab_dir, "configuration_sensitivity_stage_points_for_figure.csv"), row.names = FALSE)
    write.csv(stage_summary, file.path(tab_dir, "configuration_sensitivity_stage_summary_for_figure.csv"), row.names = FALSE)
    
    primary_overall <- read.csv(
        file.path(native_tab_dir, "native_dynamic_profile_trial_level.csv"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    ) |>
        filter(instrument %in% primary_instruments) |>
        transmute(
            instrument,
            id,
            unit_id = normalise_unit_id(instrument, id),
            device_uid,
            trial_key,
            condition = "Primary configuration",
            mean_linearity_error_pct_fsr_vm = as.numeric(mean_linearity_error_pct_fsr_vm),
            mean_abs_vm_bias_g = as.numeric(mean_abs_vm_bias_g),
            response_slope = as.numeric(response_slope),
            scale_factor_error_percent = as.numeric(scale_factor_error_percent),
            composite_error = as.numeric(composite_error)
        )
    
    secondary_overall <- readRDS(file.path(cfg$masters,"configuration_overall.rds")) |>
        filter(instrument %in% primary_instruments) |>
        transmute(
            instrument,
            id,
            unit_id = normalise_unit_id(instrument, id),
            device_uid,
            trial_key = as.character(trial_start_utc),
            condition = "Secondary configuration",
            mean_linearity_error_pct_fsr_vm = as.numeric(mean_linearity_error_pct_fsr_vm),
            mean_abs_vm_bias_g = as.numeric(mean_abs_vm_bias_g),
            response_slope = as.numeric(response_slope),
            scale_factor_error_percent = as.numeric(scale_factor_error_percent),
            composite_error = as.numeric(composite_error)
        )
    
    primary_overall_device <- primary_overall |>
        group_by(instrument, unit_id) |>
        summarise(
            id = first(id),
            device_uid = first(device_uid),
            condition = "Primary configuration",
            source_n_trials = n_distinct(trial_key),
            mean_linearity_error_pct_fsr_vm = mean(mean_linearity_error_pct_fsr_vm, na.rm = TRUE),
            mean_abs_vm_bias_g = mean(mean_abs_vm_bias_g, na.rm = TRUE),
            response_slope = mean(response_slope, na.rm = TRUE),
            scale_factor_error_percent = mean(scale_factor_error_percent, na.rm = TRUE),
            composite_error = mean(composite_error, na.rm = TRUE),
            .groups = "drop"
        )
    
    secondary_overall_device <- secondary_overall |>
        group_by(instrument, unit_id) |>
        summarise(
            id = first(id),
            device_uid = first(device_uid),
            condition = "Secondary configuration",
            source_n_trials = n_distinct(trial_key),
            mean_linearity_error_pct_fsr_vm = mean(mean_linearity_error_pct_fsr_vm, na.rm = TRUE),
            mean_abs_vm_bias_g = mean(mean_abs_vm_bias_g, na.rm = TRUE),
            response_slope = mean(response_slope, na.rm = TRUE),
            scale_factor_error_percent = mean(scale_factor_error_percent, na.rm = TRUE),
            composite_error = mean(composite_error, na.rm = TRUE),
            .groups = "drop"
        )
    
    overall_long <- bind_rows(primary_overall_device, secondary_overall_device) |>
        mutate(condition = factor(condition, levels = condition_levels)) |>
        pivot_longer(
            cols = c(
                mean_linearity_error_pct_fsr_vm,
                mean_abs_vm_bias_g,
                response_slope,
                scale_factor_error_percent,
                composite_error
            ),
            names_to = "metric_key",
            values_to = "value"
        )
    
    metric_meta <- tibble::tribble(
        ~metric_key,                           ~metric_label,                                      ~digits,
        "mean_linearity_error_pct_fsr_vm",     "Mean VM integral non-linearity, % FSRVM",          2,
        "mean_abs_vm_bias_g",                  "Mean absolute VM bias, g",                         3,
        "response_slope",                      "Mean response slope",                              4,
        "scale_factor_error_percent",          "Signed scale factor error, %",                     2,
        "composite_error",                     "VM composite error, % 1/2 FSRVM",                  2
    )
    
    paired_test_rows <- overall_long |>
        select(instrument, unit_id, condition, metric_key, value) |>
        tidyr::pivot_wider(names_from = condition, values_from = value) |>
        filter(is.finite(`Primary configuration`), is.finite(`Secondary configuration`)) |>
        group_by(instrument, metric_key) |>
        group_modify(function(.x, .y) {
            diff <- .x$`Secondary configuration` - .x$`Primary configuration`
            n <- length(diff)
            mean_diff <- mean(diff, na.rm = TRUE)
            sd_diff <- stats::sd(diff, na.rm = TRUE)
            p_t <- NA_real_
            p_w <- NA_real_
            if (n >= 3 && is.finite(sd_diff) && sd_diff > 0) {
                p_t <- tryCatch(
                    stats::t.test(.x$`Secondary configuration`, .x$`Primary configuration`, paired = TRUE)$p.value,
                    error = function(e) NA_real_
                )
                p_w <- tryCatch(
                    stats::wilcox.test(.x$`Secondary configuration`, .x$`Primary configuration`, paired = TRUE, exact = FALSE)$p.value,
                    error = function(e) NA_real_
                )
            }
            tibble(
                n_paired_units = n,
                mean_paired_difference = mean_diff,
                sd_paired_difference = sd_diff,
                paired_t_p = p_t,
                wilcoxon_p = p_w
            )
        }) |>
        ungroup() |>
        group_by(metric_key) |>
        mutate(paired_t_p_holm_within_outcome = p.adjust(paired_t_p, method = "holm")) |>
        ungroup() |>
        mutate(paired_t_p_holm_all = p.adjust(paired_t_p, method = "holm")) |>
        left_join(metric_meta, by = "metric_key")
    
    overall_summary_long <- overall_long |>
        group_by(instrument, condition, metric_key) |>
        summarise(mean_ci(value), .groups = "drop") |>
        left_join(metric_meta, by = "metric_key") |>
        mutate(
            formatted = fmt_ci(mean, ci_low, ci_high, digits)
        )
    
    overall_delta <- overall_summary_long |>
        select(instrument, condition, metric_key, mean) |>
        tidyr::pivot_wider(names_from = condition, values_from = mean) |>
        mutate(
            delta_secondary_minus_primary = `Secondary configuration` - `Primary configuration`,
            percent_change_vs_primary = 100 * delta_secondary_minus_primary / abs(`Primary configuration`)
        )
    
    paired_test_rows <- paired_test_rows |>
        mutate(
            paired_t_p_formatted = fmt_p(paired_t_p),
            paired_t_p_holm_within_outcome_formatted = fmt_p(paired_t_p_holm_within_outcome),
            paired_t_p_holm_all_formatted = fmt_p(paired_t_p_holm_all),
            wilcoxon_p_formatted = fmt_p(wilcoxon_p)
        )
    
    write.csv(paired_test_rows, file.path(tab_dir, "configuration_sensitivity_paired_tests_long.csv"), row.names = FALSE)
    write.csv(overall_summary_long, file.path(tab_dir, "configuration_sensitivity_overall_summary_long.csv"), row.names = FALSE)
    write.csv(overall_delta, file.path(tab_dir, "configuration_sensitivity_overall_delta_long.csv"), row.names = FALSE)
    
    summary_wide <- overall_summary_long |>
        select(instrument, condition, metric_key, n, formatted) |>
        mutate(
            condition_key = ifelse(condition == "Primary configuration", "primary", "secondary"),
            value_col = paste(metric_key, condition_key, "mean_ci", sep = "_")
        ) |>
        select(instrument, value_col, formatted) |>
        pivot_wider(names_from = value_col, values_from = formatted)
    
    n_wide <- overall_summary_long |>
        select(instrument, condition, n) |>
        distinct() |>
        mutate(condition_key = ifelse(condition == "Primary configuration", "primary_n", "secondary_n")) |>
        select(instrument, condition_key, n) |>
        pivot_wider(names_from = condition_key, values_from = n)
    
    delta_wide <- overall_delta |>
        left_join(metric_meta, by = "metric_key") |>
        mutate(
            delta_col = paste(metric_key, "secondary_minus_primary", sep = "_"),
            delta_formatted = dplyr::case_when(
                digits == 4 ~ fmt_num(delta_secondary_minus_primary, 4),
                digits == 3 ~ fmt_num(delta_secondary_minus_primary, 3),
                TRUE ~ fmt_num(delta_secondary_minus_primary, 2)
            ),
            pct_col = paste(metric_key, "percent_change_vs_primary", sep = "_"),
            pct_formatted = fmt_num(percent_change_vs_primary, 1)
        )
    
    delta_value_wide <- delta_wide |>
        select(instrument, delta_col, delta_formatted) |>
        pivot_wider(names_from = delta_col, values_from = delta_formatted)
    
    delta_pct_wide <- delta_wide |>
        select(instrument, pct_col, pct_formatted) |>
        pivot_wider(names_from = pct_col, values_from = pct_formatted)
    
    p_wide <- paired_test_rows |>
        mutate(p_col = paste(metric_key, "paired_t_p", sep = "_")) |>
        select(instrument, p_col, paired_t_p_formatted) |>
        pivot_wider(names_from = p_col, values_from = paired_t_p_formatted)
    
    p_holm_wide <- paired_test_rows |>
        mutate(p_col = paste(metric_key, "paired_t_p_holm", sep = "_")) |>
        select(instrument, p_col, paired_t_p_holm_within_outcome_formatted) |>
        pivot_wider(names_from = p_col, values_from = paired_t_p_holm_within_outcome_formatted)
    
    n_paired_wide <- paired_test_rows |>
        select(instrument, n_paired_units) |>
        distinct()
    
    manuscript_table <- config_lookup |>
        left_join(n_wide, by = "instrument") |>
        left_join(n_paired_wide, by = "instrument") |>
        left_join(summary_wide, by = "instrument") |>
        left_join(delta_value_wide, by = "instrument") |>
        left_join(delta_pct_wide, by = "instrument") |>
        left_join(p_wide, by = "instrument") |>
        left_join(p_holm_wide, by = "instrument")
    
    write.csv(manuscript_table, file.path(tab_dir, "configuration_sensitivity_manuscript_table.csv"), row.names = FALSE)
    
    formatted_table <- manuscript_table |>
        transmute(
            Instrument = instrument,
            `Primary configuration` = primary_configuration,
            `Secondary configuration` = secondary_configuration,
            `n primary units` = primary_n,
            `n secondary units` = secondary_n,
            `n paired units` = n_paired_units,
            `Mean VM integral non-linearity, % FSRVM (primary)` = mean_linearity_error_pct_fsr_vm_primary_mean_ci,
            `Mean VM integral non-linearity, % FSRVM (secondary)` = mean_linearity_error_pct_fsr_vm_secondary_mean_ci,
            `Non-linearity delta` = mean_linearity_error_pct_fsr_vm_secondary_minus_primary,
            `Non-linearity paired P` = mean_linearity_error_pct_fsr_vm_paired_t_p,
            `Non-linearity Holm P` = mean_linearity_error_pct_fsr_vm_paired_t_p_holm,
            `Mean absolute VM bias, g (primary)` = mean_abs_vm_bias_g_primary_mean_ci,
            `Mean absolute VM bias, g (secondary)` = mean_abs_vm_bias_g_secondary_mean_ci,
            `Bias delta` = mean_abs_vm_bias_g_secondary_minus_primary,
            `Bias paired P` = mean_abs_vm_bias_g_paired_t_p,
            `Bias Holm P` = mean_abs_vm_bias_g_paired_t_p_holm,
            `Mean response slope (primary)` = response_slope_primary_mean_ci,
            `Mean response slope (secondary)` = response_slope_secondary_mean_ci,
            `Response slope delta` = response_slope_secondary_minus_primary,
            `Response slope paired P` = response_slope_paired_t_p,
            `Response slope Holm P` = response_slope_paired_t_p_holm,
            `Signed scale factor error, % (primary)` = scale_factor_error_percent_primary_mean_ci,
            `Signed scale factor error, % (secondary)` = scale_factor_error_percent_secondary_mean_ci,
            `Scale factor error delta` = scale_factor_error_percent_secondary_minus_primary,
            `Scale factor paired P` = scale_factor_error_percent_paired_t_p,
            `Scale factor Holm P` = scale_factor_error_percent_paired_t_p_holm,
            `VM composite error, % 1/2 FSRVM (primary)` = composite_error_primary_mean_ci,
            `VM composite error, % 1/2 FSRVM (secondary)` = composite_error_secondary_mean_ci,
            `Composite error delta` = composite_error_secondary_minus_primary,
            `Composite error paired P` = composite_error_paired_t_p,
            `Composite error Holm P` = composite_error_paired_t_p_holm
        )
    
    write.csv(formatted_table, file.path(tab_dir, "configuration_sensitivity_manuscript_table_formatted.csv"), row.names = FALSE)
    
    compact_table_with_p <- overall_summary_long |>
        select(instrument, condition, metric_key, metric_label, digits, formatted) |>
        pivot_wider(names_from = condition, values_from = formatted) |>
        left_join(
            overall_delta |>
                select(instrument, metric_key, delta_secondary_minus_primary),
            by = c("instrument", "metric_key")
        ) |>
        left_join(
            paired_test_rows |>
                select(
                    instrument,
                    metric_key,
                    n_paired_units,
                    paired_t_p_formatted,
                    paired_t_p_holm_within_outcome_formatted,
                    wilcoxon_p_formatted
                ),
            by = c("instrument", "metric_key")
        ) |>
        mutate(
            delta_formatted = dplyr::case_when(
                digits == 4 ~ fmt_num(delta_secondary_minus_primary, 4),
                digits == 3 ~ fmt_num(delta_secondary_minus_primary, 3),
                TRUE ~ fmt_num(delta_secondary_minus_primary, 2)
            )
        ) |>
        arrange(factor(instrument, levels = primary_instruments), match(metric_key, metric_meta$metric_key)) |>
        transmute(
            Instrument = instrument,
            Outcome = metric_label,
            `Primary configuration` = `Primary configuration`,
            `Secondary configuration` = `Secondary configuration`,
            `Secondary - primary` = delta_formatted,
            `n paired units` = n_paired_units,
            `Paired t-test P` = paired_t_p_formatted,
            `Holm P within outcome` = paired_t_p_holm_within_outcome_formatted,
            `Wilcoxon P` = wilcoxon_p_formatted
        )
    
    write.csv(compact_table_with_p, file.path(tab_dir, "configuration_sensitivity_compact_table_with_p.csv"), row.names = FALSE)
    
    summary_500 <- stage_summary |>
        filter(rpm == 500, metric_key == "linearity_error_vs_reference") |>
        select(instrument, condition, n, mean, ci_low, ci_high) |>
        tidyr::pivot_wider(
            names_from = condition,
            values_from = c(n, mean, ci_low, ci_high)
        )
    write.csv(summary_500, file.path(tab_dir, "configuration_sensitivity_500rpm_linearity_summary.csv"), row.names = FALSE)
    
    fig_data_summary <- stage_summary |>
        mutate(
            instrument = factor(instrument, levels = primary_instruments),
            condition = factor(condition, levels = condition_levels)
        )
    
    fig_data_points <- stage_long_points |>
        mutate(
            instrument = factor(instrument, levels = primary_instruments),
            condition = factor(condition, levels = condition_levels)
        )
    
    p <- ggplot() +
        geom_ribbon(
            data = fig_data_summary,
            aes(x = rpm, ymin = ci_low, ymax = ci_high, fill = condition, group = condition),
            alpha = 0.12,
            colour = NA
        ) +
        geom_line(
            data = fig_data_summary,
            aes(x = x_pos, y = mean, colour = condition, group = condition, linetype = condition),
            linewidth = 0.75
        ) +
        geom_errorbar(
            data = fig_data_summary,
            aes(x = x_pos, ymin = ci_low, ymax = ci_high, colour = condition),
            width = 0,
            linewidth = 0.55
        ) +
        geom_point(
            data = fig_data_points,
            aes(x = x_jitter, y = value, colour = condition),
            alpha = 0.22,
            size = 1.15,
            stroke = 0
        ) +
        geom_point(
            data = fig_data_summary,
            aes(x = x_pos, y = mean, fill = condition),
            shape = 21,
            colour = "black",
            size = 2.2,
            stroke = 0.45
        ) +
        facet_grid(metric_label ~ instrument, scales = "free_y", switch = "y") +
        scale_colour_manual(values = condition_colours, name = "Configuration") +
        scale_fill_manual(values = condition_fills, name = "Configuration") +
        scale_linetype_manual(values = c("Primary configuration" = "solid", "Secondary configuration" = "22"), name = "Configuration") +
        scale_x_continuous(
            breaks = seq(50, 500, by = 100),
            minor_breaks = seq(50, 500, by = 50),
            limits = c(30, 520),
            expand = expansion(mult = c(0.01, 0.01))
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.16))) +
        labs(x = "Orbital shaker speed (RPM)", y = NULL) +
        guides(
            colour = guide_legend(override.aes = list(alpha = 1, size = 2.8, linewidth = 1.0)),
            fill = "none",
            linetype = "none"
        ) +
        theme_bw(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.35),
            panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.35),
            strip.background = element_rect(fill = "grey92", colour = "grey55", linewidth = 0.5),
            strip.background.y = element_rect(fill = "grey92", colour = "grey55", linewidth = 0.5),
            strip.placement = "outside",
            strip.text.x = element_text(face = "bold", size = 11),
            strip.text.y.left = element_text(face = "bold", size = 10.5, angle = 0, lineheight = 0.95, margin = margin(r = 6, l = 6)),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", margin = margin(t = 14)),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9),
            panel.spacing.x = unit(0.8, "lines"),
            panel.spacing.y = unit(0.9, "lines"),
            plot.margin = margin(10, 16, 10, 18)
        )
    
    ggsave(
        filename = file.path(fig_dir, "FigureS_configuration_sensitivity_linearity_bias_300dpi.png"),
        plot = p,
        width = 15,
        height = 7.8,
        dpi = 300,
        bg = "white"
    )
    
    ggsave(
        filename = file.path(fig_dir, "FigureS_configuration_sensitivity_linearity_bias_600dpi.png"),
        plot = p,
        width = 15,
        height = 7.8,
        dpi = 600,
        bg = "white"
    )
    
    ggsave(
        filename = file.path(fig_dir, "FigureS_configuration_sensitivity_linearity_bias.pdf"),
        plot = p,
        width = 15,
        height = 7.8,
        bg = "white"
    )
    
}
