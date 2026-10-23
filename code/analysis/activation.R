# Low-excitation response and activation ----
# Input: low-excitation master observations.
# Output: stage and threshold summaries.

wam_activation <- function(cfg) {
    suppressPackageStartupMessages({
        library(dplyr)
        library(tibble)
        library(tidyr)
    })
    out_root <- file.path(cfg$output, "activation")
    tab_dir <- file.path(out_root, "tables")
    dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
    primary_instruments <- c("AX3", "GENEActiv", "LEAP", "SENS motion")

    # Read low-excitation stage outcomes ----
    stage_results <- readRDS(file.path(cfg$masters, "dynamic/activation_stages.rds"))
    stage_results$instrument[stage_results$instrument == "SENS"] <- "SENS motion"
    write.csv(stage_results, file.path(tab_dir, "activation_deadband_stage_response_master.csv"), row.names = FALSE)
    schedule <- unique(stage_results[c("nominal_rpm", "frequency_hz", "centripetal_g")])
    write.csv(schedule, file.path(tab_dir, "Table_activation_conditions_rpm_frequency_acceleration.csv"),
        row.names = FALSE)
    first_sustained_crossing <- function(x, threshold_percent) {
        x <- arrange(x, measured_rpm)
        ok <- is.finite(x$coherent_response_percent) & x$coherent_response_percent >= threshold_percent
        if (!any(ok)) {
            last <- x[nrow(x), ]
            return(tibble(threshold_percent = threshold_percent, status = "not_reached_within_protocol",
                crossing_stage_index = NA_integer_, crossing_nominal_rpm = NA_real_, crossing_measured_rpm = NA_real_,
                crossing_frequency_hz = NA_real_, crossing_centripetal_g = NA_real_, crossing_centripetal_mg = NA_real_,
                boundary_nominal_rpm = last$nominal_rpm, boundary_centripetal_mg = last$centripetal_mg))
        }
        sustained <- vapply(seq_along(ok), function(i) all(ok[i:length(ok)], na.rm = FALSE), logical(1))
        idx <- which(sustained)[1]
        if (is.na(idx))
            idx <- which(ok)[1]
        row <- x[idx, ]
        status <- if (idx == 1L)
            "left_censored_at_first_stage" else "observed"
        tibble(threshold_percent = threshold_percent, status = status, crossing_stage_index = row$stage_index,
            crossing_nominal_rpm = row$nominal_rpm, crossing_measured_rpm = row$measured_rpm, crossing_frequency_hz = row$frequency_hz,
            crossing_centripetal_g = row$centripetal_g, crossing_centripetal_mg = row$centripetal_mg,
            boundary_nominal_rpm = if (idx == 1L)
                NA_real_ else x$nominal_rpm[idx - 1L], boundary_centripetal_mg = if (idx == 1L)
                NA_real_ else x$centripetal_mg[idx - 1L])
    }

    # Read operational threshold observations ----
    threshold_rows <- relocate(mutate(bind_rows(lapply(group_split(group_by(filter(stage_results, instrument %in%
        primary_instruments), instrument, id)), function(x) {
        mutate(bind_rows(first_sustained_crossing(x, 10), first_sustained_crossing(x, 50)), instrument = as.character(x$instrument[1]),
            id = x$id[1])
    })), endpoint = ifelse(threshold_percent == 10, "Dead-band endpoint (10%)", "Activation threshold (50%)"),
        instrument = factor(instrument, levels = primary_instruments)), instrument, id, endpoint, threshold_percent)

    write.csv(threshold_rows, file.path(tab_dir, "activation_deadband_unit_thresholds.csv"), row.names = FALSE)

    saveRDS(threshold_rows, file.path(out_root, "activation_deadband_unit_thresholds.rds"))

    mean_ci <- function(x) {
        x <- x[is.finite(x)]
        n <- length(x)
        if (!n)
            return(c(mean = NA_real_, lower = NA_real_, upper = NA_real_, n = 0))
        m <- mean(x)
        if (n == 1L)
            return(c(mean = m, lower = NA_real_, upper = NA_real_, n = n))
        se <- stats::sd(x)/sqrt(n)
        ci <- stats::qt(0.975, df = n - 1L) * se
        c(mean = m, lower = m - ci, upper = m + ci, n = n)
    }

    fmt_median_iqr <- function(x, digits = 1) {
        x <- x[is.finite(x)]
        if (!length(x))
            return(NA_character_)
        qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE, type = 6)
        paste0(formatC(qs[2], format = "f", digits = digits), " [", formatC(qs[1], format = "f", digits = digits),
            ", ", formatC(qs[3], format = "f", digits = digits), "]")
    }

    # Summarise thresholds and censoring ----
    threshold_summary <- arrange(summarise(group_by(threshold_rows, instrument, endpoint, threshold_percent),
        n_units = n_distinct(id), n_observed = sum(status == "observed"), n_left_censored = sum(status ==
            "left_censored_at_first_stage"), n_not_reached = sum(status == "not_reached_within_protocol"),
        crossing_nominal_rpm_median_iqr = fmt_median_iqr(crossing_nominal_rpm, 0), crossing_measured_rpm_median_iqr = fmt_median_iqr(crossing_measured_rpm,
            1), crossing_frequency_hz_median_iqr = fmt_median_iqr(crossing_frequency_hz, 3), crossing_centripetal_mg_median_iqr = fmt_median_iqr(crossing_centripetal_mg,
            2), crossing_centripetal_mg_mean = mean(crossing_centripetal_mg, na.rm = TRUE), crossing_centripetal_mg_lower = mean_ci(crossing_centripetal_mg)["lower"],
        crossing_centripetal_mg_upper = mean_ci(crossing_centripetal_mg)["upper"], .groups = "drop"),
        instrument, threshold_percent)

    write.csv(threshold_summary, file.path(tab_dir, "Table_activation_deadband_summary.csv"), row.names = FALSE)

    wide_threshold_summary <- tidyr::pivot_wider(select(threshold_summary, instrument, endpoint, n_units,
        n_observed, n_left_censored, n_not_reached, crossing_nominal_rpm_median_iqr, crossing_measured_rpm_median_iqr,
        crossing_frequency_hz_median_iqr, crossing_centripetal_mg_median_iqr), names_from = endpoint,
        values_from = c(n_observed, n_left_censored, n_not_reached, crossing_nominal_rpm_median_iqr,
            crossing_measured_rpm_median_iqr, crossing_frequency_hz_median_iqr, crossing_centripetal_mg_median_iqr),
        names_sep = " | ")

    write.csv(wide_threshold_summary, file.path(tab_dir, "Table_activation_deadband_summary_wide.csv"),
        row.names = FALSE)

    # Summarise coherent excitation response ----
    stage_summary <- mutate(summarise(group_by(filter(stage_results, instrument %in% primary_instruments),
        instrument, stage_index, nominal_rpm, measured_rpm, frequency_hz, centripetal_g, centripetal_mg),
        n_units = n_distinct(id), coherent_response_percent_mean = mean(coherent_response_percent, na.rm = TRUE),
        coherent_response_percent_lower = mean_ci(coherent_response_percent)["lower"], coherent_response_percent_upper = mean_ci(coherent_response_percent)["upper"],
        coherent_amp_mg_mean = mean(coherent_amp_g * 1000, na.rm = TRUE), coherent_amp_mg_lower = mean_ci(coherent_amp_g *
            1000)["lower"], coherent_amp_mg_upper = mean_ci(coherent_amp_g * 1000)["upper"], dynamic_rms_mg_mean = mean(dynamic_rms_g *
            1000, na.rm = TRUE), frequency_error_hz_mean = mean(expected_frequency_error_hz, na.rm = TRUE),
        .groups = "drop"), instrument = factor(instrument, levels = primary_instruments))

    write.csv(stage_summary, file.path(tab_dir, "activation_deadband_stage_response_summary.csv"), row.names = FALSE)

    # Summarise reference excitation agreement ----
    reference_validation <- select(filter(stage_results, instrument == "G-Link-200"), nominal_rpm, measured_rpm,
        frequency_hz, centripetal_mg, coherent_amp_g, coherent_response_percent, dynamic_rms_g, dominant_frequency_hz,
        expected_frequency_error_hz, n_samples, fs_derived_hz)

    write.csv(reference_validation, file.path(tab_dir, "reference_g_link_validation.csv"), row.names = FALSE)

}
