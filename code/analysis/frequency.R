# Study analysis and presentation; paths supplied by config.R.
wam_frequency <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(tidyr);library(tibble);library(ggplot2)})
    set.seed(20260929)
    tab<-file.path(cfg$output,"frequency/tables")
    dir.create(tab,recursive=TRUE,showWarnings=FALSE)
    all_rows<-read.csv(file.path(cfg$masters,"features/frequency_stages.csv"),stringsAsFactors=FALSE)
    keys<-c("instrument","device_id","trial_start_utc","rpm","signal_pipeline")
    study_reference <- all_rows |>
      filter(method == "STUDY_5S_XYZ_HANN") |>
      select(all_of(keys), study_hz = value_hz)
    method_vs_study <- all_rows |>
      left_join(study_reference, by = keys) |>
      mutate(delta_vs_study_hz = value_hz - study_hz,
        abs_delta_vs_study_hz = abs(delta_vs_study_hz))
    if (any(!is.finite(method_vs_study$study_hz))) stop("Study reference join failed")
    comparison_by_rpm <- method_vs_study |>
      group_by(method, signal_pipeline, instrument, rpm) |>
      summarise(n_stage_trials = n(),
        median_abs_delta_hz = median(abs_delta_vs_study_hz),
        p95_abs_delta_hz = quantile(abs_delta_vs_study_hz, .95),
        pct_within_0_25hz = 100 * mean(abs_delta_vs_study_hz <= .25),
        .groups = "drop")
    write.csv(comparison_by_rpm, file.path(tab, "method_vs_current_by_device_rpm.csv"), row.names = FALSE)
    comparison_overall <- method_vs_study |>
      group_by(method, signal_pipeline, instrument) |>
      summarise(n_stage_trials = n(),
        median_abs_delta_hz = median(abs_delta_vs_study_hz),
        p95_abs_delta_hz = quantile(abs_delta_vs_study_hz, .95),
        pct_within_0_25hz = 100 * mean(abs_delta_vs_study_hz <= .25),
        .groups = "drop")
    write.csv(comparison_overall, file.path(tab, "method_vs_current_by_device.csv"), row.names = FALSE)
    
    definitions <- tibble::tribble(
      ~method, ~source, ~channel, ~window, ~filter_taper, ~peak_band_hz, ~fidelity, ~full_range_eligible,
      "STUDY_5S_XYZ_HANN", "Study estimator", "Summed X/Y/Z power", "5 s nonoverlap", "Axis demean; Hann", "0.3 to min(15, 0.45 fs)", "Exact current code; stage mode", TRUE,
      "STUDY_4MIN_XYZ_BP0210", "Shaker audit", "Summed X/Y/Z power", "4 min stage", "4th-order zero-phase BP 0.2-10 Hz", "0.2-10", "Exact cached canonical audit", TRUE,
      "PREECE_2S_X", "Preece et al. 2009", "X axis", "2 s; 1 s step", "No filter or taper reported", "Nonzero to Nyquist assumed", "Reconstruction; paper does not define peak search or stage summary", TRUE,
      "PREECE_2S_Y", "Preece et al. 2009", "Y axis", "2 s; 1 s step", "No filter or taper reported", "Nonzero to Nyquist assumed", "Reconstruction; paper does not define peak search or stage summary", TRUE,
      "PREECE_2S_Z", "Preece et al. 2009", "Z axis", "2 s; 1 s step", "No filter or taper reported", "Nonzero to Nyquist assumed", "Reconstruction; paper does not define peak search or stage summary", TRUE,
      "ZHANG_12_8S_VM_F625", "Zhang et al. 2012", "Vector magnitude", "12.8 s nonoverlap", "No filter or taper reported", "0.6-2.5", "Published restricted feature; stage mode added", FALSE,
      "MANNINI_12_8S_VM_LP15", "Mannini et al. 2013", "Vector magnitude", "12.8 s nonoverlap", "4th-order 15 Hz lowpass; zero-phase assumed", "0.3-15; adapted for SENS fs", "Reconstruction; LP cap min(15,0.45fs) where required", TRUE,
      "ELLIS_60S_VM_MAX", "Ellis et al. 2016", "Vector magnitude", "60 s nonoverlap", "Raw; rectangular FFT assumed", "Nonzero to Nyquist", "Reconstruction; peak rule described, taper not specified; stage mode added", TRUE,
      "ELLIS_60S_VM_BAND03_3", "Ellis et al. 2016", "Vector magnitude", "60 s nonoverlap", "Raw; rectangular FFT assumed", "0.3-3", "Published restricted feature; stage mode added", FALSE,
      "STAUDENMAYER_15S_VM", "Staudenmayer et al. 2015", "Vector magnitude", "15 s nonoverlap", "Raw; rectangular FFT assumed", "Nonzero to Nyquist assumed", "Reconstruction; spectral implementation under-specified; stage mode added", TRUE,
      "AHMADI_15S_VM_BAND025_5", "Ahmadi et al. 2022", "Vector magnitude", "15 s nonoverlap", "Raw; rectangular FFT assumed", "0.25-5", "Reconstruction; stage mode added", FALSE,
      "COYLE_30S_X", "Coyle-Asbil et al. 2025", "X axis", "30 s nonoverlap", "4th-order causal BP 0.3-10", "Nonzero to Nyquist", "Reconstruction of released code with measured fs; modal stage summary added", TRUE,
      "COYLE_30S_Y", "Coyle-Asbil et al. 2025", "Y axis", "30 s nonoverlap", "4th-order causal BP 0.3-10", "Nonzero to Nyquist", "Reconstruction of released code with measured fs; modal stage summary added", TRUE,
      "COYLE_30S_Z", "Coyle-Asbil et al. 2025", "Z axis", "30 s nonoverlap", "4th-order causal BP 0.3-10", "Nonzero to Nyquist", "Reconstruction of released code with measured fs; modal stage summary added", TRUE,
      "JEONG_5S_X", "Jeong et al. 2022", "X axis", "5 s; 1 s step", "Hamming", "Nonzero to min(15,0.45fs)", "Published window/taper/axis design; strongest peak only; stage mode added", TRUE,
      "JEONG_5S_Y", "Jeong et al. 2022", "Y axis", "5 s; 1 s step", "Hamming", "Nonzero to min(15,0.45fs)", "Published window/taper/axis design; strongest peak only; stage mode added", TRUE,
      "JEONG_5S_Z", "Jeong et al. 2022", "Z axis", "5 s; 1 s step", "Hamming", "Nonzero to min(15,0.45fs)", "Published window/taper/axis design; strongest peak only; stage mode added", TRUE,
      "METRONAUTICA_4MIN_VM_EXACT", "Metronautica frozen bridge", "Vector magnitude", "4 min stage", "Demean; rectangular; optional bridge filter off", "Nonzero to Nyquist", "Reproduces frozen dominant_frequency function and grid using clipper-derived stage fs", TRUE,
      "METRONAUTICA_4MIN_VM_CORRECTGRID", "Metronautica diagnostic correction", "Vector magnitude", "4 min stage", "Demean; rectangular", "Nonzero to Nyquist", "Same selected bin with standard k fs/N frequency grid", TRUE
    )
    write.csv(definitions, file.path(tab, "method_definitions.csv"), row.names = FALSE)
    
    rpm_summary <- all_rows |>
      group_by(method, signal_pipeline, instrument, rpm, expected_hz) |>
      summarise(n_stage_trials = n(), n_units = n_distinct(device_id),
        mean_hz = mean(value_hz), median_hz = median(value_hz),
        p05_hz = quantile(value_hz, .05), p95_hz = quantile(value_hz, .95),
        median_abs_error_hz = median(abs_error_hz),
        mean_abs_error_hz = mean(abs_error_hz),
        pct_within_0_10hz = 100 * mean(within_0_10hz),
        pct_within_0_25hz = 100 * mean(within_0_25hz),
        pct_second_harmonic_within_0_25hz = 100 * mean(second_harmonic_within_0_25hz),
        pct_stage_fundamental_match = 100 * mean(stage_fundamental_match),
        pct_stage_double_match = 100 * mean(stage_double_match),
        mean_fundamental_window_pct = if (all(is.na(fundamental_window_pct))) NA_real_ else mean(fundamental_window_pct, na.rm = TRUE),
        .groups = "drop")
    write.csv(rpm_summary, file.path(tab, "method_by_device_rpm_summary.csv"), row.names = FALSE)
    
    performance <- all_rows |>
      group_by(method, signal_pipeline, instrument) |>
      summarise(n_stage_trials = n(), n_units = n_distinct(device_id),
        mean_error_hz = mean(error_hz), mean_abs_error_hz = mean(abs_error_hz),
        rmse_hz = sqrt(mean(error_hz^2)), median_abs_error_hz = median(abs_error_hz),
        p95_abs_error_hz = quantile(abs_error_hz, .95),
        mean_abs_error_pct = mean(abs_error_pct),
        pct_within_0_10hz = 100 * mean(within_0_10hz),
        pct_within_0_25hz = 100 * mean(within_0_25hz),
        pct_second_harmonic_within_0_25hz = 100 * mean(second_harmonic_within_0_25hz),
        pct_stage_fundamental_match = 100 * mean(stage_fundamental_match),
        pct_stage_double_match = 100 * mean(stage_double_match),
        mean_fundamental_window_pct = if (all(is.na(fundamental_window_pct))) NA_real_ else mean(fundamental_window_pct, na.rm = TRUE),
        .groups = "drop")
    write.csv(performance, file.path(tab, "method_by_device_performance.csv"), row.names = FALSE)
    
    unit_stage <- all_rows |>
      group_by(method, signal_pipeline, instrument, device_id, rpm, expected_hz) |>
      summarise(value_hz = mean(value_hz), n_trials = n_distinct(trial_number), .groups = "drop")
    if (!identical(unique(all_rows$signal_pipeline), "Native") ||
        any(unit_stage$n_trials != 3L) || nrow(unit_stage) != 19L * 4L * 15L * 10L)
        stop("Native unit-stage trial coverage audit failed")
    write.csv(unit_stage, file.path(tab, "unit_stage_trial_averages.csv"), row.names = FALSE)
    
    fit_one <- function(d) {
      fit <- lm(value_hz ~ expected_hz, data = d)
      yhat <- fitted(fit)
      span <- diff(range(yhat))
      r2 <- summary(fit)$r.squared
      if (!is.finite(r2)) r2 <- 0
      tibble(slope = unname(coef(fit)[2]), intercept_hz = unname(coef(fit)[1]),
        r2 = r2,
        response_inl_pct = if (span > .Machine$double.eps) 100 * max(abs(d$value_hz - yhat)) / span else Inf,
        mean_abs_error_pct = mean(abs(d$value_hz - d$expected_hz) / d$expected_hz) * 100)
    }
    response_unit <- unit_stage |>
      group_by(method, signal_pipeline, instrument, device_id) |>
      group_modify(~fit_one(.x)) |>
      ungroup()
    write.csv(response_unit, file.path(tab, "response_by_physical_unit.csv"), row.names = FALSE)
    
    bootstrap_device <- function(d, reps = 5000L) {
      n <- nrow(d)
      if (n != 15L) stop("Device response must have 15 physical units")
      ix <- matrix(sample.int(n, n * reps, replace = TRUE), nrow = n)
      boot <- function(x) apply(matrix(x[ix], nrow = n), 2, median)
      slope <- boot(d$slope); r2 <- boot(d$r2); inl <- boot(d$response_inl_pct)
      tibble(n_units = n,
        median_slope = median(d$slope), slope_ci_low = quantile(slope, .025), slope_ci_high = quantile(slope, .975),
        median_r2 = median(d$r2), r2_ci_low = quantile(r2, .025), r2_ci_high = quantile(r2, .975),
        median_inl_pct = median(d$response_inl_pct), inl_ci_low = quantile(inl, .025), inl_ci_high = quantile(inl, .975),
        median_mean_abs_error_pct = median(d$mean_abs_error_pct))
    }
    response_device <- response_unit |>
      group_by(method, signal_pipeline, instrument) |>
      group_modify(~bootstrap_device(.x)) |>
      ungroup()
    write.csv(response_device, file.path(tab, "response_by_device_type_bootstrap.csv"), row.names = FALSE)
    
    span <- unit_stage |>
      group_by(method, signal_pipeline, instrument, rpm) |>
      summarise(mean_hz = mean(value_hz), .groups = "drop") |>
      filter(rpm %in% c(50, 500)) |>
      pivot_wider(names_from = rpm, values_from = mean_hz, names_prefix = "rpm_") |>
      mutate(device_span_hz = abs(rpm_500 - rpm_50)) |>
      group_by(method, signal_pipeline) |>
      summarise(dynamic_span_hz = median(device_span_hz), .groups = "drop")
    write.csv(span, file.path(tab, "method_dynamic_spans.csv"), row.names = FALSE)
    
    welch_tost <- function(x, y, margin) {
      nx <- length(x); ny <- length(y)
      delta <- mean(x) - mean(y)
      vx <- var(x); vy <- var(y)
      se <- sqrt(vx / nx + vy / ny)
      if (!is.finite(margin) || margin <= 0) return(tibble(mean_difference_hz = delta, ci90_low = NA_real_, ci90_high = NA_real_, p_equivalence = NA_real_, equivalent = FALSE))
      if (!is.finite(se) || se < .Machine$double.eps) return(tibble(mean_difference_hz = delta, ci90_low = delta, ci90_high = delta, p_equivalence = ifelse(abs(delta) < margin, 0, 1), equivalent = abs(delta) < margin))
      df <- (vx / nx + vy / ny)^2 / ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))
      low <- delta - qt(.95, df) * se; high <- delta + qt(.95, df) * se
      p <- max(pt((delta + margin) / se, df, lower.tail = FALSE), pt((delta - margin) / se, df, lower.tail = TRUE))
      tibble(mean_difference_hz = delta, ci90_low = low, ci90_high = high, p_equivalence = p,
        equivalent = low > -margin && high < margin && p < .05)
    }
    pairwise <- list(); k <- 0L
    for (method_id in unique(unit_stage$method)) for (pipeline in unique(unit_stage$signal_pipeline)) {
      s <- span |> filter(method == method_id, signal_pipeline == pipeline) |> pull(dynamic_span_hz)
      for (rpm_value in seq(50, 500, 50)) {
        d <- unit_stage |> filter(method == method_id, signal_pipeline == pipeline, rpm == rpm_value)
        for (pair in combn(sort(unique(d$instrument)), 2, simplify = FALSE)) {
          x <- d$value_hz[d$instrument == pair[1]]; y <- d$value_hz[d$instrument == pair[2]]
          for (margin_pct in c(5, 10, 15)) {
            k <- k + 1L
            pairwise[[k]] <- welch_tost(x, y, s * margin_pct / 100) |>
              mutate(method = method_id, signal_pipeline = pipeline, rpm = rpm_value,
                device_1 = pair[1], device_2 = pair[2], margin_pct = margin_pct,
                dynamic_span_hz = s, absolute_margin_hz = s * margin_pct / 100)
          }
        }
      }
    }
    pairwise <- bind_rows(pairwise)
    write.csv(pairwise, file.path(tab, "pairwise_welch_tost.csv"), row.names = FALSE)
    equivalence <- pairwise |>
      group_by(method, signal_pipeline, margin_pct) |>
      summarise(pairs_passing = sum(equivalent), pairs_total = n(),
        stages_all_six_passing = sum(vapply(split(equivalent, rpm), all, logical(1))),
        all_60_passing = all(equivalent), dynamic_span_hz = first(dynamic_span_hz),
        .groups = "drop")
    write.csv(equivalence, file.path(tab, "equivalence_summary.csv"), row.names = FALSE)
    
    response_worst <- response_device |>
      group_by(method, signal_pipeline) |>
      summarise(worst_median_r2 = min(median_r2), worst_upper95_inl_pct = max(inl_ci_high),
        minimum_slope_lower95 = min(slope_ci_low), .groups = "drop")
    method_summary <- performance |>
      group_by(method, signal_pipeline) |>
      summarise(worst_device_mae_hz = max(mean_abs_error_hz), mean_abs_error_hz = mean(mean_abs_error_hz),
        mean_abs_error_pct = mean(mean_abs_error_pct),
        within_0_10hz_pct = mean(pct_within_0_10hz),
        within_0_25hz_pct = mean(pct_within_0_25hz),
        second_harmonic_within_0_25hz_pct = mean(pct_second_harmonic_within_0_25hz),
        stage_fundamental_match_pct = mean(pct_stage_fundamental_match),
        stage_double_match_pct = mean(pct_stage_double_match), .groups = "drop") |>
      left_join(response_worst, by = c("method", "signal_pipeline")) |>
      left_join(equivalence |> filter(margin_pct == 10) |>
        select(method, signal_pipeline, pairs_passing_10 = pairs_passing, stages_all_six_passing_10 = stages_all_six_passing, all_60_passing_10 = all_60_passing),
        by = c("method", "signal_pipeline")) |>
      left_join(definitions |> select(method, full_range_eligible), by = "method") |>
      mutate(response_pass_10 = minimum_slope_lower95 > 0 & worst_upper95_inl_pct <= 10,
        interdevice_response_pass_10 = full_range_eligible & all_60_passing_10 & response_pass_10)
    write.csv(method_summary, file.path(tab, "method_comparison_summary.csv"), row.names = FALSE)
    
}

