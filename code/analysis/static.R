# Static metrology comparisons ----
# Input: static outcome master.
# Output: device comparisons and summaries.

wam_static <- function(cfg) {

    project_root <- cfg$root
    master_rds <- file.path(cfg$masters, "static/static.rds")
    out_dir <- file.path(cfg$output, "static")

    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    required_pkgs <- c("dplyr", "tibble", "tidyr")
    missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_pkgs)) {
        stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "), ". Install these in R before running this workflow.")
    }

    library(dplyr)
    library(tibble)
    library(tidyr)

    device_order <- c("AX3", "GENEActiv", "GT3X-BT", "GT9X Link", "LEAP", "SENS motion")

    fmt_p <- function(p) {
        ifelse(!is.finite(p), NA_character_, ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p)))
    }

    # Unit-level mean confidence intervals ----
    mean_ci <- function(x, alpha = 0.05) {
        x <- x[is.finite(x)]
        n <- length(x)
        if (n == 0L)
            return(tibble(mean = NA_real_, lower = NA_real_, upper = NA_real_, n = 0L))
        m <- mean(x)
        if (n < 2L)
            return(tibble(mean = m, lower = NA_real_, upper = NA_real_, n = n))
        se <- stats::sd(x)/sqrt(n)
        tcrit <- stats::qt(1 - alpha/2, df = n - 1)
        tibble(mean = m, lower = m - tcrit * se, upper = m + tcrit * se, n = n)
    }

    mean_ci_text <- function(x, digits = 2) {
        s <- mean_ci(x)
        if (!is.finite(s$mean))
            return(NA_character_)
        sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"), s$mean, s$lower, s$upper)
    }

    # Median-centred variance comparison ----
    brown_forsythe <- function(data, y, group = "device") {
        tmp <- data |>
            dplyr::select(group = dplyr::all_of(group), y = dplyr::all_of(y)) |>
            dplyr::filter(is.finite(y)) |>
            dplyr::group_by(group) |>
            dplyr::mutate(z = abs(y - stats::median(y, na.rm = TRUE))) |>
            dplyr::ungroup()

        fit <- stats::aov(z ~ group, data = tmp)
        a <- summary(fit)[[1]]

        tibble(statistic = unname(a$`F value`[1]), df1 = unname(a$Df[1]), df2 = unname(a$Df[2]), p = unname(a$`Pr(>F)`[1]),
            p_text = fmt_p(unname(a$`Pr(>F)`[1])))
    }

    # Unequal-variance device comparison ----
    welch_test <- function(data, y, group = "device") {
        fit <- stats::oneway.test(stats::as.formula(paste(y, "~", group)), data = data, var.equal = FALSE)
        f_value <- unname(fit$statistic)
        df1 <- unname(fit$parameter[1])
        df2 <- unname(fit$parameter[2])
        p_value <- unname(fit$p.value)
        eta_p2 <- (f_value * df1)/((f_value * df1) + df2)

        tibble(statistic = f_value, df1 = df1, df2 = df2, p = p_value, p_text = fmt_p(p_value), eta_p2 = eta_p2)
    }

    # Unequal-variance pairwise comparisons ----
    games_howell <- function(data, y, group = "device", alpha = 0.05) {
        tmp <- data |>
            dplyr::select(group = dplyr::all_of(group), y = dplyr::all_of(y)) |>
            dplyr::filter(is.finite(y)) |>
            dplyr::mutate(group = factor(group, levels = device_order))

        groups <- levels(droplevels(tmp$group))
        k <- length(groups)
        pairs <- utils::combn(groups, 2, simplify = FALSE)

        out <- lapply(pairs, function(pair) {
            g1 <- pair[1]
            g2 <- pair[2]
            x1 <- tmp$y[tmp$group == g1]
            x2 <- tmp$y[tmp$group == g2]
            n1 <- length(x1)
            n2 <- length(x2)
            m1 <- mean(x1)
            m2 <- mean(x2)
            v1 <- stats::var(x1)
            v2 <- stats::var(x2)
            diff <- m1 - m2
            se <- sqrt(v1/n1 + v2/n2)

            if (!is.finite(se) || se <= 0 || n1 < 2L || n2 < 2L) {
                t_value <- NA_real_
                df <- NA_real_
                p_value <- NA_real_
                ci_lower <- NA_real_
                ci_upper <- NA_real_
            } else {
                t_value <- diff/se
                df <- (v1/n1 + v2/n2)^2/((v1/n1)^2/(n1 - 1) + (v2/n2)^2/(n2 - 1))
                q_value <- sqrt(2) * abs(t_value)
                p_value <- stats::ptukey(q_value, nmeans = k, df = df, lower.tail = FALSE)
                qcrit <- stats::qtukey(1 - alpha, nmeans = k, df = df)
                margin <- qcrit/sqrt(2) * se
                ci_lower <- diff - margin
                ci_upper <- diff + margin
            }

            tibble(group1 = g1, group2 = g2, mean1 = m1, mean2 = m2, mean_difference = diff, ci_lower = ci_lower,
                ci_upper = ci_upper, t = t_value, df = df, p = p_value, p_text = fmt_p(p_value), significant = is.finite(p_value) &
                  p_value < alpha)
        })

        dplyr::bind_rows(out)
    }

    # Pairwise contrast letter display ----
    compact_letters <- function(pairwise, groups) {
        sig <- matrix(FALSE, length(groups), length(groups), dimnames = list(groups, groups))

        for (i in seq_len(nrow(pairwise))) {
            g1 <- pairwise$group1[i]
            g2 <- pairwise$group2[i]
            if (isTRUE(pairwise$significant[i])) {
                sig[g1, g2] <- TRUE
                sig[g2, g1] <- TRUE
            }
        }

        all_sets <- list()
        for (size in rev(seq_along(groups))) {
            combs <- utils::combn(groups, size, simplify = FALSE)
            for (s in combs) {
                ok <- TRUE
                if (length(s) > 1) {
                  pairs <- utils::combn(s, 2, simplify = FALSE)
                  ok <- all(vapply(pairs, function(p) !sig[p[1], p[2]], logical(1)))
                }
                if (ok)
                  all_sets <- append(all_sets, list(s))
            }
        }

        nonsig_edges <- list()
        for (p in utils::combn(groups, 2, simplify = FALSE)) {
            if (!sig[p[1], p[2]])
                nonsig_edges <- append(nonsig_edges, list(p))
        }

        covers <- function(combo) {
            covered_groups <- unique(unlist(combo))
            if (!setequal(covered_groups, groups))
                return(FALSE)
            if (length(nonsig_edges) == 0L)
                return(TRUE)
            all(vapply(nonsig_edges, function(e) {
                any(vapply(combo, function(s) all(e %in% s), logical(1)))
            }, logical(1)))
        }

        chosen <- NULL
        for (r in seq_along(all_sets)) {
            idx_combos <- utils::combn(seq_along(all_sets), r, simplify = FALSE)
            for (idx in idx_combos) {
                combo <- all_sets[idx]
                if (covers(combo)) {
                  chosen <- combo
                  break
                }
            }
            if (!is.null(chosen))
                break
        }

        if (is.null(chosen))
            chosen <- as.list(groups)

        letters_out <- stats::setNames(rep("", length(groups)), groups)
        for (i in seq_along(chosen)) {
            for (g in chosen[[i]]) {
                letters_out[g] <- paste0(letters_out[g], letters[i])
            }
        }

        letters_out
    }

    write_csv_base <- function(x, path) {
        utils::write.csv(x, path, row.names = FALSE, quote = TRUE)
    }

    if (!file.exists(master_rds)) {
        stop("Master RDS not found: ", master_rds, ". Run the static stage first.")
    }

    # Load and standardise master outcomes ----
    dat <- readRDS(master_rds) |>
        dplyr::mutate(id = as.character(id), instrument = as.character(instrument), instrument = dplyr::if_else(grepl("^MOS",
            id), "GT3X-BT", instrument), instrument = dplyr::if_else(grepl("^TAS", id), "GT9X Link",
            instrument), instrument = dplyr::recode(instrument, `SENs motion` = "SENS motion", `SENs Motion` = "SENS motion"),
            device = factor(instrument, levels = device_order), vm_offset_mg = vm_offset_g_5min * 1000,
            vm_drift_mg_h = vm_drift_g_per_h * 1000, allan_median_ug = apply(as.matrix(dplyr::pick(x_allan_min_g,
                y_allan_min_g, z_allan_min_g)), 1, stats::median, na.rm = TRUE) * 1e+06, allan_max_ug = apply(as.matrix(dplyr::pick(x_allan_min_g,
                y_allan_min_g, z_allan_min_g)), 1, max, na.rm = TRUE) * 1e+06, static_noise_floor_mg = dplyr::coalesce(static_noise_floor_mg,
                max_axis_noise_sd_g * 1000), static_raw_max_axis_sd_mg = dplyr::coalesce(static_raw_max_axis_sd_mg,
                max_axis_sd_g * 1000), static_noise_floor_5min_mg = dplyr::coalesce(static_noise_floor_5min_mg,
                max_axis_sd_5min_g * 1000)) |>
        dplyr::filter(!is.na(device))

    write_csv_base(dat, file.path(out_dir, "static_clean_master.csv"))
    saveRDS(dat, file.path(out_dir, "static_clean_master.rds"))

    # Select reported metrology outcomes ----
    metrics <- tibble::tribble(~metric, ~var, ~unit, ~digits, "Signed 1-g VM offset bias", "vm_offset_mg",
        "mg", 1, "Signed VM drift rate", "vm_drift_mg_h", "mg/h", 4, "24 h max-axis static noise floor",
        "static_noise_floor_mg", "mg", 3, "Maximum-axis Allan minimum", "allan_max_ug", "ug", 1)

    # Assess distributional diagnostics ----
    normality_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        metric <- metrics$metric[i]
        var <- metrics$var[i]

        dat |>
            dplyr::group_by(device) |>
            dplyr::summarise(metric = metric, n = sum(is.finite(.data[[var]])), W = if (n >= 3L)
                unname(stats::shapiro.test(.data[[var]])$statistic) else NA_real_, p = if (n >= 3L)
                unname(stats::shapiro.test(.data[[var]])$p.value) else NA_real_, p_text = fmt_p(p), .groups = "drop")
    }))

    # Compare between-device variability ----
    brown_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        brown_forsythe(dat, metrics$var[i]) |>
            dplyr::mutate(metric = metrics$metric[i], var = metrics$var[i], .before = 1)
    }))

    # Compare device outcome means ----
    welch_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        welch_test(dat, metrics$var[i]) |>
            dplyr::mutate(metric = metrics$metric[i], var = metrics$var[i], unit = metrics$unit[i], .before = 1)
    }))

    # Calculate device-pair contrasts ----
    pairwise_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        games_howell(dat, metrics$var[i]) |>
            dplyr::mutate(metric = metrics$metric[i], var = metrics$var[i], unit = metrics$unit[i], .before = 1)
    }))

    letters_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        pw <- dplyr::filter(pairwise_tbl, metric == metrics$metric[i])
        groups <- as.character(device_order)
        letters_vec <- compact_letters(pw, groups)
        tibble(metric = metrics$metric[i], var = metrics$var[i], device = names(letters_vec), letters = unname(letters_vec))
    }))

    summary_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
        metric <- metrics$metric[i]
        var <- metrics$var[i]
        unit <- metrics$unit[i]
        digits <- metrics$digits[i]

        dat |>
            dplyr::group_by(device) |>
            dplyr::summarise(metric = metric, var = var, unit = unit, n = sum(is.finite(.data[[var]])),
                mean = mean(.data[[var]], na.rm = TRUE), lower = mean_ci(.data[[var]])$lower, upper = mean_ci(.data[[var]])$upper,
                mean_ci = mean_ci_text(.data[[var]], digits = digits), .groups = "drop")
    })) |>
        dplyr::left_join(letters_tbl, by = c("metric", "var", "device")) |>
        dplyr::mutate(mean_ci_letters = paste0(mean_ci, " ", letters))

    significant_pairwise_tbl <- pairwise_tbl |>
        dplyr::filter(significant) |>
        dplyr::arrange(metric, p) |>
        dplyr::mutate(comparison = paste(group1, "vs", group2))

    table_wide <- summary_tbl |>
        dplyr::select(device, var, mean_ci_letters) |>
        tidyr::pivot_wider(names_from = var, values_from = mean_ci_letters) |>
        dplyr::rename(Instrument = device, `1-g VM offset bias, mg` = vm_offset_mg, `VM drift, mg/h` = vm_drift_mg_h,
            `24 h max-axis static noise floor, mg` = static_noise_floor_mg, `Max-axis Allan minimum, ug` = allan_max_ug) |>
        dplyr::left_join(dat |>
            dplyr::count(device, name = "n") |>
            dplyr::rename(Instrument = device), by = "Instrument") |>
        dplyr::relocate(n, .after = Instrument)

    extended_noise_summary <- dat |>
        dplyr::group_by(device) |>
        dplyr::summarise(n = dplyr::n(), raw_max_axis_sd_mg = mean_ci_text(static_raw_max_axis_sd_mg,
            digits = 3), detrended_max_axis_sd_mg = mean_ci_text(static_noise_floor_mg, digits = 3),
            first5min_max_axis_sd_mg = mean_ci_text(static_noise_floor_5min_mg, digits = 3), vm_sd_mg = mean_ci_text(vm_sd_mg,
                digits = 3), vm_noise_sd_mg = mean_ci_text(vm_noise_sd_mg, digits = 3), vm_cv_percent = mean_ci_text(vm_cv_percent,
                digits = 3), .groups = "drop")

    write_csv_base(normality_tbl, file.path(out_dir, "static_normality_shapiro.csv"))
    write_csv_base(brown_tbl, file.path(out_dir, "static_brown_forsythe.csv"))
    write_csv_base(welch_tbl, file.path(out_dir, "static_welch_anova.csv"))
    write_csv_base(pairwise_tbl, file.path(out_dir, "static_games_howell_pairwise.csv"))
    write_csv_base(significant_pairwise_tbl, file.path(out_dir, "static_games_howell_significant_pairwise.csv"))
    write_csv_base(letters_tbl, file.path(out_dir, "static_games_howell_letters.csv"))
    write_csv_base(summary_tbl, file.path(out_dir, "static_summary_by_device.csv"))
    write_csv_base(table_wide, file.path(out_dir, "static_manuscript_table.csv"))
    write_csv_base(extended_noise_summary, file.path(out_dir, "static_noise_extended_summary.csv"))

    message("Analysis complete. Outputs written to: ", out_dir)
}
