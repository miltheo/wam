# Feature assessment table assembly ----
# Input: completed feature assessments.
# Output: criterion-specific comparison tables.

wam_feature_tables <- function(features, output) {
    for (arm in c("primary_native", "primary_calibrated", "secondary_native", "secondary_calibrated")) {
        folder <- file.path(features, arm)
        q <- wam_read_csv(file.path(folder, "qualification.csv"))
        q <- q[as.integer(q$margin_pct) == 10L, ]
        q <- q[order(as.integer(q$performance_rank)), ]
        regional <- wam_read_csv(file.path(folder, "regional.csv"))
        regional <- regional[as.integer(regional$margin_pct) == 10L, ]
        response <- wam_read_csv(file.path(folder, "device_response.csv"))
        rows <- lapply(seq_len(nrow(q)), function(i) {
            r <- q[i, ]
            devices <- response[response$metric == r$metric, ]
            stopifnot(nrow(devices) == if (startsWith(arm, "primary"))
                4L else 3L)

            # Assess full-range equivalence ----
            equivalent <- as.integer(r$passing) == as.integer(r$total)

            # Assess every model response ----
            adequate <- all(as.numeric(devices$slope_ci_low) > 0 & as.numeric(devices$inl_ci_high) <=
                10)
            stopifnot(equivalent == (r$universal_equivalence == "TRUE"), adequate == (r$adequate_response ==
                "TRUE"), (equivalent && adequate) == (r$both_criteria == "TRUE"))
            row <- list(feature_pipeline = r$metric_label, metric = r$metric, unit = r$metric_unit)

            # Add regional comparison counts ----
            for (region in c("50-200 RPM", "250-350 RPM", "400-500 RPM")) {
                cell <- regional[regional$metric == r$metric & regional$region == region, ]
                stopifnot(nrow(cell) == 1L)
                row[[region]] <- paste0(cell$passing, "/", cell$total)
            }
            row$full_range <- paste0(r$passing, "/", r$total)
            row$lowest_median_r_squared <- r$lowest_median_r2
            row$highest_upper_response_nonlinearity_pct <- r$highest_upper_inl_pct

            # Report both criterion outcomes ----
            row$criteria <- paste0("E ", if (equivalent)
                "met" else "unmet", "; R ", if (adequate)
                "met" else "unmet")
            row$implementation_qualification <- if (r$metric %in% c("SVMGSEC", "SVMGS", "ROCAM"))
                "sampling treatment" else if (startsWith(arm, "primary") && r$metric == "SVMBP_02_15")
                "SENS upper band clipped at Nyquist" else ""
            as.data.frame(row, check.names = FALSE)
        })
        rows <- do.call(rbind, rows)
        stopifnot(nrow(rows) == 16L)
        wam_write_csv(rows, file.path(output, paste0(arm, "_10pct.csv")))
    }
    notes <- paste0("E: every device-pair comparison at all ten 50-500 RPM conditions is equivalent at +/-10% of the fixed full-range feature span. ",
        "R: every model has a positive slope interval and an upper 95% bootstrap limit for median response nonlinearity <=10%. ",
        "Repeated trials are averaged within each of 15 physical units per model. Regional counts retain the full-range equivalence margin. ",
        "Sampling-treatment and clipped-bandwidth qualifications are retained in the manuscript table notes.\n")
    wam_write_text(notes, file.path(output, "table_notes.txt"))
    invisible(output)
}
