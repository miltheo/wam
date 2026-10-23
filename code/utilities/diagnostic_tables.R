# Diagnostic table assembly ----
# Input: diagnostic observations and summaries.
# Output: formatted diagnostic tables.

wam_diagnostic_tables <- function(master, output) {
    d <- wam_read_csv(master)
    d$instrument[d$instrument == "SENS"] <- "SENS motion"
    stopifnot(nrow(d) == 2790L)
    num <- function(x, k) as.numeric(x[[k]])
    truth <- function(x, k) x[[k]] == "TRUE"
    keys <- paste(d$family, d$instrument, d$id, sep = "\034")
    units <- lapply(unique(keys), function(key) {
        x <- d[keys == key, , drop = FALSE]
        integrity <- Reduce(`|`, lapply(c("missing_time", "duplicate_time", "backward_time", "invalid_axis"),
            function(k) num(x, k) > 0))
        zero <- truth(x, "all_zero_sequence")
        equal <- truth(x, "equal_nonzero_sequence")
        near <- num(x, "near_limit_n") > 0
        u <- data.frame(family = x$family[1], instrument = x$instrument[1], id = x$id[1], records = nrow(x),
            integrity = any(integrity), all_zero = any(zero), equal_nonzero = any(equal), near_limit = any(near),
            flagged_records = sum(integrity | zero | equal | near), integrity_records = sum(integrity),
            pattern_records = sum(zero | equal), near_limit_records = sum(near), median_spacing_ms = median(num(x,
                "median_dt_ms")), highest_iqr_ms = max(num(x, "iqr_dt_ms")), longest_spacing_ms = max(num(x,
                "longest_dt_ms")), all_zero_samples = sum(num(x, "all_zero_samples")), equal_nonzero_samples = sum(num(x,
                "equal_nonzero_samples")))
        u$integrity_or_pattern <- u$integrity | u$all_zero | u$equal_nonzero
        u$any_flag <- u$integrity_or_pattern | u$near_limit
        for (axis in c("x", "y", "z")) u[[paste0("zero_", axis, "_pct")]] <- 100 * sum(num(x, paste0("zero_",
            axis, "_n")))/sum(num(x, paste0("finite_", axis, "_n")))
        u$near_limit_pct <- 100 * sum(num(x, "near_limit_n"))/sum(num(x, "finite_axis_n"))
        u
    })
    units <- do.call(rbind, units)
    dyn <- units[units$family == "dynamic", ]
    stopifnot(sum(units$family == "static") == 90L, nrow(dyn) == 90L, all(dyn$records == ifelse(dyn$instrument ==
        "LEAP", 50L, ifelse(dyn$instrument %in% c("GT3X-BT", "GT9X Link"), 20L, 30L))))
    models <- c("AX3", "GENEActiv", "GT3X-BT", "GT9X Link", "LEAP", "SENS motion")
    summaries <- list()
    for (family in c("static", "dynamic")) for (model in models) {
        x <- units[units$family == family & units$instrument == model, ]
        if (!nrow(x))
            next
        row <- data.frame(family = family, instrument = model, units = nrow(x), records = sum(x$records))
        for (k in c("integrity", "all_zero", "equal_nonzero", "near_limit", "integrity_or_pattern", "any_flag",
            "flagged_records", "near_limit_records")) row[[k]] <- sum(x[[k]])
        summaries[[length(summaries) + 1L]] <- row
    }
    summaries <- do.call(rbind, summaries)
    fmt <- function(x) if (x == 0)
        "0" else if (abs(x) < 0.001)
        "<0.001" else sub("\\.?0+$", "", sprintf("%.3f", x))
    headers <- c("Condition / model", "Units", "1. Data integrity\nUnits flagged", "2. Sample timing\nTypical / spread / longest (ms)",
        "3. Axis-zero proportion\nX / Y / Z (%): median; [highest]", "4. Unusual cross-axis patterns\nUnits flagged: all-zero / equal non-zero",
        "5. Near-limit values\nMedian [highest] %; units; records")
    presentation <- lapply(seq_len(nrow(summaries)), function(i) {
        s <- summaries[i, ]
        x <- units[units$family == s$family & units$instrument == s$instrument, ]
        zero <- function(fun) paste(vapply(c("x", "y", "z"), function(a) fmt(fun(x[[paste0("zero_", a,
            "_pct")]])), character(1)), collapse = " / ")
        values <- c(paste0(if (s$family == "static") "Static" else "Dynamic", "\n", s$instrument), as.character(s$units),
            paste0(s$integrity, "/", s$units), paste(vapply(c(median(x$median_spacing_ms), max(x$highest_iqr_ms),
                max(x$longest_spacing_ms)), fmt, character(1)), collapse = " / "), paste0(zero(median),
                "\n[", zero(max), "]"), paste0(s$all_zero, "/", s$units, " ; ", s$equal_nonzero, "/",
                s$units), paste0(fmt(median(x$near_limit_pct)), " [", fmt(max(x$near_limit_pct)), "]%\n",
                s$near_limit, "/", s$units, " units; ", s$near_limit_records, "/", s$records, " records"))
        as.data.frame(setNames(as.list(values), headers), check.names = FALSE)
    })
    presentation <- do.call(rbind, presentation)
    products <- list(unit_diagnostics = units, model_flag_summary = summaries, combined_results_table = presentation)
    if (any(units$any_flag))
        products$flagged_units <- units[units$any_flag, ]
    for (name in names(products)) wam_write_csv(products[[name]], file.path(output, paste0(name, ".csv")))
    invisible(products)
}
