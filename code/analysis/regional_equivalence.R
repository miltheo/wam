# Intersection-union tests combine the required pairs/stages and both comparisons.
# Holm adjustment across 16 pipelines x 3 regions controls selection across 48 tests.
wam_regional_equivalence_values <- function(primary, secondary) {
    regions <- c("50-200 RPM", "250-350 RPM", "400-500 RPM")
    aggregate_arm <- function(x) {
        x <- x[x$margin_pct == 10, ]
        if (any(!is.finite(x$p_equivalence))) stop("Non-finite equivalence p values.")
        x$region <- ifelse(x$rpm <= 200, regions[1], ifelse(x$rpm <= 350, regions[2], regions[3]))
        stats::aggregate(p_equivalence ~ metric + region, x, max)
    }
    a <- aggregate_arm(primary); b <- aggregate_arm(secondary)
    z <- merge(a, b, by = c("metric", "region"), suffixes = c("_primary", "_secondary"))
    if (nrow(a) != 48L || nrow(b) != 48L || nrow(z) != 48L)
        stop("Expected 16 pipelines and three regions in both comparisons.")
    z$p_both <- pmax(z$p_equivalence_primary, z$p_equivalence_secondary)
    z$p_holm_all48 <- stats::p.adjust(z$p_both, "holm")
    z$equivalence_established <- z$p_holm_all48 <= .05
    z
}

wam_regional_equivalence <- function(cfg) {
    folder <- file.path(cfg$output, "features")
    z <- wam_regional_equivalence_values(
        utils::read.csv(file.path(folder, "primary_native/pairwise_tost.csv")),
        utils::read.csv(file.path(folder, "secondary_native/pairwise_tost.csv")))
    labels <- utils::read.csv(file.path(cfg$root, "code/settings/feature_pipelines.csv"))
    z$pipeline <- labels$metric_label[match(z$metric, labels$metric)]
    summary <- do.call(rbind, lapply(c("50-200 RPM", "250-350 RPM", "400-500 RPM"), function(region) {
        d <- z[z$region == region, ]; yes <- d[d$equivalence_established, ]
        data.frame(region = region, unadjusted_count = sum(d$p_both <= .05),
            simultaneous_lower_bound = nrow(yes),
            largest_adjusted_p_established = if(nrow(yes)) max(yes$p_holm_all48) else NA_real_)
    }))
    out <- file.path(folder, "regional_posthoc")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(z, file.path(out, "pipeline_results.csv"), row.names = FALSE)
    utils::write.csv(summary, file.path(out, "regional_summary.csv"), row.names = FALSE)
    invisible(z)
}
