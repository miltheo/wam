# Check the products of a complete run; no models or figures are regenerated.
source_path <- if (sys.nframe()) sys.frame(1)$ofile else NULL
script <- if (!is.null(source_path)) source_path else
    sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script,winslash="/",mustWork=TRUE))))
source(file.path(root,"code/config.R"))
cfg <- wam_config(root)
verify <- function(..., message) {
    if (!all(vapply(list(...), function(x) isTRUE(all(x)), logical(1))))
        stop(message, call. = FALSE)
}
read_output <- function(path) {
    full <- file.path(cfg$output, path)
    if (!file.exists(full)) stop("Missing analysis output: ", full,
        ". Run its producing stage before checking results.", call. = FALSE)
    read.csv(full, check.names = FALSE)
}

# The registries must resolve entirely to newly generated products.
for (registry in c("figures","tables")) {
    index <- read.csv(file.path(root,"code/settings",paste0(registry,".csv")))
    missing <- index$source[!file.exists(file.path(cfg$output, index$source))]
    verify(!length(missing), message = paste0("Missing registered ", registry,
        ": ", paste(missing, collapse = ", "), ". Run the producing stages, then export."))
}
manifest <- file.path(cfg$results, "assets.csv")
verify(file.exists(manifest), message = paste0("Missing export manifest: ", manifest,
    ". Run the export stage after completing the analyses."))
assets <- read.csv(manifest)
verify(sum(assets$kind=="Figure")==21L, sum(assets$kind=="Table")==13L,
    message = "Export manifest must contain 21 figures and 13 tables; rerun export.")
for (i in seq_len(nrow(assets))) {
    path <- file.path(cfg$results,assets$file[i])
    verify(file.exists(path), message = paste0("Missing exported asset: ", path))
    verify(digest::digest(file=path,algo="sha256")==assets$sha256[i],
        message = paste0("Exported asset differs from its manifest checksum: ", path,
            ". Rerun export to synchronise files and manifest."))
}
verify(all(file.exists(file.path(cfg$output,"provenance",
    paste0(c("static","dynamic","features","diagnostics","sensitivity",
             "frequency","activation","threshold_transfer","export"),"_session.txt")))),
    message = "One or more stage session records are missing; complete all nine workflow stages.")

# These are the study findings, not additional statistical criteria.
regional <- read_output("features/regional_posthoc/regional_summary.csv")
regional <- regional[match(c("50-200 RPM","250-350 RPM","400-500 RPM"),regional$region),]
verify(identical(regional$simultaneous_lower_bound,c(16L,8L,1L)),
          max(abs(regional$largest_adjusted_p_established-
                  c(.0301122217897233,.0454473472841306,2.22861699939472e-12)))<1e-12,
    message = "Regional equivalence counts or adjusted p values differ from the validated study findings; inspect features/regional_posthoc/regional_summary.csv.")
timing <- read_output("timing_sensitivity/summary.csv")
pairing <- timing[timing$assessment=="Pooled versus within-stage reference pairing",]
verify(pairing$comparisons==24L,pairing$decisions_retained==24L,
          pairing$directions_retained==24L,
    message = "Reference-pairing sensitivity differs from the validated 24-comparison result; inspect timing_sensitivity/summary.csv.")
orientation <- read_output("features/context/orientation_summary.csv")
verify(nrow(orientation)==120L,all(orientation$n==15L),
          any(grepl("^Gravity-vector",orientation$metric)),
    message = "Gravity-vector context row counts, unit counts or metric labels differ; inspect features/context/orientation_summary.csv.")

# Independently check the worst-device summary against its device-level inputs.
performance <- read_output("frequency/tables/method_by_device_performance.csv")
summary <- read_output("frequency/tables/method_comparison_summary.csv")
for (i in seq_len(nrow(summary))) {
    values <- performance$mean_abs_error_hz[performance$method==summary$method[i]]
    verify(length(values)==4L,
              abs(summary$mean_abs_error_hz[i]-mean(values))<1e-10,
              abs(summary$worst_device_mae_hz[i]-max(values))<1e-10,
        message = paste0("Frequency summary is inconsistent with its four device-level inputs: ", summary$method[i]))
}
verify(all(c("all_60_passing_10","response_pass_10")%in%names(summary)),
    message = "Frequency summary lacks the required equivalence/response decision columns.")
cat("PASS: all registered outputs and hashes, nine-stage provenance, regional\n",
    "and timing findings, gravity-vector diagnostic and frequency summary checks.\n",sep="")
