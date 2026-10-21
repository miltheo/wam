# Check the products of a complete run; no models or figures are regenerated.
source_path <- if (sys.nframe()) sys.frame(1)$ofile else NULL
script <- if (!is.null(source_path)) source_path else
    sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE)[1L])
root <- dirname(dirname(normalizePath(script,winslash="/",mustWork=TRUE)))
source(file.path(root,"code/config.R"))
cfg <- wam_config(root)
read_output <- function(path) read.csv(file.path(cfg$output,path),check.names=FALSE)

# The registries must resolve entirely to newly generated products.
for (registry in c("figures","tables")) {
    index <- read.csv(file.path(root,"code",paste0(registry,".csv")))
    stopifnot(all(file.exists(file.path(cfg$output,index$source))))
}
assets <- read.csv(file.path(cfg$results,"assets.csv"))
stopifnot(sum(assets$kind=="Figure")==21L, sum(assets$kind=="Table")==13L)
for (i in seq_len(nrow(assets))) {
    path <- file.path(cfg$results,assets$file[i])
    stopifnot(digest::digest(file=path,algo="sha256")==assets$sha256[i])
}
stopifnot(all(file.exists(file.path(cfg$output,"provenance",
    paste0(c("static","dynamic","features","diagnostics","sensitivity",
             "frequency","activation","threshold_transfer","export"),"_session.txt")))))

# These are the study findings, not additional statistical criteria.
regional <- read_output("features/regional_posthoc/regional_summary.csv")
regional <- regional[match(c("50-200 RPM","250-350 RPM","400-500 RPM"),regional$region),]
stopifnot(identical(regional$simultaneous_lower_bound,c(16L,8L,1L)),
          max(abs(regional$largest_adjusted_p_established-
                  c(.0301122217897233,.0454473472841306,2.22861699939472e-12)))<1e-12)
timing <- read_output("timing_sensitivity/summary.csv")
pairing <- timing[timing$assessment=="Pooled versus within-stage reference pairing",]
stopifnot(pairing$comparisons==24L,pairing$decisions_retained==24L,
          pairing$directions_retained==24L)
orientation <- read_output("features/context/orientation_summary.csv")
stopifnot(nrow(orientation)==120L,all(orientation$n==15L),
          any(grepl("^Gravity-vector",orientation$metric)))

# Independently check the worst-device summary against its device-level inputs.
performance <- read_output("frequency/tables/method_by_device_performance.csv")
summary <- read_output("frequency/tables/method_comparison_summary.csv")
for (i in seq_len(nrow(summary))) {
    values <- performance$mean_abs_error_hz[performance$method==summary$method[i]]
    stopifnot(length(values)==4L,
              abs(summary$mean_abs_error_hz[i]-mean(values))<1e-10,
              abs(summary$worst_device_mae_hz[i]-max(values))<1e-10)
}
stopifnot(all(c("all_60_passing_10","response_pass_10")%in%names(summary)))
cat("PASS: all registered outputs and hashes, nine-stage provenance, regional\n",
    "and timing findings, gravity-vector diagnostic and frequency summary checks.\n",sep="")
