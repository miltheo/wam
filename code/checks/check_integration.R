# Package and workflow integration checks ----
# Input: installed package and sources.
# Output: compatibility assertions.

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE))))
source(file.path(root, "code/config.R"))
source(file.path(root, "code/setup.R"))
cfg <- wam_config(root)
wam_check_setup(cfg)
for (path in list.files(file.path(root, "code"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE)) parse(path)
e <- new.env(parent = globalenv())
wam_load_code(cfg, e)

# Check installed table/bundle interfaces.
t <- as.POSIXct("2026-01-01", tz = "UTC") + (0:999)/100
ref <- 1 + sin(2 * pi * (0:999)/100) * 0.1
test <- 0.02 + 1.1 * ref
x <- metronautica::as_accel_tbl(t, test, rep(0, 1000), rep(0, 1000), list(acquisition = list(fs_derived_hz = 100,
    fs_nominal_hz = 100, full_scale_range_g = 16)))
b <- list(data = x, meta = metronautica::accel_meta(x))
stopifnot(identical(metronautica::qc_accel_tbl(x), metronautica::qc_accel_tbl(b)), metronautica::qc_accel_tbl(b)$ruleset ==
    "primary-five")
response <- metronautica::m_dynamic_response(b, ref, 16)
stopifnot(abs(response$response_slope - 1.1) < 1e-12, abs(response$response_intercept - 0.02) < 1e-12)

# Use prespecified joint p values.
# Only the first rejection survives.
metrics <- sprintf("M%02d", 1:16)
make_arm <- function(n_pairs, divisor) {
    d <- expand.grid(metric = metrics, rpm = seq(50, 500, 50), pair = seq_len(n_pairs), stringsAsFactors = FALSE)
    region <- ifelse(d$rpm <= 200, 0L, ifelse(d$rpm <= 350, 1L, 2L))
    d$margin_pct <- 10
    d$p_equivalence <- (match(d$metric, metrics) + region * 16)/divisor
    d
}
z <- e$wam_regional_equivalence_values(make_arm(6, 1000), make_arm(3, 2000))
stopifnot(sum(z$equivalence_established) == 1L, abs(min(z$p_holm_all48) - 0.048) < 1e-14, abs(sort(z$p_holm_all48)[2] -
    0.094) < 1e-14, all(z$p_both == z$p_equivalence_primary))

# Load without creating analysis outputs.
# Check master integrity separately.
stopifnot(!exists("wam_study_runtime", envir = e, inherits = FALSE))
cat("PASS: input checksums, all R syntax, installed package API, table/bundle diagnostics,\n", "known dynamic response, and independent regional/Holm contracts.\n",
    sep = "")
