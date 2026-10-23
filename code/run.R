# Command-line workflow entry point ----
# Input: stage or inspection flag.
# Output: requested stage execution.

args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
source(file.path(root, "code/config.R"))
source(file.path(root, "code/setup.R"))
cfg <- wam_config(root)
if (!length(args) || identical(args, "--list")) {
    print(wam_stages(), row.names = FALSE)
} else if (identical(args, "--check")) {
    wam_check_setup(cfg)
    cat("Package API, R dependencies and input checksums verified; no analyses run.\n")
} else if (length(args) == 1L) {
    wam_run(args, cfg)
} else stop("Use --list, --check, one stage name, or all.")
