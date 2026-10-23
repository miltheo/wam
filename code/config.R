# Repository path configuration ----
# Input: root and environment overrides.
# Output: resolved workflow paths.

wam_config <- function(root = getwd()) {
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    list(root = root, masters = Sys.getenv("WAM_MASTER_ROOT", file.path(root, "data")), output = Sys.getenv("WAM_ANALYSIS_ROOT",
        file.path(root, "analysis")), results = Sys.getenv("WAM_RESULTS_ROOT", file.path(root, "results")))
}
