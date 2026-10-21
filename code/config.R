# Change environment variables to relocate inputs/intermediates/final assets.
# No original recordings or previous analysis trees are required by this workflow.
wam_config <- function(root = getwd()) {
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    list(root = root,
         masters = Sys.getenv("WAM_MASTER_ROOT", file.path(root, "data")),
         output = Sys.getenv("WAM_ANALYSIS_ROOT", file.path(root, "analysis")),
         results = Sys.getenv("WAM_RESULTS_ROOT", file.path(root, "results")))
}
