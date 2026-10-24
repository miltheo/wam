# Article-asset index generation ----
# Input: figure/table registries.
# Output: repository output index.

wam_write_output_index <- function(root = getwd()) {
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    settings <- file.path(root, "code", "settings")
    figures <- read.csv(file.path(settings, "figures.csv"), colClasses = "character")
    tables <- read.csv(file.path(settings, "tables.csv"), colClasses = "character")
    prefix_stage <- c(static = "static", dynamic_native = "dynamic", dynamic_processing = "dynamic",
        secondary_native = "dynamic", features = "features", diagnostics = "diagnostics", threshold_transfer = "threshold_transfer",
        frequency = "frequency", activation = "activation", configuration = "sensitivity", timing_sensitivity = "sensitivity",
        hardware = "export")
    stage <- function(source) {
        value <- unname(prefix_stage[sub("/.*", "", source)])
        if (anyNA(value))
            stop("Unmapped output prefix; update the documentation stage map.")
        value[startsWith(source, "features/tables/") | source == "timing_sensitivity/summary.csv"] <- "export"
        value
    }
    entries <- list.files(file.path(root, "code", "stages"), pattern = "[.]R$")
    stages <- sub("[.]R$", "", sub("^[0-9]+_", "", entries))
    link_stage <- function(value) {
        entry <- entries[match(value, stages)]
        if (anyNA(entry))
            stop("Stage entry point missing for output index.")
        paste0("[`", value, "`](stages/", entry, ")")
    }
    escape <- function(x) gsub("|", "\\|", x, fixed = TRUE)
    lines <- c("# Article output index", "",
        "Generated from [figures.csv](settings/figures.csv) and [tables.csv](settings/tables.csv).",
        "Run the [required stages](README.md#stages), then `export`.",
        "Figures are written to `results/figures/`, tables to `results/tables/` and",
        "checksums to `results/assets.csv`. Only registered assets are exported.",
        "", "## Figures", "", "| Asset | Description | Producing stage | Exported file |", "|---|---|---|---|")
    for (i in seq_len(nrow(figures))) {
        row <- figures[i, ]
        lines <- c(lines, paste0("| Figure ", row$number, " | ", escape(row$title), " | ",
            link_stage(stage(row$source)), " | `figure-", row$number, ".png` |"))
    }
    lines <- c(lines, "", "Available PDFs are copied beside figure PNGs.", "", "## Tables", "",
        "| Asset | Description | Producing stage | Exported file |", "|---|---|---|---|")
    for (i in seq_len(nrow(tables))) {
        row <- tables[i, ]
        lines <- c(lines, paste0("| Table ", row$number, " | ", escape(row$title), " | ",
            link_stage(stage(row$source)), " | `table-", row$number, ".csv` |"))
    }
    lines <- c(lines, "", "Each CSV also has a `.tex` fragment.", "",
        "<details>", "<summary>Intermediate paths</summary>", "",
        "Paths are relative to the repository root with default output settings.", "",
        "| Asset | Intermediate source |", "|---|---|")
    for (i in seq_len(nrow(figures)))
        lines <- c(lines, paste0("| Figure ", figures$number[i], " | `analysis/", figures$source[i], "` |"))
    for (i in seq_len(nrow(tables)))
        lines <- c(lines, paste0("| Table ", tables$number[i], " | `analysis/", tables$source[i], "` |"))
    lines <- c(lines, "", "The export stage assembles feature tables, the clip preview figure and the",
        "condensed timing-sensitivity summary from completed analyses.", "", "</details>")
    path <- file.path(root, "code", "OUTPUTS.md")
    writeLines(enc2utf8(lines), path, useBytes = TRUE)
    invisible(path)
}
