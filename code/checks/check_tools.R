# Input integrity, source syntax, export formatting and dependency contracts.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE))))
source(file.path(root, "code/config.R")); source(file.path(root, "code/setup.R"))
cfg <- wam_config(root)
wam_check_setup(cfg)
for (p in list.files(file.path(root, "code"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE)) parse(p)
e <- new.env(parent = globalenv()); wam_load_code(cfg, e)
index <- read.csv(file.path(root, "code/settings/inputs.csv"))
stopifnot(all(file.info(file.path(cfg$masters, index$file))$size == index$bytes))
figures <- read.csv(file.path(root, "code/settings/figures.csv"))
tables <- read.csv(file.path(root, "code/settings/tables.csv"))
stopifnot(nrow(figures) == 21L, nrow(tables) == 13L,
          !anyDuplicated(figures$number), !anyDuplicated(tables$number))
row <- data.frame(region = "50-200 RPM", pipeline = "Example", p_equivalence_primary = ".001",
    p_equivalence_secondary = ".002", p_both = ".002", p_holm_all48 = ".048", equivalence_established = "TRUE")
s <- e$wam_table_fragment("S14", "Post hoc regional equivalence", row)
stopifnot(grepl(r"(\multicolumn{6})", s, fixed = TRUE),
          grepl(r"(\textbf{50-200 RPM})", s, fixed = TRUE), e$wam_ptext(".00001") == "<0.001",
          e$wam_tex("a_b & 10%") == r"(a\_b \& 10\%)")
local({
    path <- tempfile(fileext = ".csv"); on.exit(unlink(path))
    x <- data.frame(a = c("one,two", 'a"b', "line\nbreak"), b = c("0.12345678901234567", "TRUE", ""))
    e$wam_write_csv(x, path)
    stopifnot(identical(e$wam_read_csv(path), x))
})
for (p in list.files(file.path(root, "code"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE)) {
    if (startsWith(basename(p), "check_")) next
    text <- paste(readLines(p, warn = FALSE), collapse = "\n")
    stopifnot(!grepl("setwd(", text, fixed = TRUE), !grepl("rm(list", text, fixed = TRUE),
              !grepl("inst/engine", text, fixed = TRUE))
}
stopifnot(!length(list.files(file.path(root, "code"), pattern = "[.]py$")),
          !any(dir.exists(file.path(root, c("tests", "config", "docs")))),
          !exists("script", envir = e, inherits = FALSE),
          !file.exists(file.path(root, "manuscript")),
          !file.exists(file.path(root, "code/signal_features.R")),
          all(dir.exists(file.path(root, c("code/analysis", "code/checks", "code/settings", "3d-clips/previews")))))
stopifnot("/samples/*.gz" %in% readLines(file.path(root, ".gitignore")),
          "data/**/*.csv -text" %in% readLines(file.path(root, ".gitattributes")))
cat("PASS: R syntax, master integrity, asset registries, CSV/LaTeX formatting and R-only dependencies.\n")
