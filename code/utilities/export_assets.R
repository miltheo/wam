# Numbered assets and LaTeX table fragments; all numerical fields remain character strings.
wam_tex <- function(value) {
    chars <- strsplit(as.character(value), "", fixed = TRUE)[[1]]
    map <- setNames(c(r"(\textbackslash{})", r"(\&)", r"(\%)", r"(\_)",
        r"(\#)", r"(\$)", r"(\{)", r"(\})", r"(\newline )"),
        c("\\", "&", "%", "_", "#", "$", "{", "}", "\n"))
    hit <- chars %in% names(map); chars[hit] <- unname(map[chars[hit]])
    text <- paste(chars, collapse = "")
    text <- gsub(r"((?<![[:alnum:]_])(m?)(g)(?![[:alnum:]_]))",
        r"(\1\\textit{g})", text, perl = TRUE)
    gsub(r"((?<![[:alnum:]_])(micro-g|ug)(?![[:alnum:]_]))",
        r"($\\mu$\\textit{g})", text, perl = TRUE)
}

wam_ptext <- function(value) if (as.numeric(value) < .001) "<0.001" else sprintf("%.3f", as.numeric(value))

wam_table_fragment <- function(number, title, rows) {
    if (number == "S14") {
        fields <- c("pipeline", "p_equivalence_primary", "p_equivalence_secondary", "p_both", "p_holm_all48", "equivalence_established")
        labels <- c("Feature pipeline", "Primary p", "Secondary p", "Across-comparison p", "Holm-adjusted p", "Equivalence established")
    } else { fields <- names(rows); labels <- gsub("_", " ", fields, fixed = TRUE) }
    n <- length(fields); landscape <- n > 6L || number == "S14"
    out <- c(if (landscape) r"(\begin{landscape})", r"(\begingroup)", r"(\small)",
        r"(\setlength{\tabcolsep}{3pt})",
        paste0(r"(\begin{longtable}{@{}*{)", n, r"(}{>{\raggedright\arraybackslash}p{\dimexpr\linewidth/)",
            n, r"(-2\tabcolsep\relax}}@{}})"),
        paste0(r"(\caption{\textbf{)", wam_tex(title), r"(}}\label{tab:)", number, r"(}\\)"),
        r"(\toprule)", paste0(paste(vapply(labels, function(x)
            paste0(r"(\textbf{)", wam_tex(x), "}"), character(1)), collapse = " & "), r"(\\)"), r"(\midrule\endhead)")
    region <- NULL
    for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        if (number == "S14" && !identical(row$region, region)) {
            region <- row$region
            out <- c(out, paste0(r"(\multicolumn{6}{l}{\textbf{)", wam_tex(region), r"(}}\\)"))
        }
        values <- vapply(fields, function(f) wam_tex(if (number == "S14" && startsWith(f, "p_"))
            wam_ptext(row[[f]]) else row[[f]]), character(1))
        out <- c(out, paste0(paste(values, collapse = " & "), r"(\\)"))
    }
    out <- c(out, r"(\bottomrule)", r"(\end{longtable})")
    if (number %in% c("4", "S13")) out <- c(out, paste0(
        r"(\noindent E: all device pairs equivalent at all tested conditions at the fixed )",
        r"($\pm10\%$ full-range feature-span margin. R: positive slope confidence interval )",
        r"(and upper 95\% bootstrap limit for response non-linearity $\leq10\%$ for every device model.)"))
    if (number == "S14") out <- c(out, paste0(
        r"(\noindent Pipeline-region p values are intersection-union maxima over the required )",
        "device pairs and stages, then over both native comparisons. Holm adjustment covers all ",
        "48 pipeline-region tests. Repeated trials are averaged within each physical unit before comparison."))
    out <- c(out, r"(\endgroup)", if (landscape) r"(\end{landscape})")
    paste0(paste(out, collapse = "\n"), "\n")
}

wam_export_assets <- function(cfg) {
    figures <- wam_read_csv(file.path(cfg$root, "code/settings/figures.csv"))
    tables <- wam_read_csv(file.path(cfg$root, "code/settings/tables.csv"))
    sources <- c(figures$source, tables$source)
    missing <- sources[!file.exists(file.path(cfg$output, sources))]
    if (length(missing)) stop("Run the missing analysis stages first:\n", paste(missing, collapse = "\n"))
    manifest <- list()
    copy <- function(src, dst) {
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        if (!file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)) stop("Cannot export: ", dst)
    }
    for (i in seq_len(nrow(figures))) {
        source <- file.path(cfg$output, figures$source[i])
        relative <- paste0("figures/figure-", figures$number[i], ".png")
        target <- file.path(cfg$results, relative); copy(source, target)
        pdf <- sub("[.]png$", ".pdf", source)
        if (file.exists(pdf)) copy(pdf, sub("[.]png$", ".pdf", target))
        manifest[[length(manifest) + 1L]] <- data.frame(kind = "Figure", number = figures$number[i],
            file = relative, sha256 = digest::digest(file = target, algo = "sha256"))
    }
    for (i in seq_len(nrow(tables))) {
        source <- file.path(cfg$output, tables$source[i])
        relative <- paste0("tables/table-", tables$number[i], ".csv")
        target <- file.path(cfg$results, relative); copy(source, target)
        rows <- wam_read_csv(source)
        if (tables$number[i] == "S14") rows <- rows[order(match(rows$region,
            c("50-200 RPM", "250-350 RPM", "400-500 RPM")), rows$pipeline, method = "radix"), ]
        wam_write_text(wam_table_fragment(tables$number[i], tables$title[i], rows), sub("[.]csv$", ".tex", target))
        manifest[[length(manifest) + 1L]] <- data.frame(kind = "Table", number = tables$number[i],
            file = relative, sha256 = digest::digest(file = target, algo = "sha256"))
    }
    wam_write_csv(do.call(rbind, manifest), file.path(cfg$results, "assets.csv"))
    invisible(cfg$results)
}
