# Character-preserving CSV and text output for publication assets.
wam_read_csv <- function(path) {
    read.csv(path, colClasses = "character", check.names = FALSE,
             na.strings = NULL, stringsAsFactors = FALSE)
}

wam_write_csv <- function(x, path) {
    encode <- function(x) {
        if (is.logical(x)) x <- ifelse(x, "True", "False")
        else if (is.numeric(x)) x <- sprintf("%.17g", x)
        x <- as.character(x)
        stopifnot(!anyNA(x))
        quoted <- grepl('[,"\r\n]', x)
        x[quoted] <- paste0('"', gsub('"', '""', x[quoted], fixed = TRUE), '"')
        x
    }
    columns <- lapply(x, encode)
    rows <- vapply(seq_len(nrow(x)), function(i)
        paste(vapply(columns, `[`, character(1), i), collapse = ","), character(1))
    lines <- c(paste(encode(names(x)), collapse = ","), rows)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), path)
    invisible(path)
}

wam_write_text <- function(text, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(gsub("\r?\n", "\r\n", text))), path)
    invisible(path)
}
