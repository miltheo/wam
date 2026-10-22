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

# Display-only gravity typography; machine-readable names and values are unchanged.
wam_gravity_labels <- function(labels) {
    one_line <- function(text) {
        pattern <- "(?<![[:alnum:]_])(micro-g|ug|mg|g)(?![[:alnum:]_])"
        hits <- gregexpr(pattern, text, perl = TRUE)[[1L]]
        if (hits[1L] < 0L) return(encodeString(text, quote = '"'))
        widths <- attr(hits, "match.length"); parts <- character(); start <- 1L
        for (i in seq_along(hits)) {
            if (hits[i] > start) parts <- c(parts, encodeString(substr(text, start, hits[i]-1L), quote = '"'))
            unit <- substr(text, hits[i], hits[i]+widths[i]-1L)
            parts <- c(parts, switch(unit, g = "italic(g)", mg = "plain(m)*italic(g)",
                ug = "mu*italic(g)", `micro-g` = "mu*italic(g)"))
            start <- hits[i]+widths[i]
        }
        if (start <= nchar(text)) parts <- c(parts, encodeString(substring(text,start),quote='"'))
        paste(parts,collapse="*")
    }
    code <- vapply(labels, function(text) {
        lines <- vapply(strsplit(text, "\n", fixed = TRUE)[[1L]], one_line, character(1))
        out <- tail(lines,1L)
        if (length(lines)>1L) for(i in rev(seq_len(length(lines)-1L)))
            out <- paste0("atop(",lines[i],",",out,")")
        out
    }, character(1), USE.NAMES=FALSE)
    parse(text=code)
}
