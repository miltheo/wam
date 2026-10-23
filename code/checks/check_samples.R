# Sample catalogue integrity checks ----
# Input: catalogue and optional attachments.
# Output: filename/checksum assertions.

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE))))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && !identical(args, "--require-all")) stop("Use --require-all or no arguments.")
samples <- file.path(root, "samples")
index <- read.csv(file.path(samples, "sample_index.csv"), colClasses = "character", check.names = FALSE)
stopifnot(nrow(index) == 9L, !anyDuplicated(paste(index$device, index$type)))
verify <- function(row) {
    storage <- file.path(samples, row$storage_file)
    stopifnot(file.info(storage)$size == as.numeric(row$storage_bytes), digest::digest(file = storage,
        algo = "sha256") == row$storage_sha256)
    restored <- tempfile("wam-native-")
    on.exit(unlink(restored), add = TRUE)
    input <- gzfile(storage, "rb")
    on.exit(close(input), add = TRUE)
    output <- file(restored, "wb")
    tryCatch(repeat {
        block <- readBin(input, "raw", n = 4L * 1024L * 1024L)
        if (!length(block))
            break
        writeBin(block, output)
    }, finally = close(output))
    stopifnot(file.info(restored)$size == as.numeric(row$native_bytes), digest::digest(file = restored,
        algo = "sha256") == row$native_sha256)
}
missing <- character()
checked <- 0L
for (i in seq_len(nrow(index))) {
    row <- index[i, ]
    for (field in c("storage_file", "file_name")) stopifnot(identical(basename(row[[field]]), row[[field]]),
        !grepl("[/\\\\]", row[[field]]))
    if (!file.exists(file.path(samples, row$storage_file))) {
        missing <- c(missing, row$storage_file)
        next
    }
    verify(row)
    checked <- checked + 1L
}
catalogue <- file.path(dirname(root), "Metronautica/inst/extdata/native/sample_index.csv")
if (file.exists(catalogue)) stopifnot(digest::digest(file = catalogue, algo = "sha256") == digest::digest(file = file.path(samples,
    "sample_index.csv"), algo = "sha256"))
if ("--require-all" %in% args && length(missing)) stop("Missing release attachments: ", paste(missing,
    collapse = ", "))
cat("PASS: sample catalogue and", checked, "compressed/restored recordings verified;", length(missing),
    "optional release attachments absent.\n")
