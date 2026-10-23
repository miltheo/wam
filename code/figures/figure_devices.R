# Shared graphics output devices ----
# Input: requested figure dimensions.
# Output: raster/vector graphics devices.

wam_save_grid_figure <- function(draw, output, width, height, dpi) {
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    ragg::agg_png(output, width = width, height = height, units = "in", res = dpi, background = "white")
    tryCatch(draw(), finally = grDevices::dev.off())
    pdf <- sub("[.]png$", ".pdf", output)
    grDevices::cairo_pdf(pdf, width = width, height = height, family = "Arial", bg = "white")
    tryCatch(draw(), finally = grDevices::dev.off())
    invisible(c(png = output, pdf = pdf))
}
