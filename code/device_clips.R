# Six-panel mounting-clip figure using the supplied PNG thumbnails and model dimensions.
wam_device_clips <- function(folder, output) {
    records <- wam_read_csv(file.path(folder, "dimensions.csv"))
    stopifnot(nrow(records) == 6L)
    images <- lapply(records$file, function(name) png::readPNG(file.path(folder,
        sub("[.]3mf$", ".png", name)), native = TRUE))
    draw <- function() {
        grid::grid.newpage()
        for (i in seq_len(nrow(records))) {
            column <- (i - 1L) %% 3L; row <- (i - 1L) %/% 3L
            centre <- (column + .5) / 3
            top <- 1 - row / 2
            grid::grid.text(paste0(LETTERS[i], ". ", records$device[i]),
                x = (column + .07) / 3, y = top - .035, just = c("left", "centre"),
                gp = grid::gpar(fontfamily = "Arial", fontface = "bold", fontsize = 12))
            grid::grid.raster(images[[i]], x = centre, y = top - .267,
                width = grid::unit(3.48, "in"), height = grid::unit(3.48, "in"), interpolate = TRUE)
            grid::grid.text(paste0("H ", records$height_mm[i], " \u00d7 W ", records$width_mm[i],
                " \u00d7 D ", records$depth_mm[i], " mm"), x = centre, y = top - .48,
                gp = grid::gpar(fontfamily = "Arial", fontsize = 10))
        }
    }
    wam_save_grid_figure(draw, output, 11.7, 8.3, 300)
    invisible(records)
}
