# Spectral-response diagnostic figure ----
# Input: spectral observations and settings.
# Output: spectral diagnostic panels.

wam_render_spectral <- function(master, output) {

    # Read figure input tables ----
    d <- read.csv(master)
    devices <- c("AX3", "GENEActiv", "LEAP", "SENS")
    labels <- c(AX3 = "AX3", GENEActiv = "GENEActiv", LEAP = "LEAP", SENS = "SENS motion")
    colours <- c(AX3 = "#56B4E9", GENEActiv = "#009E73", LEAP = "#CC79A7", SENS = "#E69F00")
    q95 <- vapply(devices, function(device) 0.95 * median(d$fs_hz[d$instrument == device])/2, numeric(1))
    stopifnot(all(is.finite(q95) & q95 > 0))
    # Preserve native-unit display bounds.
    panel <- function(x, y, width, height, xlim, ylim, xticks, yticks, xlabels, ylabels, xlabel, ylabel = NULL,
        xsize = 8, ygrid = TRUE) {
        grid::pushViewport(grid::viewport(x = x, y = y, width = width, height = height, just = c("left",
            "bottom"), xscale = xlim, yscale = ylim, clip = "off"))
        grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
        for (v in xticks) grid::grid.lines(grid::unit(c(v, v), "native"), grid::unit(ylim, "native"),
            gp = grid::gpar(col = "#E5E5E5", lwd = 0.6))
        if (ygrid)
            for (v in yticks) grid::grid.lines(grid::unit(xlim, "native"), grid::unit(c(v, v), "native"),
                gp = grid::gpar(col = "#E5E5E5", lwd = 0.6))
        grid::grid.segments(x0 = grid::unit(xticks, "native"), x1 = grid::unit(xticks, "native"), y0 = grid::unit(0,
            "npc"), y1 = grid::unit(-1.2, "mm"), gp = grid::gpar(lwd = 0.7))
        grid::grid.segments(x0 = grid::unit(0, "npc"), x1 = grid::unit(-1.2, "mm"), y0 = grid::unit(yticks,
            "native"), y1 = grid::unit(yticks, "native"), gp = grid::gpar(lwd = 0.7))
        # Separate highest-frequency endpoint labels.
        for (j in seq_along(xticks)) grid::grid.text(xlabels[j], x = grid::unit(xticks[j], "native"),
            y = grid::unit(-2.5, "mm"), just = c(if (length(xticks) == 6L && j == 5L) "right" else if (length(xticks) ==
                6L && j == 6L) "left" else "centre", "top"), gp = grid::gpar(fontfamily = "Arial", fontsize = xsize))
        grid::grid.text(ylabels, x = grid::unit(-2.2, "mm"), y = grid::unit(yticks, "native"), just = "right",
            gp = grid::gpar(fontfamily = "Arial", fontsize = 8))
        grid::grid.text(xlabel, y = grid::unit(-7.8, "mm"), gp = grid::gpar(fontfamily = "Arial", fontsize = 9))
        if (!is.null(ylabel))
            grid::grid.text(ylabel, x = grid::unit(-9, "mm"), rot = 90, gp = grid::gpar(fontfamily = "Arial",
                fontsize = 9))
    }
    segment <- function(x0, x1, y0, y1, colour, width = 1, type = "solid") grid::grid.lines(grid::unit(c(x0,
        x1), "native"), grid::unit(c(y0, y1), "native"), gp = grid::gpar(col = colour, lwd = width, lty = type,
        lineend = "butt"))
    draw <- function() {
        grid::grid.newpage()
        grid::grid.text("A. Condition-resolved principal spectral peaks", x = 0.08, y = 0.975, just = "left",
            gp = grid::gpar(fontfamily = "Arial", fontsize = 11, fontface = "bold"))
        width <- 0.905/4.54
        gap <- width * 0.18
        total_height <- 0.78/2.52 * 2
        top_height <- total_height * 1.25/2.15
        bottom_height <- total_height * 0.9/2.15
        for (i in seq_along(devices)) {
            device <- devices[i]
            x <- d[d$instrument == device, ]
            left <- 0.08 + (i - 1L) * (width + gap)
            ylim <- c(0, max(13, if (q95[i] < 14) q95[i] + 0.8 else 13))
            panel(left, 0.89 - top_height, width, top_height, c(0.55, 8.65), ylim, c(0.83, 2.5, 4.17,
                5.83, 7.5, 8.33), seq(0, 12, 2), c("0.83", "2.50", "4.17", "5.83", "7.5", "8.33"), as.character(seq(0,
                12, 2)), "Programmed frequency (Hz)", if (i == 1L)
                "Observed principal peak (Hz)")
            grid::grid.text(paste0(labels[device], "\n95% Nyquist = ", sprintf("%.1f", q95[i]), " Hz"),
                y = grid::unit(1, "npc") + grid::unit(6, "mm"), gp = grid::gpar(fontfamily = "Arial",
                  fontsize = 10, fontface = "bold", lineheight = 1.15))
            segment(0.6, 8.6, 0.6, 8.6, "black", 1, "dashed")
            segment(0.55, 8.65, 10, 10, "#0072B2", 1.3)
            if (q95[i] <= ylim[2])
                segment(0.55, 8.65, q95[i], q95[i], "#666666", 1, "dotted")
            for (j in seq_along(c("ax_g", "ay_g", "az_g"))) {
                z <- x[x$axis == c("ax_g", "ay_g", "az_g")[j], ]
                grid::grid.points(grid::unit(z$rpm/60, "native"), grid::unit(z$peak_hz, "native"), pch = c(21,
                  22, 24)[j], size = grid::unit(1.85, "mm"), gp = grid::gpar(col = "white", fill = grDevices::adjustcolor(colours[device],
                  0.92), lwd = 0.55))
            }
            grid::grid.rect(gp = grid::gpar(fill = NA, col = "black", lwd = 0.8))
            grid::popViewport()
        }
        grid::grid.text("B. Universal cut-off feasibility across device sampling rates", x = 0.08, y = 0.405,
            just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 11, fontface = "bold"))
        ticks <- c(0, 9, 10, 11, 20, 26, 30, 40, 50, 60)
        panel(0.08, 0.11, 0.905, bottom_height, c(0, 65), c(-0.75, 3.95), ticks, 0:3, as.character(ticks),
            unname(rev(labels)), "Frequency (Hz)", xsize = 7, ygrid = FALSE)
        grid::grid.rect(x = grid::unit((0.83 + 8.33)/2, "native"), width = grid::unit(8.33 - 0.83, "native"),
            gp = grid::gpar(fill = grDevices::adjustcolor("#D9D9D9", 0.55), col = NA))
        for (v in c(9, 10, 11, 26)) segment(v, v, -0.75, 3.95, if (v == 10)
            "#0072B2" else if (v == 26)
            "#D55E00" else "#999999", if (v == 10)
            1.8 else if (v == 26)
            1.5 else 1, if (v == 10)
            "solid" else if (v == 26)
            "dotted" else "dashed")
        for (i in seq_along(devices)) {
            y <- 4 - i
            segment(0, q95[i], y, y, grDevices::adjustcolor(colours[i], 0.75), 8)
            grid::grid.points(grid::unit(q95[i], "native"), grid::unit(y, "native"), pch = "|", size = grid::unit(4.7,
                "mm"), gp = grid::gpar(col = "black", lwd = 1.2))
            grid::grid.text(paste0(sprintf("%.1f", q95[i]), " Hz"), x = grid::unit(q95[i] + 0.9, "native"),
                y = grid::unit(y, "native"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 8))
        }
        segment(10.2, 13.5, 3.48, 3.48, "#0072B2", 0.9)
        grid::grid.text("Selected 10-Hz cutoff", x = grid::unit(13.8, "native"), y = grid::unit(3.48,
            "native"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 8, col = "#0072B2"))
        segment(26.2, 30, 0, 0, "#D55E00", 0.9)
        grid::grid.text("26 Hz invalid for SENS motion", x = grid::unit(30.3, "native"), y = grid::unit(0,
            "native"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 8, col = "#D55E00"))
        grid::grid.rect(gp = grid::gpar(fill = NA, col = "black", lwd = 0.8))
        grid::popViewport()
        legend <- c("Programmed frequency", "Observed axis peak", "9- and 11-Hz candidates", "Selected 10-Hz cutoff",
            "95% Nyquist limit", "26-Hz sensitivity cutoff")
        starts <- c(0.025, 0.205, 0.361, 0.551, 0.718, 0.855)
        for (i in seq_along(legend)) {
            col <- c("black", "#777777", "#999999", "#0072B2", "#666666", "#D55E00")[i]
            if (i == 2L)
                grid::grid.circle(x = grid::unit(starts[i], "npc"), y = grid::unit(0.024, "npc"), r = grid::unit(0.65,
                  "mm"), gp = grid::gpar(col = NA, fill = col)) else grid::grid.lines(x = c(starts[i] - 0.008, starts[i] + 0.012), y = c(0.024, 0.024), gp = grid::gpar(col = col,
                lwd = if (i == 4L)
                  1.8 else 1, lty = c("dashed", "solid", "dashed", "solid", "dotted", "dotted")[i]))
            grid::grid.text(legend[i], x = starts[i] + 0.018, y = 0.024, just = "left", gp = grid::gpar(fontfamily = "Arial",
                fontsize = 7.1))
        }
    }
    wam_save_grid_figure(draw, output, 10.4, 7.4, 600)
    invisible(list(observations = d, programmed_hz = d$rpm/60, q95_hz = q95, cutoffs_hz = c(9, 10, 11,
        26)))
}
