# Final assets are copied/rendered from this run, never from frozen publication files.
wam_condition_mapping <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(tidyr)})
    x <- read.csv(file.path(cfg$output,"features/primary_native/unit_stage.csv")) |>
        filter(metric%in%c("ENMO","MAD")) |> group_by(rpm,metric) |>
        summarise(observed_median_mg=median(value),.groups="drop") |>
        pivot_wider(names_from=metric,values_from=observed_median_mg)
    design <- read.csv(file.path(cfg$root,"config/condition_mapping.csv"))
    write.csv(left_join(design,x,by="rpm"),file.path(cfg$output,"features/condition_mapping.csv"),row.names=FALSE)
}

wam_export <- function(cfg) {
    # Current-run inference remains in analysis/; only selected final assets go
    # into results/. No file is written into the manuscript or package folders.
    feature_out <- file.path(cfg$output,"features/tables")
    dir.create(feature_out,recursive=TRUE,showWarnings=FALSE)
    call_python <- function(script, ...) {
        status <- system2(cfg$python,c(shQuote(file.path(cfg$root,"tools",script)),
                                       vapply(list(...),shQuote,character(1))))
        if(status != 0L)stop("Export failed: ",script)
    }
    call_python("feature_tables.py",file.path(cfg$output,"features"),feature_out)
    call_python("device_clips.py",file.path(cfg$root,"hardware/device-clips"),
                file.path(cfg$output,"hardware/device_clips.png"))
    # Condensed sensitivity evidence for the separate reference-timing table.
    pairing <- read.csv(file.path(cfg$output,"timing_sensitivity/pairing/device_comparisons.csv"))
    events <- read.csv(file.path(cfg$output,"timing_sensitivity/event/comparison_changes.csv"))
    events <- events[events$baseline=="index_stage",]
    row <- function(label,changed,direction) data.frame(assessment=label,comparisons=length(changed),
        decisions_retained=sum(!changed),directions_retained=sum(!direction))
    summary <- row("Pooled versus within-stage reference pairing",pairing$significance_changed,pairing$direction_changed)
    for(metric in unique(events$metric)) {
        d <- events[events$metric==metric,]
        summary <- rbind(summary,row(paste("Apparent-event alignment:",metric),d$significance_changed,d$direction_changed))
    }
    write.csv(summary,file.path(cfg$output,"timing_sensitivity/summary.csv"),row.names=FALSE)
    call_python("export_assets.py",cfg$output,cfg$results)
}
