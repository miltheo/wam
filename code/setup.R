# Configuration, package contract and explicit study-stage dispatch.
# Loading this file performs no analysis and does not create output directories.
wam_dependencies <- function() c("metronautica", "dplyr", "tidyr", "tibble",
    "ggplot2", "nlme", "digest", "ragg", "gridExtra", "scales", "ggrepel", "png")

wam_check_setup <- function(cfg, check_inputs = TRUE) {
    lib <- Sys.getenv("WAM_R_LIBRARY")
    if (nzchar(lib)) .libPaths(unique(c(lib, .libPaths())))
    missing <- wam_dependencies()[!vapply(wam_dependencies(), requireNamespace,
                                        logical(1), quietly = TRUE)]
    if (length(missing)) stop("Install required packages first: ", paste(missing, collapse = ", "))
    if (utils::packageVersion("metronautica") < "0.1.0")
        stop("WAM requires metronautica >= 0.1.0.")
    if (utils::packageVersion("dplyr") < "1.1.0") stop("WAM requires dplyr >= 1.1.0.")
    required_api <- c("parse_raw_auto", "accel_meta", "qc_accel_tbl", "clip_bundle",
        "static_metrology_from_tbl", "clip_dynamic_stages", "dynamic_metrology_report",
        "m_dynamic_response", "filter_accel_tbl", "compute_accel_summaries")
    if (!all(required_api %in% getNamespaceExports("metronautica")))
        stop("Installed metronautica lacks the current direct API. Reinstall the companion package.")
    clean <- function(path) tolower(gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = FALSE)))
    protected <- clean(cfg$masters)
    for (path in c(cfg$output, cfg$results)) {
        path <- clean(path)
        if (path == clean(cfg$root) || path == protected || startsWith(path, paste0(protected, "/")))
            stop("Analysis/results paths must not overwrite the repository root or input masters.")
    }
    a <- clean(cfg$output); b <- clean(cfg$results)
    if (a == b || startsWith(a, paste0(b, "/")) || startsWith(b, paste0(a, "/")))
        stop("Analysis and results must be separate, non-nested directories.")
    if (check_inputs) {
        index <- utils::read.csv(file.path(cfg$root, "code", "settings", "inputs.csv"), stringsAsFactors = FALSE)
        for (i in seq_len(nrow(index))) {
            path <- file.path(cfg$masters, index$file[i])
            if (!file.exists(path)) stop("Missing master: ", path)
            if (digest::digest(file = path, algo = "sha256") != index$sha256[i])
                stop("Master checksum mismatch: ", index$file[i])
        }
    }
    invisible(TRUE)
}

wam_load_code <- function(cfg, envir = parent.frame()) {
    paths <- list.files(file.path(cfg$root, "code"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
    modules <- paths[!grepl("^(setup|config|run)[.]R$|^[0-9]{2}_|^check_", basename(paths))]
    for (path in modules) sys.source(path, envir = envir)
    invisible(envir)
}

wam_stages <- function() data.frame(
    stage = c("static", "dynamic", "features", "diagnostics", "sensitivity",
              "frequency", "activation", "threshold_transfer", "export"),
    requires = c("", "", "", "", "dynamic", "features", "", "static,features",
                 "all analysis stages"),
    purpose = c("Static group comparisons and selected figure",
        "Primary/secondary dynamic models and processing comparisons",
        "Feature response, equivalence, regional post hoc tests and figures",
        "Signal/reference-timing summaries and Allan/spectral diagnostics",
        "Configuration, reference-pairing and event-alignment sensitivity",
        "Sixteen supplementary frequency estimators and native tracking",
        "Low-excitation response, activation threshold and deadband",
        "Exploratory static drift and ENMO/MAD/SVMbp threshold transfer",
        "Numbered manuscript result assets from completed analysis outputs"))

wam_run <- function(stage, cfg) {
    if (!stage %in% c("all", wam_stages()$stage)) stop("Unknown stage; use --list.")
    wam_check_setup(cfg)
    env <- new.env(parent = environment(wam_run))
    wam_load_code(cfg, env)
    previous <- options(wam.config = cfg, wam.render_figures = TRUE,
                        device = function(...) ragg::agg_capture())
    on.exit(options(previous), add = TRUE)
    devices_before <- grDevices::dev.list()
    on.exit(for (device in setdiff(grDevices::dev.list(), devices_before))
        try(grDevices::dev.off(device), silent = TRUE), add = TRUE)
    Sys.setenv(TZ = "UTC")
    jobs <- if (stage == "all") wam_stages()$stage else stage
    for (job in jobs) {
        message("WAM: ", job)
        dir.create(cfg$output, recursive = TRUE, showWarnings = FALSE)
        switch(job,
            static = {env$wam_static(cfg); env$wam_static_figure(cfg)},
            dynamic = {
                options(wam.render_figures = FALSE)
                env$wam_dynamic_native(cfg); env$wam_dynamic_secondary(cfg)
                options(wam.render_figures = TRUE)
                env$wam_dynamic_processing(cfg)
                env$wam_dynamic_figure(cfg, "primary"); env$wam_dynamic_figure(cfg, "secondary")
            },
            features = {env$wam_features(cfg); env$wam_regional_equivalence(cfg); env$wam_feature_figures(cfg);
                        env$wam_condition_mapping(cfg); env$wam_orientation_figure(cfg)},
            diagnostics = {env$wam_diagnostics(cfg); env$wam_allan_figures(cfg); env$wam_spectral_figure(cfg)},
            sensitivity = {env$wam_configuration(cfg); env$wam_pairing_sensitivity(cfg); env$wam_event_sensitivity(cfg)},
            frequency = {env$wam_frequency(cfg); env$wam_frequency_figure(cfg); env$wam_frequency_tracking(cfg)},
            activation = {env$wam_activation(cfg); env$wam_activation_figure(cfg)},
            threshold_transfer = env$wam_threshold_transfer(cfg),
            export = env$wam_export(cfg))
        # Record the runtime environment and package API used for each stage.
        provenance <- file.path(cfg$output, "provenance")
        dir.create(provenance, recursive = TRUE, showWarnings = FALSE)
        writeLines(c(paste("metronautica", utils::packageVersion("metronautica")),
            capture.output(sessionInfo())), file.path(provenance, paste0(job, "_session.txt")))
        api <- getNamespaceExports("metronautica")
        hashes <- vapply(api, function(name) {
            f <- getExportedValue("metronautica", name)
            digest::digest(list(formals(f), body(f)), algo = "sha256")
        }, character(1))
        write.csv(data.frame(function_name = api, sha256 = unname(hashes)),
                  file.path(provenance, "metronautica_api.csv"), row.names = FALSE)
    }
    invisible(cfg)
}
