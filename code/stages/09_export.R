# Run after documented prerequisites.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- dirname(dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE))))
source(file.path(root, "code/config.R"))
source(file.path(root, "code/setup.R"))
wam_run("export", wam_config(root))
