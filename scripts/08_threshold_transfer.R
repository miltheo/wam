# Run this stage independently after its documented prerequisites.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash="/", mustWork=TRUE)))
source(file.path(root,"config.R"))
source(file.path(root,"R/setup.R"))
wam_run("threshold_transfer", wam_config(root))
