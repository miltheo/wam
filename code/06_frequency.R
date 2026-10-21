# Run this stage independently after its documented prerequisites.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash="/", mustWork=TRUE)))
source(file.path(root,"code/config.R"))
source(file.path(root,"code/setup.R"))
wam_run("frequency", wam_config(root))
