## The script for running each module individually 

source("global.R")

module_names <- list.files("R")
lapply(module_names, function(x) source(paste("R", x, sep = "/")))

scatterplotApp()

pcaApp()
