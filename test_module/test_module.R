## The script for running each module individually 

source("global.R")

module_names <- c("scatterplot.R")
lapply(module_names, function(x) source(paste("R", x, sep = "/")))

scatterplotApp()
