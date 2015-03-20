#######################################################################################################################
#### This R script cyclically duplicates flammability maps to simulate a backcast for use in longer ALFRESCO runs. ####
#######################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/19/2015        ####

# @knitr duplicate
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("mapset")) stop("Argument 'mapset' not passed at command line.") # Currently must be "3models_tif" or "5models_tif"

setwd(file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability", period, model, mapset))
files <- list.files(pattern="\\.tif$", full=TRUE)

f1 <- function(x, n, period){
	yr <- as.numeric(substr(x, nchar(x)-7, nchar(x)-4))
	yr <- yr - length(period)*(1:n)
	x.out <- paste0(substr(x, 1, nchar(x)-8), yr, ".tif")
	for(i in 1:n) if(yr[i] >= -1) file.copy(x, x.out[i]) # allow for one negative year for testing
	return()
}

f1v <- Vectorize(f1, "x")

f1v(x=files, n=18, period=1901:2009)
