##############################################################################################################
#### This R script multiplies annual gbm flammability maps by scalar coefficient which vary through time. ####
##############################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/18/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("mapset")) stop("Argument 'mapset' not passed at command line.") # Currently must be "3models_tif" or "5models_tif"
if(!exists("cp2scratch")) cp2scratch <- TRUE
if(!exists("cp_originals")) cp_originals <- TRUE

setwd(file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability", period, model, mapset))
dir.create(outDir <- paste0("../", mapset, "_scaled"), showWarnings=FALSE)
if(cp2scratch){
	dir.create(outDir2a <- file.path("/big_scratch/mfleonawicz/Alf_Files_20121129/gbmFlamMaps", period, model, mapset), recursive=TRUE, showWarnings=FALSE)
	dir.create(outDir2b <- paste0(outDir2a, "_scaled"), showWarnings=FALSE)
} else outDir2b <- NULL
if(!cp_originals) outDir2a <- NULL

# load scalars data frame for observed years
load("../../../../../workspaces/gbmFlammability/ALF_ignit_premult.RData")

# seteup
library(raster)
files <- list.files(pattern="\\.tif$")
yrs <- as.numeric(gsub("gbm.flamm_", "", gsub(".tif", "", files)))
files <- files[order(yrs)]
yrs <- yrs[order(yrs)]

# sample random coeffcients for unobserved years
set.seed(47)
a <- sample(c(0.05, 0.5, 0.95), length(yrs), prob=c(21/62, 20/62, 21/62), replace=T)

if(all(1950:2011 %in% yrs)) a[yrs >= 1950 & yrs <= 2011] <- ignit.scalar[,2]

library(parallel)
f <- function(i, a, outDir, files, cp.origin=NULL, cp.new=NULL){
	r <- raster(files[i])
	if(!is.null(cp.origin)) writeRaster(r, file.path(cp.origin, files[i]), datatype="FLT4S", overwrite=T)
	r <- a[i]*r
	writeRaster(r, file.path(outDir, files[i]), datatype="FLT4S", overwrite=T)
	if(!is.null(cp.new)) writeRaster(r, file.path(cp.new, files[i]), datatype="FLT4S", overwrite=T)
	print(i)
}

mclapply(1:length(files), f, a=a, outDir=outDir, files=files, cp.origin=outDir2a, cp.new=outDir2b, mc.cores=32)
