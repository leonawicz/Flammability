###################################################################################################################
#### This R script applies vegetation-specific flammability distribution truncation to input flammability maps ####
###################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   06/23/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU32", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("samples")) samples <- TRUE
if(!exists("mapset")) stop("Argument 'mapset' not passed at command line.")
if(substr(mapset, 1, 1) == "3") gbm <- 3 else gbm <- 5

# 10th and 90th percentiles of the flammability distributions across space and through time, by vegetation class
q.cavm <- c(0.0180, 0.0449)
q.shrub <- c(0.0078, 0.0216)
q.gram <- c(0.0069, 0.0310)
q.wet <- c(0.0059, 0.0164)

verDir <- if(samples) "samples_based" else "means_based"
setwd(file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability", verDir, period, model, mapset))
dir.create(outDir <- paste0("../", mapset, "_cavmDistTrunc"), showWarnings=FALSE)

library(raster)
library(parallel)

files <- list.files(pattern="\\.tif$")
yrs <- as.numeric(gsub("gbm.flamm_", "", gsub("\\.tif", "", files)))
files <- files[order(yrs)]
yrs <- yrs[order(yrs)]

r.veg <- readAll(raster("../../../../../alf2005.cavm.merged.030212.tif"))
noa.shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2")
ind.cavm <- which(r.veg[] >= 5)
ind.shrub <- which(r.veg[] == 5)
ind.gram <- which(r.veg[] == 6)
ind.wet <- which(r.veg[] == 7)

# @knitr func
f <- function(i, outDir, files, gbm, ...){

    func_trunc <- function(x, q, ind){
        ind <- intersect(ind, which(!is.na(x[])))
        x[ind][x[ind] < q[1]] <- q[1]
        x[ind][x[ind] > q[2]] <- q[2]
        x
    }
    
	r <- raster(files[i])
	if(gbm==3) r <- func_trunc(r, q.cavm, ind.cavm)
    if(gbm==5){
        r <- func_trunc(r, q.shrub, ind.shrub)
        r <- func_trunc(r, q.gram, ind.gram)
        r <- func_trunc(r, q.wet, ind.wet)
    }
	writeRaster(r, file.path(outDir, files[i]), datatype="FLT4S", overwrite=T)
	print(i)
}

# @knit run
mclapply(1:length(files), f, outDir=outDir, files=files, gbm=gbm, mc.cores=32)
