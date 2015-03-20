###############################################################
#### This R script resamples 2-km AK-CAN geotiffs to 1-km. ####
###############################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/19/2015        ####

# @knitr setup
library(parallel)
library(raster)
msk <- raster("/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/5ModelAvg/sresa2/tas/tas_mean_C_alf_ar4_5modelAvg_sresa2_01_2001.tif")

mainDir <- "/Data/Base_Data/Climate/AK_CAN_2km/projected/AR5_CMIP5_models"
varid <- rep(c("pr","tas"), 15)
rcp <- rep(rep(paste0("rcp",c(45,60,85)), each=2), 5)
model <- rep(list.files(file.path(mainDir,"rcp60"))[-1], each=6)

subDir <- file.path(mainDir, rcp, model, varid)
outDir <- file.path("/big_scratch/mfleonawicz/Climate_1km", rcp, model, varid)
for(i in 1:length(outDir)) dir.create(outDir[i], recursive=T, showWarnings=T)

f <- function(i, subDir, outDir, msk){
	require(raster)
	files <- list.files(subDir[i], full=T, pattern=".tif$")
	for(j in 1:length(files)){
		r <- raster(files[j])
		r <- resample(r, msk, method="ngb") # Nearest neighbor for efficiency, scale change is small and use case doesn't require interpolation
		r <- mask(r, msk)
		writeRaster(r, file.path(outDir[i], basename(files[j])), datatype="FLT4S", overwrite=T)
		print(length(files) - j)
	}
	return()
}

mclapply(1:length(subDir), f, subDir=subDir, outDir=outDir, msk=msk, mc.cores=length(subDir))
