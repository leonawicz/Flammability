library(raster)
setwd("/big_scratch/mfleonawicz/Alf_Files_20121129")

comArgs <- commandArgs(trailingOnly=TRUE)
outDir <- comArgs[3]
domain <- comArgs[4]

if(substr(domain, 1, 6)=="Statew"){
  msk <- readAll(raster("alf2005.cavm.merged.030212.tif"))
} else if(substr(domain, 1, 6)=="Noatak"){
  msk <- readAll(raster("binary_ignition.tif"))
}

ignition <- msk
ignition[ignition > 0] <- as.numeric(comArgs[1])
ignition[is.na(ignition)] <- 0
writeRaster(ignition, filename=file.path(outDir, 'ignition.tif'), options='COMPRESS=LZW', overwrite=TRUE, datatype='FLT4S')

sensitivity <- msk
if(substr(domain, 1, 6)=="Noatak"){
  dem <- resample(raster("AKCanada_2km_DEM_mosaic.tif"), msk)
  sensitivity[dem > 500] <- 0
} else if(substr(domain, 1, 6)=="Statew"){
  dem <- resample(raster("AKCanada_2km_DEM_mosaic.tif"), msk)
  sensitivity[dem > 750] <- 0
}

sensitivity[sensitivity > 0] <- as.numeric(comArgs[2])
sensitivity[is.na(sensitivity)] <- 0
writeRaster(sensitivity, filename=file.path(outDir, 'sensitivity.tif'), options='COMPRESS=LZW', overwrite=TRUE, datatype='FLT4S')
