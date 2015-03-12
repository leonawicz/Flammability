library(parallel)
library(raster)

msk <- raster("/workspace/Shared/Users/mfleonawicz/tmp/meanTPbyVegClass/alf2005.cavm.merged.030212.tif")
e <- extent(msk)
mainDir <- "/big_scratch/mfleonawicz/CMIP5_Climate_1km"
varid <- rep(c("pr","tas"), 15)
rcp <- rep(rep(paste0("rcp",c(45,60,85)), each=2), 5)
model <- rep(list.files(file.path(mainDir,"rcp60")), each=6)

subDir <- file.path(mainDir, rcp, model, varid)
outDir <- file.path("/big_scratch/mfleonawicz/CMIP5_Climate_1km_AKstatewide", rcp, model, varid)
for(i in 1:length(outDir)) dir.create(outDir[i], recursive=T, showWarnings=F)

f <- function(i, subDir, outDir, msk){
	require(raster)
	files <- list.files(subDir[i], full=T, pattern=".tif$")
	for(j in 1:length(files)){
		r <- raster(files[j])
		r <- crop(r, msk)
		r <- mask(r, msk)
		extent(r) <- e
		writeRaster(r, file.path(outDir[i], basename(files[j])), datatype="FLT4S", overwrite=T)
		print(length(files) - j)
	}
	return()
}

#for(k in 1:length(subDir)) f(k, subDir, outDir, msk)
mclapply(1:length(subDir), f, subDir=subDir, outDir=outDir, msk=msk, mc.cores=length(subDir))
