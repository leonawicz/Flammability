#######################################################################################################
#### This R script generates climate-driven gradient-boosted flamability maps for use by ALFRESCO. ####
#######################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/10/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("allcavm")) allcavm <- FALSE
if(!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")

library(gbm); library(rgdal); library(raster); library(fields); library(parallel)
rasterOptions(chunksize=10e12,maxmemory=10e13)
ncores <- 32

if(period=="historical"){
	if(!exists(yrs)) yrs <- 1901:2009
	tpDir <- "/Data/Base_Data/ALFRESCO_formatted/AK_1km(from800m)/cru_TS31/historical"
} else {
	if(!exists(yrs)) yrs <- 2010:2099
	tpDir <- file.path("/big_scratch/mfleonawicz/CMIP5_Climate_1km_AKstatewide", period, model)
}

load(paste0("/workspace/Shared/Users/mfleonawicz/tmp/gbmFlammability/090814/", model, "_", period, "_Jun-AugTP.RData")
load("/workspace/Shared/Users/mfleonawicz/tmp/gbmFlammability/090814/GBMs/gbm_seasonal_all_models.RData")

if(allcavm){
	outDir <- "3models_tifs"
	gbm.gram <- gbm.shrub <- gbm.wetland <- gbm.all.cavm
	tree.numbers <- c(3355, 32, 1554, 1554, 1554) # order: forest, alpine tundra, shrub, graminoid, wetland
} else outDir <- "5models_tifs"

f <- function(p, bins=1){
	d.names <- rownames(summary(get(gbm.names[p])))
	tmp <- c()
	if(any(grep("Summer", d.names))){
		obj.names.list <- list(ls(pattern="^m.*.T$", envir=.GlobalEnv), ls(pattern="^m.*.P$", envir=.GlobalEnv))
		for(i in 1:nrow(summary(get(gbm.names[p])))){
			obj.names <- obj.names.list[[i]]
			n <- length(obj.names)
			tmp2 <- 0
			for(j in 1:n) tmp2 <- tmp2 + get(obj.names[j])
			if(length(grep("T$", obj.names.list[[i]]))) tmp2 <- tmp2/n
			tmp <- cbind(tmp, as.numeric(tmp2[get(ind.names[p]),]))
			rm(tmp2); gc()
		}
	} else {
		for(i in 1:nrow(summary(get(gbm.names[p])))) tmp <- cbind(tmp, as.numeric(get(paste0("m.",d.names[i]))[get(ind.names[p]),]))
	}
	colnames(tmp) <- d.names
	rownames(tmp) <- NULL
	tmp <- data.frame(tmp)
	assign("tmp.names", paste0("tmp_", c(paste0(0,c(1:9)), 10:bins)[1:bins]), envir=.GlobalEnv)
	brks <- round(seq(1, nrow(tmp), length.out=bins+1))
	for(i in 1:bins){
		if(i==bins) rows <- brks[i]:(brks[i+1]) else rows <- brks[i]:(brks[i+1]-1)
		assign(tmp.names[i], tmp[rows,], envir=.GlobalEnv)
	}
	rm(tmp); gc()
	#x <- matrix(predict.gbm(get(gbm.names[p]),tmp,n.trees=tree.numbers[p]/100),ncol=109)
	return(p)
}

#preds <- mclapply(1:5,f,mc.cores=5)

getGBMpreds <- function(a,b) predict.gbm(get(gbm.names[b]), get(tmp.names[a]), n.trees=tree.numbers[b])

preds <- list()
for(zzz in 1:5){
	model.index <- f(zzz, bins=ncores)
	tmp.preds <- mclapply(1:ncores, getGBMpreds, b=model.index, mc.cores=ncores)
	preds[[model.index]] <- matrix(unlist(tmp.preds), ncol=length(yrs))
	print(zzz)
}

flam <- matrix(NA,nrow=length(veg.tmp),ncol=length(yrs))
for(i in 1:5) flam[which(get(ind.names[i])),] <- preds[[i]]
flam.range <- range(flam,na.rm=T)
flam <- (flam-flam.range[1])/(flam.range[2]-flam.range[1])

partifs <- function(i){
	r <- r.veg
	r <- setValues(r,flam[,i])
	names(r) <- paste0("gbm.flamm_",yrs[1]+i-1)
	writeRaster(r,paste0("/workspace/Shared/Users/mfleonawicz/tmp/gbmFlammability/090814/", outDir, "/gbm.flamm_",yrs[1]+i-1,".tif"), datatype="FLT4S", overwrite=T)
	print(i)
}

mclapply(1:length(yrs), partifs, mc.cores=32)
#############################
# Make PNGs

library(raster); # library(fields)
library(rasterVis)

at.vals <- c(0, 0.25, 0.5, 0.75, 1)
colkey <- list(at=at.vals, labels=list(labels=c("Low", "Medium", "High", "Severe"), at=at.vals + 0.125))

revRasterTheme <- function (pch = 19, cex = 0.7, region=brewer.pal(9, "YlOrRd")[-1],
    ...)
{
    theme <- custom.theme.2(pch = pch, cex = cex, region = region,
        ...)
    theme$strip.background$col = "transparent"
    theme$strip.shingle$col = "transparent"
    theme$strip.border$col = "transparent"
    theme$add.line$lwd = 0.4
    theme
}

library(parallel)
parplot <- function(i){
	r <- raster(paste0("/workspace/Shared/Users/mfleonawicz/tmp/gbmFlammability/090814/", outDir, "/gbm.flamm_",yrs[1]+i-1,".tif"))
	dir.create(pngDir <- paste0("/workspace/Shared/Users/mfleonawicz/tmp/gbmFlammability/090814/", outDir, "/PNGs"), showWarnings=F)
	png(paste0(pngDir, "/gbm.flamm_", yrs[1]+i-1,".png"), height=1600, width=1600, res=200)
	p <- levelplot(r, maxpixels=ncell(r), main=paste(yrs[1]+i-1,"flammability"), par.settings=revRasterTheme, contour=T, margin=F, at=at.vals, colorkey=colkey) #col=rev(heat.colors(30)))
	print(p)
	dev.off()
	print(i)
}

mclapply(1:length(yrs), parplot, mc.cores=32)

