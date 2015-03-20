########################################################################################################
#### This R script generates climate-driven gradient-boosted flammability maps for use by ALFRESCO. ####
########################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/19/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("allcavm")) allcavm <- FALSE
if(!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")

library(gbm); library(rgdal); library(raster); library(rasterVis); library(parallel)
rasterOptions(chunksize=10e10,maxmemory=10e11)
ncores <- 32

setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")

if(period=="historical"){
	yrs <- 1901:2009
	tpDir <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", period, "cru_TS31")
} else {
	yrs <- 2006:2100
	tpDir <- file.path("/big_scratch/mfleonawicz/CMIP5_Climate_1km_AKstatewide", period, model)
}

load(paste0("gbmFlammability/", model, "_", period, "_Jun-AugTP.RData"))
load("gbm_seasonal_all_models.RData")

if(allcavm){
	out <- "3models_tif"
	plot.out <- "3models_png"
	gbm.gram <- gbm.shrub <- gbm.wetland <- gbm.all.cavm
	tree.numbers <- c(3355, 32, 1554, 1554, 1554) # order: forest, alpine tundra, shrub, graminoid, wetland
} else {
	out <- "5models_tif"
	plot.out <- "5models_png"
}
dir.create(outDir <- file.path("../data/gbmFlammability", period, model, out), recursive=T, showWarnings=F)
dir.create(plotDir <- file.path("../plots/gbmFlammability", period, model, plot.out), recursive=T, showWarnings=F)

# @knitr func_prep
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
	assign("tmp.names", paste0("tmp_", c(paste0(0,c(1:9)), 10:bins)[1:bins]), envir=.GlobalEnv) # global assignment side effects
	brks <- round(seq(1, nrow(tmp), length.out=bins+1))
	for(i in 1:bins){
		if(i==bins) rows <- brks[i]:(brks[i+1]) else rows <- brks[i]:(brks[i+1]-1)
		assign(tmp.names[i], tmp[rows,], envir=.GlobalEnv) # global assignment side effects
	}
	rm(tmp); gc()
	return(p)
}

# @knitr func_predict
getGBMpreds <- function(a,b, nam1, nam2) predict.gbm(get(nam1[b]), get(nam2[a]), n.trees=tree.numbers[b])

# @knitr func_write
partifs <- function(i, outDir){
	r <- r.veg
	r <- setValues(r,flam[,i])
	names(r) <- paste0("gbm.flamm_",yrs[1]+i-1)
	writeRaster(r, paste0(outDir, "/gbm.flamm_",yrs[1]+i-1,".tif"), datatype="FLT4S", overwrite=T)
	print(i)
}

# @knitr run
preds <- list()
for(zzz in 1:5){
	model.index <- f(zzz, bins=ncores) # Prep data
	tmp.preds <- mclapply(1:ncores, getGBMpreds, b=model.index, nam1=gbm.names, nam2=tmp.names, mc.cores=ncores) # GBM predictions
	preds[[model.index]] <- matrix(unlist(tmp.preds), ncol=length(yrs))
	print(zzz)
}

# Organize results
flam <- matrix(NA,nrow=length(veg.tmp),ncol=length(yrs))
for(i in 1:5) flam[which(get(ind.names[i])),] <- preds[[i]]
flam.range <- range(flam,na.rm=T)
flam <- (flam-flam.range[1])/(flam.range[2]-flam.range[1])

# Write geotiffs
mclapply(1:length(yrs), partifs, outDir=outDir, mc.cores=32)

# @knitr plot
# Setup
at.vals <- c(0, 0.25, 0.5, 0.75, 1)
colkey <- list(at=at.vals, labels=list(labels=c("Low", "Medium", "High", "Severe"), at=at.vals + 0.125))

# Theme settings
revRasterTheme <- function (pch = 19, cex = 0.7, region=brewer.pal(9, "YlOrRd")[-1], ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

# parallelize levelplot
parplot <- function(i, outDir, dataDir){
	r <- raster(paste0(dataDir, "/gbm.flamm_",yrs[1]+i-1,".tif"))
	png(paste0(outDir, "/gbm.flamm_", yrs[1]+i-1,".png"), height=1600, width=1600, res=200)
	p <- levelplot(r, maxpixels=ncell(r), main=paste(yrs[1]+i-1,"flammability"), par.settings=revRasterTheme, contour=T, margin=F, at=at.vals, colorkey=colkey) #col=rev(heat.colors(30)))
	print(p)
	dev.off()
	print(i)
}

# Write PNGs
mclapply(1:length(yrs), parplot, outDir=plotDir, dataDir=outDir, mc.cores=32)
