########################################################################################################
#### This R script generates climate-driven gradient-boosted flammability maps for use by ALFRESCO. ####
########################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   04/22/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("period")) stop("Argument 'period' not passed at command line.")
if(!exists("model")) stop("Argument 'model' not passed at command line.")
if(!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if(!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3"))) stop("Invalid data set specified.")
if(!exists("allcavm")) allcavm <- FALSE
if(!exists("samples")) samples <- FALSE
if(!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")
if(!is.logical(samples)) stop("Argument 'samples' must be logical.")

library(gbm); library(rgdal); library(raster); library(rasterVis); library(parallel); library(data.table)
rasterOptions(chunksize=10e10,maxmemory=10e11)
ncores <- 32

verDir <- if(samples) "samples_based" else "means_based"
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")
load(paste0("gbmFlammability/", model, "_", period, "_Jun-AugTP.RData"))

# Load gbm models
if(samples){
	load("gbm_seasonal_all_models_1Ksamples.RData")
	tree.numbers <- c(5000, 5000, 5000, 5000, 5000, 5000) # order: forest, alpine tundra, shrub, graminoid, wetland, cavm
} else {
	load("gbm_seasonal_all_models.RData")
	tree.numbers <- c(3355, 32, 2200, 152, 2478, 1554) # order: forest, alpine tundra, shrub, graminoid, wetland, cavm
}
gbm.names <- c("gbm.forest", "gbm.alp.tundra", "gbm.shrub", "gbm.gram", "gbm.wetland")

if(allcavm){
	out <- "3models_tif"
	plot.out <- "3models_png"
	gbm.gram <- gbm.shrub <- gbm.wetland <- gbm.all.cavm
	tree.numbers <- tree.numbers[c(1,2,6,6,6)] # order: forest, alpine tundra, shrub, graminoid, wetland
} else {
	out <- "5models_tif"
	plot.out <- "5models_png"
}
dir.create(outDir <- file.path("../data/gbmFlammability", verDir, period, model, out), recursive=T, showWarnings=F)
dir.create(plotDir <- file.path("../plots/gbmFlammability", verDir, period, model, plot.out), recursive=T, showWarnings=F)

# @knitr func_prep
f <- function(p, yrs=NULL, bins=1, samples=FALSE){
	d.names <- sort(rownames(summary(get(gbm.names[p])))) # force alphabetical
	tmp <- c()
	if(any(grep("Summer", d.names))){
		obj.names.list <- list(ls(pattern="^m\\..*.P$", envir=.GlobalEnv), ls(pattern="^m\\..*.T$", envir=.GlobalEnv)) # alphabetical
		for(i in 1:nrow(summary(get(gbm.names[p])))){
			obj.names <- obj.names.list[[i]]
			n <- length(obj.names)
			tmp2 <- 0
			for(j in 1:n) tmp2 <- tmp2 + get(obj.names[j])
			is.temp <- length(grep("T$", obj.names)) > 0
			if(is.temp) tmp2 <- tmp2/n
			tmp2 <- tmp2[get(ind.names[p]),]
			dims <- dim(tmp2)
			tmp2 <- as.numeric(tmp2)
			if(samples) tmp2 <- (tmp2 - mean(tmp2, na.rm=TRUE))/sd(tmp2, na.rm=TRUE)
			tmp <- cbind(tmp, tmp2)
			rm(tmp2); gc()
		}
	} else {
		for(i in 1:nrow(summary(get(gbm.names[p])))){
			tmp2 <- get(paste0("m.",d.names[i]))[get(ind.names[p]),]
			dims <- dim(tmp2)
			tmp2 <- as.numeric(tmp2)
			if(samples) tmp2 <- (tmp2 - mean(tmp2, na.rm=TRUE))/sd(tmp2, na.rm=TRUE)
			tmp <- cbind(tmp, tmp2)
		}
	}
	colnames(tmp) <- d.names
	rownames(tmp) <- NULL
	tmp <- data.frame(tmp)
	if(length(yrs)==dims[2]) ids <- yrs else ids <- 1:dims[2]
	tmp$ID <- rep(ids, each=dims[1])
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
getGBMpreds <- function(a,b, nam1, nam2){
	x0 <- get(nam1[b])
	x1 <- get(nam2[a])
	y <- predict.gbm(x0, x1, n.trees=tree.numbers[b])
	data.frame(Predicted=y, ID=x1$ID)
}

# @knitr func_write
partifs <- function(i, r, flam, outDir){
	r <- setValues(r, flam[,i])
	names(r) <- paste0("gbm.flamm_",yrs[1]+i-1)
	writeRaster(r, paste0(outDir, "/gbm.flamm_",yrs[1]+i-1,".tif"), datatype="FLT4S", overwrite=T)
	print(i)
}

# @knitr run
preds <- list()
for(zzz in 1:5){
	model.index <- f(zzz, bins=ncores, samples=samples) # Prep data
	tmp.preds <- mclapply(1:ncores, getGBMpreds, b=model.index, nam1=gbm.names, nam2=tmp.names, mc.cores=ncores) # GBM predictions
	tmp.preds <- rbindlist(tmp.preds)
	preds[[model.index]] <- matrix(tmp.preds$Predicted, ncol=length(yrs))
	print(zzz)
}

# Organize results
flam <- matrix(NA, nrow=length(veg), ncol=length(yrs))
for(i in 1:5) flam[which(get(ind.names[i])),] <- preds[[i]]
flam.range <- range(flam, na.rm=T)
flam <- (flam - flam.range[1])/(flam.range[2] - flam.range[1])

# Write geotiffs
mclapply(1:length(yrs), partifs, r=r.veg, flam=flam, outDir=outDir, mc.cores=32)

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
