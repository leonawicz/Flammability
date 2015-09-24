# @knitr script
# Alfresco 2005 vegetation input map
library(rasterVis)
setwd("C:/github/Flammability/data")
outDir <- "../plots/alfInputs"
shp <- shapefile("shapefiles/noa_basin2/Noa_basin2")
r.veg <- raster("alf2005.cavm.merged.030212.tif")
r.veg[r.veg==0] <- NA
r.veg <- mask(crop(r.veg, shp), shp)

pts <- read.csv("C:/github/shiny-apps/run_alfresco/pts/Noatak_lake_locations.csv")
pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)

wgs2ak <- function(xy){
	require(rgdal)
	if(class(xy)=="matrix") xy <- data.frame(xy)
	names(xy) <- c("x","y")
	coordinates(xy) <- names(xy)
	proj4string(xy)<- CRS("+proj=longlat +datum=WGS84")
	xy <- coordinates(spTransform(xy,
		CRS=CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")))
}

pts <- wgs2ak(pts)

cbpal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
at.vals <- seq(0.5,7.5,by=1)
colkey <- list(at=at.vals, labels=list(labels=c("Alpine", "B. Spruce", "W. Spruce", "Deciduous", "Shrub", "Graminoid", "Wetland"), at=at.vals + 0.5))

revRasterTheme <- function (pch = 19, cex = 0.7, region=cbpal, ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

r.pts <- r.veg
r.pts[] <- NA
r.pts[cellFromXY(r.pts, pts)] <- 1

png(file.path(outDir, "noatak_2005vegInputMap.png"), height=1600, width=3200, res=200)
p <- levelplot(r.veg, main="ALFRESCO vegetation input map", par.settings=revRasterTheme, margin=F, at=at.vals, colorkey=colkey) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()


# Alfresco 1950-2013 CRU 3.2-based flammability map summaries
library(rasterVis)
rasterOptions(chunksize=10e10,maxmemory=10e11)
setwd("X:/leonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU32")
dir.create(outDir <- "X:/leonawicz/projects/Flammability/plots/alfInputs", recur=T, showWarnings=F)
shp <- shapefile("X:/leonawicz/projects/Flammability/data/shapefiles/noa_basin2/Noa_basin2")
#r.veg <- raster("/workspace/UA/mfleonawicz/projects/Flammability/data/alf2005.cavm.merged.030212.tif")
#r.veg[r.veg==0] <- NA
#r.veg <- mask(crop(r.veg, shp), shp)
files <- list.files("3m100n_cavmDistTrunc_loop_L", full=T)
yrs <- as.numeric(sub(".tif", "", gsub("gbm.flamm_", "", basename(files))))
files3 <- files[yrs>=1950 & yrs<=2013]
files <- list.files("5m100n_cavmDistTrunc_loop_L", full=T)
files5 <- files[yrs>=1950 & yrs<=2013]
r3 <- calc(mask(crop(stack(files3), shp), shp), function(x) c(mean(x), sd(x), min(x), max(x)))
r5 <- calc(mask(crop(stack(files5), shp), shp), function(x) c(mean(x), sd(x), min(x), max(x)))
names(r3) <- paste("GBM3", c("Mean", "SD", "Min", "Max"), sep="_")
names(r5) <- paste("GBM5", c("Mean", "SD", "Min", "Max"), sep="_")

pts <- read.csv("/workspace/UA/mfleonawicz/projects/Flammability/data/pts/Noatak_lake_locations.csv")
pts <- pts[order(pts$ID),]
locs <- as.character(pts$ID)
pts <- cbind(pts$Lon,pts$Lat)

wgs2ak <- function(xy){
	require(rgdal)
	if(class(xy)=="matrix") xy <- data.frame(xy)
	names(xy) <- c("x","y")
	coordinates(xy) <- names(xy)
	proj4string(xy)<- CRS("+proj=longlat +datum=WGS84")
	xy <- coordinates(spTransform(xy,
		CRS=CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")))
}

pts <- wgs2ak(pts)

revRasterTheme <- function (pch = 19, cex = 0.7, region=brewer.pal(9, "YlOrRd")[-1], ...){
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

r.pts <- r3
r.pts[] <- NA
r.pts[cellFromXY(r.pts, pts)] <- 1

png(file.path(outDir, "noatak_flammability_mean_1959-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,1),subset(r5,1)), main="ALFRESCO 1950-2013 annual flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_sd_1959-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,2),subset(r5,2)), main="ALFRESCO 1950-2013 annual flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_min_959-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,3),subset(r5,3)), main="ALFRESCO 1950-2013 annual flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()

png(file.path(outDir, "noatak_flammability_max_1959-2013.png"), height=3200, width=3200, res=300)
p <- levelplot(stack(subset(r3,4),subset(r5,4)), main="ALFRESCO 1950-2013 annual flammability input summary", par.settings=revRasterTheme, margin=F) +
    layer(sp.points(rasterToPoints(r.pts, sp=T), col='black'))
print(p)
dev.off()
