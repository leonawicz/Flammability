


##
##
## gbm_flam_prep.R

The `gbm_flam_prep.R` script prepares **R** workspace files for CRU and each GCM.
These workspaces are loaded as inputs in the subsequent `gbm_flam_maps.R` script.

## R code

### Setup


```r
comargs <- (commandArgs(TRUE))
if (!length(comargs)) q("no") else for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("period")) stop("Argument 'period' not passed at command line.")
if (!exists("model")) stop("Argument 'model' not passed at command line.")
if (!(period %in% c("historical", "rcp45", "rcp60", "rcp85"))) stop("Invalid period specified.")
if (!(model %in% c("CRU31", "CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", 
    "MRI-CGCM3"))) stop("Invalid data set specified.")

library(rgdal)
library(raster)
rasterOptions(chunksize = 1e+10, maxmemory = 1e+11)

setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")
dir.create(outDir <- "gbmFlammability", showWarnings = F)

# Load gbm models
load("gbm_seasonal_all_models.RData")
tree.numbers <- c(3355, 32, 2200, 152, 2478)  # order: forest, alpine tundra, shrub, graminoid, wetland
```

### Ecoregions and vegetation


```r
# If exluding ecoregions
rm.eco <- T
ecoreg <- raster(as.matrix(read.table("../data/ecoreg_mark_mask_zero.txt", skip = 6, 
    header = F)))
drop.ind <- Which(ecoreg == 4, cells = T)
if (rm.eco) eco.ind <- values(Which(ecoreg != 0 & ecoreg != 4)) else eco.ind <- 1

# Assembling the flammability based on the veg map
r.veg <- raster("../data/alf2005.cavm.merged.030212.tif")
veg <- getValues(r.veg)
if (any(is.na(veg))) veg[is.na(veg)] <- 0
veg.tmp <- as.numeric(veg)
rm(veg)
veg.val <- as.numeric(veg.tmp != 0) * eco.ind * veg.tmp
f.ind <- veg.val == 2 | veg.val == 3 | veg.val == 4
a.ind <- veg.val == 1
s.ind <- veg.val == 5
g.ind <- veg.val == 6
w.ind <- veg.val == 7
land.gray <- as.numeric(veg.tmp != 0) * (1 - eco.ind)

gbm.names <- c("gbm.forest", "gbm.alp.tundra", "gbm.shrub", "gbm.gram", "gbm.wetland")
ind.names <- c("f.ind", "a.ind", "s.ind", "g.ind", "w.ind")

# @knit obs_fire If adding fire scars to graphical maps
write.emp.tifs <- FALSE
if (write.emp.tifs) {
    fah <- shapefile("/Data/Base_Data/GIS/GIS_Data/Vector/Fire/FireAreaHistory_11182013.shp")
    emp.fire.years <- sort(as.numeric(unique(fah@data$FireYear)))  # 1940:2013 max range
    emp.fire.years.all <- seq(emp.fire.years[1], tail(emp.fire.years, 1))
    fireScarsFun <- function(x, y, year, years.avail) {
        if (year %in% years.avail) {
            x <- x[x$FireYear == year, ]
            x <- rasterize(x, y, field = 1)
        } else {
            x <- x[x$FireYear == years.avail[1], ]
            x <- rasterize(x, y, field = 1)
            x[!is.na(x)] <- NA
        }
        x
    }
    
    fireScarsFunVec <- Vectorize(fireScarsFun, "year")
    emp.fire.brick <- fireScarsFunVec(x = fah, y = r.veg, year = emp.fire.years.all, 
        years.avail = emp.fire.years)
    emp.fire.sum <- do.call("sum", c(emp.fire.brick, na.rm = T))
    emp.fire.brick <- brick(emp.fire.brick)
    names(emp.fire.brick) <- emp.fire.years.all
    names(emp.fire.sum) <- paste(emp.fire.years.all[1], tail(emp.fire.years.all, 
        1), sep = "_")
    writeRaster(emp.fire.brick, "../data/firescarbrick_annual_observed_statewide_1940_2013.tif", 
        datatype = "FLT4S", overwrite = T)
    writeRaster(emp.fire.sum, "../data/firescarlayer_total_observed_statewide_1940_2013.tif", 
        datatype = "FLT4S", overwrite = T)
} else {
    emp.fire.brick <- brick("../data/firescarbrick_annual_observed_statewide_1940_2013.tif")
    emp.fire.sum <- raster("../data/firescarlayer_total_observed_statewide_1940_2013.tif")
}
```

### Prepare and save


```r
# Prepare and save workspace
if (period == "historical") {
    tpDir <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", period, 
        "cru_TS31")
} else {
    tpDir <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", period, 
        model)
}
mo.ind <- 6:8
varid <- c("pr", "tas")
pat.mo <- paste0(".*._", c(paste0(0, 1:9), 10:12), "_.*.tif$")
files.precip <- files.temp <- list()
for (i in mo.ind) {
    files.precip[[i]] <- list.files(file.path(tpDir, varid[1]), full = T, pattern = pat.mo[i])
    files.temp[[i]] <- list.files(file.path(tpDir, varid[2]), full = T, pattern = pat.mo[i])
    print(i)
}

for (i in mo.ind) {
    assign(paste0("m.", month.abb[i], "P"), as.matrix(stack(files.precip[[i]])))
    assign(paste0("m.", month.abb[i], "T"), as.matrix(stack(files.temp[[i]])))
    print(i)
}

save.image(paste0(outDir, "/", model, "_", period, "_Jun-AugTP.RData"))
```
