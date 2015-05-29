


##
##
## FlammabilityMapMultipliers.R

The `FlammabilityMapMultipliers.R` script multiplies each flammability map in a series by a scalar coeeficient to stretch or compress inter-annual variability in flammability.
For CRU 3.1 historical flammability maps, observation-based coefficients are applied for the years 1950 - 2011.
A small set of discrete coefficient values are used.
Historical CRU-based and projected GCM-based flammability maps outside the 1950 - 2011 period are multiplied by randomized coefficients from the same set.

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
if (!exists("samples")) samples <- TRUE
if (!exists("mapset")) stop("Argument 'mapset' not passed at command line.")
if (!exists("lightning")) lightning <- TRUE
if (!exists("cp2scratch")) cp2scratch <- TRUE
if (!exists("cp_originals")) cp_originals <- TRUE

verDir <- if (samples) "samples_based" else "means_based"
setwd(file.path("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability", 
    verDir, period, model, mapset))
dir.create(outDir <- paste0("../", mapset, "_scaled"), showWarnings = FALSE)
if (cp2scratch) {
    dir.create(outDir2a <- file.path("/big_scratch/mfleonawicz/Alf_Files_20121129/gbmFlamMaps", 
        period, model, mapset), recursive = TRUE, showWarnings = FALSE)
    dir.create(outDir2b <- paste0(outDir2a, "_scaled"), showWarnings = FALSE)
} else outDir2b <- NULL
if (!cp_originals) outDir2a <- NULL

library(raster)
library(parallel)
load("../../../../../../workspaces/gbmFlammability/ALF_ignit_premult.RData")  # scalars data frame for observed years

files <- list.files(pattern = "\\.tif$")
yrs <- as.numeric(gsub("gbm.flamm_", "", gsub("\\.tif", "", files)))
files <- files[order(yrs)]
yrs <- yrs[order(yrs)]

# Sample random coeffcients for unobserved years
set.seed(47)
if (lightning) {
    classes <- sapply(ignit.scalar$ignit.lightning, function(x) switch(as.character(x), 
        `0.05` = 1, `0.5` = 2, `0.95` = 3))
    load("/workspace/UA/mfleonawicz/leonawicz/projects/Lightning/data/summerLightningMaps_2003_2012/summerLightningMaps.RData")
    light.yrs <- sapply(classes, function(x, d) sample(d$Year[d$Class == x], 
        1), d = d.coef)
    ind <- which(ignit.scalar$V1 %in% d.coef$Year)
    light.yrs[ind] <- ignit.scalar$V1[ind]
    a <- sample(d.coef$Year, length(yrs), replace = T)
    if (all(1950:2011 %in% yrs)) 
        a[yrs >= 1950 & yrs <= 2011] <- light.yrs
} else {
    a <- sample(c(0.05, 0.5, 0.95), length(yrs), prob = c(21/62, 20/62, 21/62), 
        replace = T)
    if (all(1950:2011 %in% yrs)) 
        a[yrs >= 1950 & yrs <= 2011] <- ignit.scalar[, 2]
}
```

### Run


```r
f <- function(i, a, b = NULL, type = "coef", outDir, files, flam.min = NULL, 
    f_of_xy = NULL, cp.origin = NULL, cp.new = NULL) {
    r <- raster(files[i])
    if (!is.null(cp.origin)) 
        writeRaster(r, file.path(cp.origin, files[i]), datatype = "FLT4S", overwrite = T)
    if (!is.null(flam.min)) 
        r[r < flam.min] <- flam.min
    if (type == "coef") {
        r <- a[i] * r
    } else if (type == "year") {
        if (is.null(b)) 
            stop("b cannot be NULL if type='year'. Change to type='coef' or provide a raster brick b.")
        b.yrs <- as.numeric(substr(names(b), 2, 5))
        ind <- which(b.yrs == a[i])
        if (is.null(f_of_xy)) {
            r2 <- subset(b, ind)
            r2[r2 < 0.5] <- 0.5
            r <- r2 * r
        } else f_of_xy(x = subset(b, ind), y = r)
    }
    r <- round(r, 8)
    writeRaster(r, file.path(outDir, files[i]), datatype = "FLT4S", overwrite = T)
    if (!is.null(cp.new)) 
        writeRaster(r, file.path(cp.new, files[i]), datatype = "FLT4S", overwrite = T)
    print(i)
}

# @knit run
mclapply(1:length(files), f, a = a, b = kde.maps, type = "year", outDir = outDir, 
    files = files, flam.min = 0.25, cp.origin = outDir2a, cp.new = outDir2b, 
    mc.cores = 32)
```
