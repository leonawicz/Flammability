


##
##
## tpByVeg_CMIP5.R

The `tpByVeg_CMIP5.R` script calculates mean temperature and precipitation over all grid cells in a CGM map layer conditional on vegetation class using a given vegetation classification map.
Unlike the `tpByVeg_CRU32.R` script, this script does not currently provide a sampling option since the CMIP5 models are not used for fitting GBM models.

## R code

### Setup


```r
comargs <- (commandArgs(TRUE))
if (length(comargs)) for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("allcavm")) allcavm <- FALSE
if (!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")

library(raster)
library(parallel)

setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")
dataDir <- "/big_scratch/mfleonawicz/Climate_1km_AKstatewide"

r.veg <- raster("/workspace/Shared/Users/mfleonawicz/tmp/meanTPbyVegClass/alf2005.cavm.merged.030212.tif")
veg.vec <- getValues(r.veg)
sort(unique(veg.vec))
rm.eco <- T
ecoreg <- raster(as.matrix(read.table("../data/ecoreg_mark_mask_zero.txt", skip = 6, 
    header = F)))
drop.ind <- Which(ecoreg == 4, cells = T)
if (rm.eco) eco.ind <- values(Which(ecoreg != 0 & ecoreg != 4)) else eco.ind <- 1
veg.vec <- as.numeric(veg.vec != 0) * eco.ind * veg.vec

veg.vec[veg.vec == 3 | veg.vec == 4] <- 2  # for forest
if (allcavm) veg.vec[veg.vec == 6 | veg.vec == 7] <- 5  # 'cavm' all three shrub, graminoid, wetland combined

veg.vals <- if (allcavm) 5 else c(1, 2, 5, 6, 7)
veg.names <- if (allcavm) "cavm" else c("tundra", "forest", "shrub", "graminoid", 
    "wetland")
scenario <- c("rcp45", "rcp60", "rcp85")
modnames <- rep(list.files(file.path(dataDir, scenario[1])), length(scenario))
scenario <- rep(scenario, each = length(modnames)/length(scenario))
path <- list(file.path(dataDir, scenario, modnames, "pr"), file.path(dataDir, 
    scenario, modnames, "tas"))
dir.create(wsDir <- "tpByVeg", showWarnings = F)

### pick the years and months for the analysis
yrs <- 2010:2099
```

### Function


```r
f <- function(k, path, veg.vec, veg.vals, veg.names) {
    paths <- list.files(path, full = T)
    ind <- which(as.numeric(substr(paths, nchar(paths) - 7, nchar(paths) - 4)) == 
        k)
    paths <- paths[ind]
    precip.paths <- paths[1:12]
    temp.paths <- paths[13:24]
    precip.tmp <- getValues(stack(precip.paths))
    temp.tmp <- getValues(stack(temp.paths))
    means.P.mos.veg <- means.T.mos.veg <- c()
    for (j in 1:length(veg.vals)) {
        means.P.mos.veg <- cbind(means.P.mos.veg, round(colMeans(precip.tmp[veg.vec == 
            veg.vals[j], ], na.rm = T)))
        means.T.mos.veg <- cbind(means.T.mos.veg, round(colMeans(temp.tmp[veg.vec == 
            veg.vals[j], ], na.rm = T), 1))
    }
    rownames(means.P.mos.veg) <- rownames(means.T.mos.veg) <- month.abb
    colnames(means.P.mos.veg) <- colnames(means.T.mos.veg) <- veg.names
    print(k)
    return(list(Pmeans = means.P.mos.veg, Tmeans = means.T.mos.veg, k = k - 
        yrs[1] + 1))
}
```

### Run and save outputs


```r
for (z in 1:length(path[[1]])) {
    f.out <- mclapply(yrs, f, path = c(path[[1]][z], path[[2]][z]), veg.vec = veg.vec, 
        veg.vals = veg.vals, veg.names = veg.names, mc.cores = min(length(yrs), 
            32))
    names(f.out) <- yrs
    for (i in 1:length(veg.vals)) {
        assign(paste("table.p", veg.names[i], scenario[z], modnames[z], sep = "."), 
            c())
        assign(paste("table.t", veg.names[i], scenario[z], modnames[z], sep = "."), 
            c())
    }
    for (j in 1:length(f.out)) {
        for (i in 1:length(veg.vals)) {
            assign(paste("table.p", veg.names[i], scenario[z], modnames[z], 
                sep = "."), rbind(get(paste("table.p", veg.names[i], scenario[z], 
                modnames[z], sep = ".")), c(Year = as.numeric(names(f.out)[j]), 
                f.out[[j]]$Pmeans[, i])))
            assign(paste("table.t", veg.names[i], scenario[z], modnames[z], 
                sep = "."), rbind(get(paste("table.t", veg.names[i], scenario[z], 
                modnames[z], sep = ".")), c(Year = as.numeric(names(f.out)[j]), 
                f.out[[j]]$Tmeans[, i])))
        }
    }
}

if (length(ls(pattern = ".*.cavm.*."))) {
    save(list = ls(pattern = "^table\\."), file = file.path(wsDir, "tpByVeg_means_CMIP5_cavm.RData"))
} else {
    save(list = ls(pattern = "^table\\."), file = file.path(wsDir, "tpByVeg_means_CMIP5_individual.RData"))
}
```
