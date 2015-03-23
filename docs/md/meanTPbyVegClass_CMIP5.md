


##
##
## meanTPbyVegClass_CMIP5.R

The `meanTPbyVegClass_CMIP5.R` script calculates mean temperature and precipitation over all grid cells in a CGM map layer conditional of vegetation class using a given vegetation classification map.
These vegetation-specific climate means are used as variables in gradient boosting machine (GBM), or generalized boosted regression modeling.
The resulting GBM models are used to develop climate-driven, vegetation-mediated flammability maps with a nonlinear fire response to climate for use in ALFRESCO in place of basic temperature and precipitation maps.

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
dir.create(wsDir <- "meanTPbyVegClass", showWarnings = F)

### pick the years and months for the analysis
yr.start <- 2006
yr.end <- 2100
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
    row.names(means.P.mos.veg) <- row.names(means.T.mos.veg) <- month.abb
    colnames(means.P.mos.veg) <- colnames(means.T.mos.veg) <- veg.names
    print(k)
    return(list(Pmeans = means.P.mos.veg, Tmeans = means.T.mos.veg, k = k - 
        yr.start + 1))
}
```

### Run and save outputs


```r
for (z in 1:length(path[[1]])) {
    f.out <- mclapply(yr.start:yr.end, f, path = c(path[[1]][z], path[[2]][z]), 
        veg.vec = veg.vec, veg.vals = veg.vals, veg.names = veg.names, mc.cores = min(length(yr.start:yr.end), 
            32))
    names(f.out) <- yr.start:yr.end
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
    dir.create(outDir <- file.path("../data/meanTPbyVegClass", scenario[z], 
        modnames[z]), showWarnings = F, recursive = T)
    for (k in 1:length(veg.names)) {
        write.csv(get(paste("table.p", veg.names[k], scenario[z], modnames[z], 
            sep = ".")), paste(outDir, "/pr_", scenario[z], "_", modnames[z], 
            "_", veg.names[k], ".csv", sep = ""), row.names = F)
        write.csv(get(paste("table.t", veg.names[k], scenario[z], modnames[z], 
            sep = ".")), paste(outDir, "/tas_", scenario[z], "_", modnames[z], 
            "_", veg.names[k], ".csv", sep = ""), row.names = F)
        
    }
}

rm(comargs, i, j, k, f.out, yr.start, yr.end, veg.vals, veg.vec, path, f, modnames, 
    scenario, r.veg, veg.names, outDir, dataDir, allcavm)
if (length(ls(pattern = ".*.cavm.*."))) save.image(file.path(wsDir, "meanTPbyVegClass_CMIP5_cavm.RData")) else save.image(file.path(wsDir, 
    "meanTPbyVegClass_CMIP5_individual.RData"))
```
