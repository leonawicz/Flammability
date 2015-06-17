


##
##
## tpByVeg_CRU32.R

The `tpByVeg_CRU32.R` script calculates mean temperature and precipitation over all grid cells in CRU 3.2 map layers conditional on vegetation class using a given vegetation classification map.
Alternatively, instead of computing an overall mean, the script can be passed arguments to specify sampling of climate values and a sample size.
These vegetation-specific climate means and/or samples are used as variables in gradient boosting machine (GBM), or generalized boosted regression modeling.
The resulting GBM models are used to develop climate-driven, vegetation-mediated flammability maps with a distinct nonlinear fire responses to climate for each vegetation class.
These flammability maps are used in ALFRESCO in lieu of the original temperature and precipitation maps.

## R code

### Setup


```r
comargs <- (commandArgs(TRUE))
if (length(comargs)) for (z in 1:length(comargs)) eval(parse(text = comargs[[z]]))

if (!exists("samples")) samples <- TRUE
if (!exists("allcavm")) allcavm <- FALSE
if (!exists("n") & samples) stop("Must provide n if samples=TRUE.")
if (!is.logical(samples)) stop("Argument 'samples' must be logical.")
if (!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")

library(raster)
library(parallel)

setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")

r.veg <- raster("../data/alf2005.cavm.merged.030212.tif")
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
modnames <- "CRU_TS32"
scenario <- "historical"
path <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", scenario, 
    modnames, c("pr", "tas"))
dir.create(wsDir <- "tpByVeg", showWarnings = F)

yrs <- 1950:2013
```

### Functions


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


```r
f3 <- function(k, path, veg.vec, veg.vals, veg.names, n = 100, fid = NULL, fire.only = FALSE, 
    seed = 2504) {
    paths <- list.files(path, full = T)
    ind <- which(as.numeric(substr(paths, nchar(paths) - 7, nchar(paths) - 4)) == 
        k)
    paths <- paths[ind]
    precip.paths <- paths[1:12]
    temp.paths <- paths[13:24]
    precip.tmp <- getValues(stack(precip.paths, quick = T))
    temp.tmp <- getValues(stack(temp.paths, quick = T))
    na.rows <- unique(c(which(is.na(precip.tmp), arr.ind = TRUE)[, 1], which(is.na(temp.tmp), 
        arr.ind = TRUE)[, 1]))
    mp <- mt <- c()
    nv <- length(veg.vals)
    if (!is.null(fid)) {
        fid <- getValues(raster(fid, which(as.numeric(substr(names(fid), 2, 
            5)) == k)))
        fs_df <- function(i, x, vegID) {
            if (length(x[[i]])) 
                FID <- as.numeric(names(x[[i]])) else FID <- NA
            if (length(x[[i]])) 
                FS <- as.numeric(x[[i]]) else FS <- NA
            data.frame(VegID = vegID[i], FID = FID, FS = FS)
        }
        tb <- tapply(fid, veg.vec, table)
        d.fs <- rbindlist(lapply(1:length(tb), fs_df, x = tb, vegID = as.numeric(names(tb))))
        fs <- c()
    }
    if (!is.null(seed)) 
        set.seed(seed)
    for (j in 1:nv) {
        if (!is.null(fid) && fire.only) {
            ind2 <- veg.vec == veg.vals[j] & !(1:length(veg.vec) %in% na.rows) & 
                !is.na(fid)
        } else {
            ind2 <- veg.vec == veg.vals[j] & !(1:length(veg.vec) %in% na.rows)
        }
        pre <- precip.tmp[ind2, ]
        tas <- temp.tmp[ind2, ]
        samp <- sample(1:nrow(pre), n)
        mp <- cbind(mp, as.numeric(pre[samp, ]))
        mt <- cbind(mt, as.numeric(tas[samp, ]))
        if (!is.null(fid)) {
            fid.tmp <- fid[ind2][samp]
            fs <- cbind(fs, sapply(fid.tmp, function(x, d) if (is.na(x)) 
                return(0) else return(d$FS[which(d$FID == x & d$VegID == veg.vals[j])]), d = d.fs))
        }
    }
    mp <- data.frame(mp)
    mt <- data.frame(mt)
    names(mp) <- names(mt) <- veg.names
    mp$Month <- mt$Month <- rep(month.abb, each = n)
    mp$Var <- "Precipitation"
    mt$Var <- "Temperature"
    d <- rbind(mp, mt)
    d <- cbind(Year = k, d)
    d <- d[, c(1, nv + (2:3), 2:(nv + 1))]
    if (!is.null(fid)) {
        fs <- data.frame(fs)
        names(fs) <- veg.names
        fs <- cbind(Year = k, fs)
        return(list(d = d, d.fs = fs))
    } else return(d)
}
```

### Run


```r
if (!samples) {
    f.out <- mclapply(yrs, f, path = path, veg.vec = veg.vec, veg.vals = veg.vals, 
        veg.names = veg.names, mc.cores = min(length(yrs), 32))
    names(f.out) <- yrs
    for (i in 1:length(veg.vals)) {
        assign(paste("table.p", veg.names[i], scenario, modnames, sep = "."), 
            c())
        assign(paste("table.t", veg.names[i], scenario, modnames, sep = "."), 
            c())
    }
    for (j in 1:length(f.out)) {
        for (i in 1:length(veg.vals)) {
            assign(paste("table.p", veg.names[i], scenario, modnames, sep = "."), 
                rbind(get(paste("table.p", veg.names[i], scenario, modnames, 
                  sep = ".")), c(Year = as.numeric(names(f.out)[j]), f.out[[j]]$Pmeans[, 
                  i])))
            assign(paste("table.t", veg.names[i], scenario, modnames, sep = "."), 
                rbind(get(paste("table.t", veg.names[i], scenario, modnames, 
                  sep = ".")), c(Year = as.numeric(names(f.out)[j]), f.out[[j]]$Tmeans[, 
                  i])))
        }
    }
}
```


```r
if (samples) {
    library(data.table)
    library(reshape2)
    set.seed(55)
    system.time(f.out <- mclapply(yrs, f3, path = path, veg.vec = veg.vec, veg.vals = veg.vals, 
        veg.names = veg.names, n = n, seed = NULL, mc.cores = min(length(yrs), 
            32)))
    d <- rbindlist(f.out)
    d <- melt(d, id.var = c("Year", "Month", "Var"), variable.name = "Vegetation", 
        value.name = "Val")
    if (allcavm) 
        d.cavm <- d
}
```

### Save outputs


```r
if (!samples) {
    if (allcavm) 
        save(list = ls(pattern = "^table\\."), file = file.path(wsDir, "tpByVeg_means_CRU32_cavm.RData")) else save(list = ls(pattern = "^table\\."), file = file.path(wsDir, "tpByVeg_means_CRU32_individual.RData"))
}

if (samples) {
    if (allcavm) 
        save(d.cavm, file = paste0(wsDir, "/tpByVeg_samples", n, "_CRU32_cavm.RData")) else save(d, file = paste0(wsDir, "/tpByVeg_samples", n, "_CRU32_individual.RData"))
}
```
