


##
##
## fseByVeg.R

The `fseByVeg.R` script carries out post-processing of ALFRESCO simulation outputs.
It is assumed that `AlfrescoCalibration.R` has already executed. This and any other post-processing **R** script are always run secondary to the primary script.

The script extracts fire event sizes (FSE) from ALFRESCO output fire scar geotiffs for each simulation replicate,
conditional on vegetation class, and combines the modeled FSE values with similarly extracted historical observations of vegetation-specific FSE.
The hardcoded vegetation classification includes separate tundra types (alpine, shrub, graminoid, and wetland)
and an aggregate forest type (black spruce, white spruce, and deciduous trees).
Hardcoded years are currently 1950 - 2009.

An **R** workspace file containing a data frame of veg-specific FSE is attached to an email which is sent from the Atlas cluster to intended recipients as part of the broader SLURM process.
This script is called by the SLURM script, `CompileData.slurm` after the initial post-processing script, `AlfrescoCalibration.R` has run.

## R code

### Setup


```r
comArgs <- commandArgs(TRUE)
if (length(comArgs > 0)) {
    arg.mat <- do.call("rbind", strsplit(comArgs, "="))
    options(warn = -1)
    arg.char <- which(is.na(as.numeric(arg.mat[, 2])))
    options(warn = 0)
    if (length(arg.char > 0)) 
        arg.mat[arg.char, 2] <- paste("'", arg.mat[arg.char, 2], "'", sep = "")
    eval(parse(text = apply(arg.mat, 1, paste, collapse = "=")))
}
cat(comArgs)

library(raster)
library(data.table)
library(parallel)

mainDir <- file.path(input, "Maps")
```

### Functions: fseByVeg

`fseByVeg` performs the basic operation of calculating FSEs by vegetation class and tabling the individual observations.


```r
fseByVeg <- function(i, v, f) {
    v[v != i] <- NA
    x <- f[!is.na(v) & !is.na(f)]
    if (length(x)) 
        return(data.frame(Vegetation = i, FSE = sort(as.numeric(tapply(x, x, 
            length))))) else return(NULL)
}
```

### Functions: fseByRep

`fseByRep` is a parallel processing wrapper to `fseByVeg`, parallelized by simulation replicate.


```r
fseByRep <- function(d, mainDir, vid, v.veg, v.fid, years = 1950:2009) {
    # hardcoded years
    reps <- paste0("_", d - 1, "_")
    files <- list.files(mainDir, pattern = gsub("expression", "", paste(bquote(expression("^FireSc.*.", 
        .(reps), ".*.tif$")), collapse = "")), full = T)
    yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
    ord <- order(yrs)
    files <- files[ord]
    yrs <- yrs[ord]
    ind <- which(yrs %in% years)
    files <- files[ind]
    yrs <- yrs[ind]
    n <- length(yrs)
    dlist <- vector("list", n)
    for (k in 1:n) {
        v.fid <- getValues(raster(files[k], band = 2))
        if (!all(is.na(v.fid))) {
            dl <- lapply(vid, fseByVeg, v = v.veg, f = v.fid)
            dlist[[k]] <- as.data.frame(rbindlist(dl))
            dlist[[k]]$Year <- yrs[k]
        }
    }
    d <- as.data.frame(rbindlist(dlist))
    d$Source <- "Modeled"
    d$Replicate <- paste("Rep", gsub("_", "", reps))
    d <- d[, c(4, 5, 1, 3, 2)]
    d
}
```

### Functions: fseByRepEmp

`fseByRepEmp` is a basic wrapper to `fseByVeg` for empirical/historical observational FSE extraction.


```r
fseByRepEmp <- function(b, vid, v.veg, yrs) {
    n <- nlayers(b)
    dlist <- vector("list", n)
    for (k in 1:n) {
        v.fid <- getValues(subset(b, k))
        if (!all(is.na(v.fid))) {
            dl <- lapply(vid, fseByVeg, v = v.veg, f = v.fid)
            dlist[[k]] <- as.data.frame(rbindlist(dl))
            dlist[[k]]$Year <- yrs[k]
        }
    }
    d <- as.data.frame(rbindlist(dlist))
    d$Replicate <- d$Source <- "Observed"
    d <- d[, c(4, 5, 1, 3, 2)]
    d
}
```

# Empirical data setup


```r
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(r)
v.veg[v.veg == 3 | v.veg == 4] <- 2  # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")
```

# Run and save results


```r
# Process empirical data
fse.emp <- fseByRepEmp(b = b.fid, vid = vid, v.veg = v.veg, yrs = yrs.all)
# Process modeled data
num.reps <- 32  # hardcoded
fse.alf.list <- mclapply(1:min(num.reps, 32), fseByRep, mainDir = mainDir, vid = vid, 
    v.veg = v.veg, mc.cores = min(num.reps, 32))
fse.alf <- as.data.frame(rbindlist(fse.alf.list))
d.fse.veg <- rbind(fse.emp, fse.alf)
d.fse.veg$Vegetation <- v.names[d.fse.veg$Vegetation]
dom <- if (substr(tolower(alf.domain), 1, 6) == "noatak") "Noatak" else if (substr(tolower(alf.domain), 
    1, 6) == "statew") "Statewide"
save(d.fse.veg, file = paste0(out, "/fseByVeg_df_", dom, ".RData"))

sink(file = file.path(out, "message.txt"), append = TRUE)
cat("An R workspace file containing fire event sizes partitioned by vegetation class is attached.\n")
sink()
```