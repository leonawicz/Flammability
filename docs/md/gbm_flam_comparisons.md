# gbm_flam_comparisons.R



##
##
## gbm_flam_comparisons.R

The `gbm_flam_comparisons.R` script makes select comparisons of flammability maps with respect to GBM models used, integration of lightning probability, and ad hoc functions applied to input maps.

## R code

### Setup


```r
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data/gbmFlammability/samples_based/historical/CRU31")
dir.create(plotDir <- "/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/plots/gbmFlammability/map_comparisons", 
    showWarnings = FALSE)

library(rasterVis)
(dirs <- list.files())
dirs3 <- dirs[1:3]
dirs5 <- dirs[5:7]

yr <- 2004
files3 <- unlist(lapply(dirs3, function(x, year) list.files(x, pattern = paste0("_", 
    year, "\\.tif$"), full = TRUE), year = yr))
files5 <- unlist(lapply(dirs3, function(x, year) list.files(x, pattern = paste0("_", 
    year, "\\.tif$"), full = TRUE), year = yr))
s <- stack(c(files3, files5))
names(s) <- paste0(rep(c("GBM3", "GBM5"), each = length(dirs3)), paste0("_Flammability", 
    c("", "_Lightning0.5", "0.5_Lightning 0.5")))
s <- brick(s)

# Check for difference between GBM 3-model and GBM 5-model versions in CAVM
# region
r1 <- raster(list.files(dirs[4], pattern = "1977.tif", full = T))
r2 <- raster(list.files(dirs[8], pattern = "1977.tif", full = T))
r1b <- raster(list.files(dirs[1], pattern = "1917.tif", full = T))
r2b <- raster(list.files(dirs[5], pattern = "1917.tif", full = T))
s2 <- stack(r1b, r2b, r1b - r2b)
names(s2) <- c("GBM3", "GBM5", "Difference")
```

### Plots


```r
# Setup
at.vals <- c(0, 0.25, 0.499, 0.75, 1)
colkey <- list(at = at.vals, labels = list(labels = c("Low", "Medium", "High", 
    "Severe"), at = at.vals + 0.125))

# Theme settings
revRasterTheme <- function(pch = 19, cex = 0.7, region = colorRampPalette(brewer.pal(9, 
    "YlOrRd")[-1])(30), ...) {
    theme <- custom.theme.2(pch = pch, cex = cex, region = region, ...)
    theme$strip.background$col <- theme$strip.shingle$col <- theme$strip.border$col <- "transparent"
    theme$add.line$lwd = 0.4
    theme
}

# Write PNGs
png(paste0(plotDir, "/gbm.flamm_", yr, "_comparisons.png"), height = 1600, width = 2400, 
    res = 200)
p <- levelplot(s, maxpixels = ncell(s)/10, main = paste(yr, "flammability map comparisons"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()

png(paste0(plotDir, "/GBM3vsGBM5_1977.png"), height = 800, width = 2400, res = 200)
p <- levelplot(s2, maxpixels = ncell(s)/10, main = paste("1977 3- vs. 5-GBM flammability maps"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()
```
