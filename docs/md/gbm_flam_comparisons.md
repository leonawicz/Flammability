# gbm_flam_comparisons.R



##
##
## gbm_flam_comparisons.R

The `gbm_flam_comparisons.R` script makes select comparisons of flammability maps with respect to GBM models used, integration of lightning probability, and ad hoc functions applied to input maps.

## R code

### Setup



### Plots


```r
# Setup
library(grid)
library("gridBase")

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


# Write PNGs from part one: comparisons Statewide
w <- h <- 1600
png(paste0(plotDir, "/gbm.flamm_", yr, "_comparisons_Statewide.png"), height = h, 
    width = w, res = 200)
p <- levelplot(s, maxpixels = ncell(s)/10, main = paste(yr, "flammability map comparisons"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()

png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Statewide.png"), height = h, width = w * 
    1.5, res = 200)
p <- levelplot(s2, maxpixels = ncell(s)/10, main = paste(yr, "3- vs. 5-GBM flammability maps"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()

# Noatak
png(paste0(plotDir, "/gbm.flamm_", yr, "_comparisons_Noatak.png"), height = h, 
    width = w * 1.5, res = 200)
p <- levelplot(s.noa, maxpixels = ncell(s.noa), main = paste(yr, "flammability map comparisons"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()

png(paste0(plotDir, "/GBM3vsGBM5_", yr, "_Noatak.png"), height = h * 1.5, width = w, 
    res = 200)
p <- levelplot(s2.noa, maxpixels = ncell(s2.noa), main = paste(yr, "3- vs. 5-GBM flammability maps"), 
    par.settings = revRasterTheme, contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
print(p)
dev.off()


# Write PNGs from part two: thresholds
png(paste0(plotDir, "/flam_spacetime_threshDist_", threshold, "_", domain, ".png"), 
    height = 2400, width = 2400, res = 200)
p <- levelplot(s, maxpixels = ncell(s), main = paste("1950-2009", threshold, 
    "flammability threshold distributions"), par.settings = revRasterTheme, 
    contour = T, margin = F)  #, at=at.vals, colorkey=colkey)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2, widths = unit(c(1, 1), "null"), 
    heights = unit(c(1, 1), "null"))))

vp <- pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
par(omi = gridOMI())
# base graphics
hist(pval3, main = paste("GBM-3 % area <=", threshold), col = "gray", xlab = "", 
    ylab = "", freq = FALSE)
popViewport()

vp <- pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
par(omi = gridOMI())
par(new = TRUE)
# base graphics
hist(pval5, main = paste("GBM-5 % area <=", threshold), col = "gray", xlab = "", 
    ylab = "", freq = FALSE)
popViewport()

# lattice plot
vp <- pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
print(p, vp = vp, newpage = FALSE)
popViewport()

popViewport()
dev.off()
```
