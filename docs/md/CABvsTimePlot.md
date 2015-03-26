


##
##
## CABvsTimePlot.R

`CABvsTimePlot.R` stores the function `CABvsTimePlot`, which plots curves of cumulative burn area over the burnable ALFRESCO domain by time.
A curve is plotted for each simulation replicate.
The plot includes an overlay curve representing the historically observed cumulative burn area by time.

This plot is called in `AlfrescoCalibration.R` and currently is hardcoded to plot curves based on 1950 - 2011.

## R code


```r
CABvsTimePlot <- function(years, baseline.year) {
    abByYear <- tapply(emp.fse.trun1km, emp.fse.trun1km.year, sum)
    indices <- match(names(abByYear), years)
    cab.emp.all <- rep(0, length(years))
    cab.emp.all[indices] <- abByYear
    cab.emp.all <- cumsum(cab.emp.all)
    cab.alf.all <- apply(alf.fs[years - baseline.year + 1, ], 2, cumsum)
    ylm <- range(c(cab.emp.all, c(cab.alf.all)))
    png(file.path(outDir, paste("CAB_", years[1], "to", years[length(years)], 
        ".png", sep = "")), res = 120, width = 1000, height = 800)
    par(mar = c(5, 5, 4, 2) + 0.1, mfrow = c(1, 1))
    plot(0, type = "n", xlim = range(years), ylim = ylm, ylab = expression(paste(plain("Cumalitive Area Burn   "), 
        ("km"^2), sep = "")), xlab = "Year", main = paste("CAB ", years[1], 
        "-", years[length(years)], ": Statewide", sep = ""), cex.axis = 1.1, 
        cex.lab = 1.2)
    for (j in 1:numrep) lines(years, cab.alf.all[, j], lwd = 1, lty = 2, col = "dark gray")
    lines(years, cab.emp.all, lwd = 4, col = 1)
    legend("topleft", c("Historical", "ALFRESCO Rep"), col = c(1, "dark gray"), 
        lty = c(1, 2), bty = "n", cex = 1.2, lwd = c(4, 1))
    dev.off()
}
```