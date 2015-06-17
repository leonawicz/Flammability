


##
##
## baByVeg_historical.R

The `baByVeg_historical.R` script calculates annual burn area by vegetation class using empirical historical fire perimeter data.
These values inform GBM modeling.

## R code


```r
library(raster)
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data")
b <- brick("historicalFireObs/firescarbrick_annual_observed_Statewide_lightning_1950_2013.tif")
r.veg <- raster("alf2005.cavm.merged.030212.tif")
v <- r.veg[]
ind.list <- lapply(1:7, function(i, x) which(x == i), x = v)
names(ind.list) <- paste0(c("alp.tundra", "mariana", "glauca", "decid", "shrub", 
    "gram", "wetland"), ".ha")
yrs <- 1950:2013

getVegFire <- function(b, v) {
    for (i in 1:nlayers(b)) {
        fire.ind <- which(subset(b, i)[] == 1)
        x <- sapply(v, function(i, x) length(intersect(x, i)), x = fire.ind)
        if (i == 1) 
            ha <- x else ha <- rbind(ha, x)
        print(nlayers(b) - i)
    }
    rownames(ha) <- NULL
    data.frame(Year = yrs, ha)
}

d.ha <- getVegFire(b, ind.list)

head(d.ha)
apply(d.ha[, -1], 2, sum)
apply(d.ha[, -1], 1, sum)
```
