# @knitr script
library(raster)
setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/data")
outDir <- "../workspaces/gbmFlammability"
b <- brick("historicalFireObs/firescarbrick_annual_observed_Statewide_lightning_1950_2013.tif")
r.veg <- raster("alf2005.cavm.merged.030212.tif")
v <- r.veg[]
ind.list <- lapply(1:7, function(i, x) which(x==i), x=v)
names(ind.list) <- c("alp.tundra", "mariana", "glauca", "decid", "shrub", "gram", "wetland")
yrs <- 1950:2013

getVegFire <- function(b, v){
    for(i in 1:nlayers(b)){
        fire.ind <- which(subset(b, i)[]==1)
        x <- sapply(v, function(i, x) length(intersect(x, i)), x=fire.ind)
        if(i==1) ba <- x else ba <- rbind(ba, x)
        print(nlayers(b)-i)
    }
    rownames(ba) <- NULL
    data.frame(Year=yrs, ba)
}

d.ba <- getVegFire(b, ind.list)

head(d.ba)
apply(d.ba[,-1], 2, sum)
apply(d.ba[,-1], 1, sum)

save(d.ba, file=file.path(outDir, "baByVeg_historical_1950_2013.RData"))
