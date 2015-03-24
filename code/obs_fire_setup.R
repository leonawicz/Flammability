if(emp.fire.cause=="All") fah <- shapefile("/big_scratch/mfleonawicz/FAH/FireAreaHistory_11182013.shp")
if(emp.fire.cause=="Lightning") fah <- shapefile("/big_scratch/mfleonawicz/FAH/Lightning_Fires_11182013.shp")
fah <- subset(fah, FireYear >= 1950) # do nto use observed data prior to 1950
yrs <- sort(as.numeric(unique(fah@data$FireYear)))
yrs.all <- seq(min(yrs), max(yrs))

if(substr(tolower(alf.domain),1,6)=="noatak"){
	r <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/alf2005.cavm.merged.030212_Noatak.tif")
	shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/noa_basin2/Noa_basin2.shp")
} else if(substr(tolower(alf.domain),1,6)=="statew") {
	r <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/alf2005.cavm.merged.030212.tif")
	shp <- shapefile("/big_scratch/mfleonawicz/Alf_Files_20121129/statewide_shape/Alaska_Albers_ESRI.shp")
}

suffix <- paste0("_observed_", gsub("_", "", alf.domain), "_", tolower(emp.fire.cause), "_", min(yrs), "_", max(yrs), ".tif")
b.fid.name <- paste0(out, "/fireIDbrick_annual", suffix)
result.name <- paste0(out, "/firescarbrick_annual", suffix)
result2.name <- paste0(out, "/firescarlayer_total", suffix)
if(all(file.exists(c(result.name, result2.name)))){
	dummy <- capture.output( b.fid <- brick(b.fid.name) )
	dummy <- capture.output( result <- brick(result.name) )
	dummy <- capture.output( result2 <- raster(result2.name) )
} else {
	fireScarsFun <- function(x, y, year, years.avail, field=1){
		if(year %in% years.avail) {
			x <- x[x$FireYear==year,]
			x <- rasterize(x,y,field=field)
		} else {
			x <- x[x$FireYear==years.avail[1],]
			x <- rasterize(x,y,field=1)
			x[!is.na(x)] <- NA
		}
		x
	}

	fireScarsFunVec <- Vectorize(fireScarsFun,"year")
	dummy <- capture.output( b.fid <- fireScarsFunVec(x=fah, y=r, year=yrs.all, years.avail=yrs, field="FIREID") )
	b.fid <- brick(b.fid)
	dummy <- capture.output( result <- fireScarsFunVec(x=fah, y=r, year=yrs.all, years.avail=yrs) )
	result <- lapply(result, function(x, r.veg) { x[is.na(x) & !is.na(r.veg) & r.veg>0] <- 0; x }, r.veg=r)
	dummy <- capture.output( result2 <- do.call("sum",c(result, na.rm=T)) )
	result <- brick(result)
	names(result) <- names(b.fid) <- yrs.all
	names(result2) <- paste(yrs.all[1], tail(yrs.all,1), sep="_")
	dummy <- capture.output( writeRaster(b.fid, b.fid.name, datatype="FLT4S", overwrite=T) )
	dummy <- capture.output( writeRaster(result, result.name, datatype="FLT4S", overwrite=T) )
	dummy <- capture.output( writeRaster(result2, result2.name, datatype="FLT4S", overwrite=T) )
}
