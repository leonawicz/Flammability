library(rasterVis)
r <- raster("data/fmo/fmo_2017_buffered.tif")
r[r==0] <- 1

# Fire management options. Fire suppression effort ratios.
r.ig <- r.fs <- r < 5

r.ig[r==2] <- 1.25 # modified
r.ig[r==4] <- 1.5 # full

r.fs[r==2] <- 1.25 # modified
r.fs[r==4] <- 1.5 # full

writeRaster(r.ig, "data/fmo/fmo_2017_buffered_ig.tif", overwrite=TRUE, datatype='FLT4S')
writeRaster(r.fs, "data/fmo/fmo_2017_buffered_fs.tif", overwrite=TRUE, datatype='FLT4S')

# FMO ratios plots
r1 <- ratify(r.ig)
r2 <- ratify(r.fs)
Cairo::CairoPNG("data/fmo/fmo_ratios_ignition.png", width=1000, height=1000)
levelplot(r1, att="ID", col.regions=RColorBrewer::brewer.pal(9, "Spectral")[seq_along(lev$ID)],
  maxpixels = 1e6, main="Fire suppression effort ratios: ignition factor",
  xlab=NULL, ylab=NULL, scales=list(draw=FALSE), colorkey = list(space='bottom'))
dev.off()
Cairo::CairoPNG("data/fmo/fmo_ratios_sensitivity.png", width=1000, height=1000)
levelplot(r2, att="ID", col.regions=RColorBrewer::brewer.pal(9, "Spectral")[seq_along(lev$ID)],
  maxpixels = 1e6, main="Fire suppression effort ratios: fire sensitivity",
  xlab=NULL, ylab=NULL, scales=list(draw=FALSE), colorkey = list(space='bottom'))
dev.off()

# FMO IDs plot
r <- ratify(r)
classes <- c("Domain", "Limited", "Modified", "Critical", "Full")
suppressWarnings(levels(r) <- data.frame(ID=0:4, class=factor(classes, levels=classes)))

slice_ratified <- function(x, id){
  r <- mask(x==id, x!=id, maskvalue=0, updatevalue=id)
  if(is.factor(x)) levels(r) <- levels(x)[[1]]
  r
}

s <- stack(c(purrr::map(1:4, ~slice_ratified(r, .x)), r))
names(s) <- c(classes[-1], "Stacked")
Cairo::CairoPNG("data/fmo/fmo.png", width=1200, height=800)
levelplot(s, att="class", col.regions=c("#eeeeee", RColorBrewer::brewer.pal(6, "Set2")[c(6,2,4,1)]),
  maxpixels = 1e6, main="15-km buffered FMO: individual and stacked overlapping layers",
  xlab=NULL, ylab=NULL, scales=list(draw=FALSE), colorkey = list(space='bottom'))
dev.off()
