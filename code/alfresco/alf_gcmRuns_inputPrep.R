# @knitr prep_files
library(raster)
library(parallel)

# Two options for domains. Historical run name must be provided as well.
#domain <- "Noatak"
#run.name <- "m3TL_26150s_00255i_historical_CRU32"
#run.name <- "m5TL_24950s_0026i_historical_CRU32"

domain <- "Statewide"
####run.name <- "m3TL_24250s_002425i_historical_CRU32"
run.name <- "m5TL_24225s_00245i_historical_CRU32"

# This is needed for FM0 historical runs beginning in 1950 and all future runs beginning in 2014.
# 1949 and 2013 input maps both come from the same 1-2013 historical run.
final.year <- 2013 # 1949

gbm <- paste0(substr(run.name, 2, 2), substr(run.name, 1, 1))
inDir <- paste0("/big_scratch/shiny/Runs_", domain, "/paul.duffy_at_neptuneinc.org/", run.name, "/Maps")
dir.create(
  outDir <- paste0("/big_scratch/shiny/Final_", domain, "_", gbm, "/secondaryRunInputs"),
  showWarnings=FALSE, recursive=TRUE)
files <- list.files(inDir, pattern=paste0(final.year, "\\.tif"), full.names=TRUE)
files <- files[-which(substr(basename(files), 1, 8)=="FireScar")]

r.template <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/Spinup300Year_32Reps/Age_0_1900.tif")

par_copy <- function(i, r.template, outDir){
    r <- raster(files[i])
    r <- extend(r, r.template)
    if(substr(basename(files[i]), 1, 3)=="Age")
      writeRaster(r, file.path(outDir, basename(files[i])), datatype="INT4S", overwrite=T)
    if(substr(basename(files[i]), 1, 3)!="Age")
      writeRaster(r, file.path(outDir, basename(files[i])), datatype="INT1U", overwrite=T)
    return(NULL)
}

system.time( mclapply(1:length(files), par_copy, r.template=r.template, outDir=outDir, mc.cores=32) )
