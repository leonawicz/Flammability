
library(raster)
library(parallel)
r.template <- raster("/big_scratch/mfleonawicz/Alf_Files_20121129/Spinup300Year_32Reps/Age_0_1900.tif")
dir.create(outDir <- "/big_scratch/shiny/Final_Noatak_5m/gcmRunInputs", showWarnings=F, recursive=T)
files <- list.files("/big_scratch/shiny/Runs_Noatak/paul.duffy_at_neptuneinc.org/m5TL_30750s_00185i_historical_CRU32/Maps", pattern="2013\\.tif", full=T)
files <- files[-which(substr(basename(files), 1, 8)=="FireScar")]

par_copy <- function(i, r.template, outDir){
    r <- raster(files[i])
    r <- extend(r, r.template)
    if(substr(basename(files[i]), 1, 3)=="Age") writeRaster(r, file.path(outDir, basename(files[i])), datatype="INT4S", overwrite=T)
    if(substr(basename(files[i]), 1, 3)!="Age") writeRaster(r, file.path(outDir, basename(files[i])), datatype="INT1U", overwrite=T)
    return(NULL)
}

mclapply(1:length(files), par_copy, r.template=r.template, outDir=outDir, mc.cores=32)
