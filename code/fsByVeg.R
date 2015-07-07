##################################################################################################################
#### This R script tables fire sizes (FS) by vegetation class and year for observed data and ALFRESCO outputs ####
##################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   06/06/2015        ####

# @knitr setup
comArgs <- commandArgs(TRUE)
if(length(comArgs>0)){
        arg.mat <- do.call("rbind",strsplit(comArgs,"="))
        options(warn=-1); arg.char <- which(is.na(as.numeric(arg.mat[,2]))); options(warn=0)
        if(length(arg.char>0)) arg.mat[arg.char,2] <- paste("'",arg.mat[arg.char,2],"'",sep="")
        eval(parse(text=apply(arg.mat,1,paste,collapse="=")))
}
cat(comArgs)

if(exists("yr.start") & exists("yr.end")) yrs <- yr.start:yr.end else yrs <- 1950:2013

library(raster)
library(data.table)
library(parallel)

rasterOptions(tmpdir="/big_scratch/shiny", chunksize=10e10, maxmemory=10e11)
mainDir <- file.path(input, "Maps")

# @knitr func_fsByVeg
fsByVeg <- function(i, v, f){
	v[v!=i] <- NA
	x <- f[!is.na(v) & !is.na(f)]
	if(length(x)) return(data.frame(Vegetation=i, FS=sort(as.numeric(tapply(x, x, length))))) else return(NULL)
}

# @knitr func_fsByRep
fsByRep <- function(d, mainDir, vid, v.veg, years=1950:2013){ # hardcoded years
	reps <- paste0("_",d-1,"_")
	files <- list.files(mainDir, pattern=gsub("expression","",paste(bquote(expression("^FireSc.*.",.(reps),".*.tif$")),collapse="")), full=T)
	yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
	ord <- order(yrs)
	files <- files[ord]
	yrs <- yrs[ord]
	ind <- which(yrs %in% years)
	files <- files[ind]
	yrs <- yrs[ind]
	n <- length(yrs)
	dlist <- vector("list", n)
	for(k in 1:n){
		v.fid <- getValues(raster(files[k], band=2))
		if(!all(is.na(v.fid))){
			dl <- lapply(vid, fsByVeg, v=v.veg, f=v.fid)
			dlist[[k]] <- as.data.frame(rbindlist(dl))
			dlist[[k]]$Year <- yrs[k]
		}
	}
	d <- as.data.frame(rbindlist(dlist))
	d$Source <- "Modeled"
	d$Replicate <- paste("Rep", gsub("_", "", reps))
	d <- d[,c(4,5,1,3,2)]
	d
}

# @knitr func_fsByRepEmp
fsByRepEmp <- function(i, b, vid, v.veg, yrs){
	v.fid <- getValues(subset(b, i))
	if(all(is.na(v.fid))) return(NULL)
	dl <- lapply(vid, fsByVeg, v=v.veg, f=v.fid)
	d <- as.data.frame(rbindlist(dl))
	d$Year <- yrs[i]
	d$Replicate <- d$Source <- "Observed"
	d <- d[,c(4,5,1,3,2)]
	d
}

# @knitr empirical_data_setup
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(r)
v.veg[v.veg==3 | v.veg==4] <- 2 # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")

# @knitr run
# Process empirical data
n.cores <- 32 # hardcoded
fs.emp <- mclapply(1:nlayers(b.fid), fsByRepEmp, b=b.fid, vid=vid, v.veg=v.veg, yrs=yrs.all, mc.cores=n.cores)
fs.emp <- as.data.frame(rbindlist(fs.emp))
# Process modeled data
num.reps <- 32 # hardcoded
fs.alf.list <- mclapply(1:min(num.reps, 32), fsByRep, mainDir=mainDir, vid=vid, v.veg=v.veg, years=yrs.all, mc.cores=min(num.reps, 32))
fs.alf <- as.data.frame(rbindlist(fs.alf.list))
d.fs <- rbind(fs.emp, fs.alf)
d.fs$Vegetation <- v.names[d.fs$Vegetation]
dom <- if(substr(tolower(alf.domain),1,6)=="noatak") "Noatak" else if(substr(tolower(alf.domain),1,6)=="statew") "Statewide"
save(d.fs, file=paste0(out, "/fsByVeg_df_", dom, ".RData"))

sink(file=file.path(out, "message.txt"), append=TRUE)
cat("An R workspace file containing fire event sizes partitioned by vegetation class is attached.\n")
sink()
