##################################################################################################################
#### Basic plots for classic statewide AK (Seward Peninsula + Interior) or Noatak domain ALFRESCO Calibration ####
##################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/27/2015        ####

# @knitr alf_calib
comArgs <- commandArgs(TRUE)
print(comArgs)
if(length(comArgs>0)){
	arg.mat <- do.call("rbind",strsplit(comArgs,"="))
	options(warn=-1); arg.char <- which(is.na(as.numeric(arg.mat[,2]))); options(warn=0)
	if(length(arg.char>0)) arg.mat[arg.char,2] <- paste("'",arg.mat[arg.char,2],"'",sep="")
	eval(parse(text=apply(arg.mat,1,paste,collapse="=")))
}

if(exists("main")) dir.create(mainDir <- main, showWarnings=F) else stop("must provide 'main' directory")
if(exists("input")) dir.create(mainDir <- input, showWarnings=F) else stop("must provide 'input' directory")
if(exists("out")) dir.create(outDir <- out, showWarnings=F) else stop("must provide 'out' directory")
if(exists("yr.start") & exists("yr.end")) yrs <- yr.start:yr.end else yrs <- 1950:2013
if(!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if(substr(tolower(alf.domain),1,6)=="statew") alf.domain <- "Statewide" else if(substr(tolower(alf.domain),1,6)=="noatak") alf.domain <- substr(alf.domain,1,6)

# @knitr func_fseByVeg
fseByVeg <- function(i, v, f){
	v[v!=i] <- NA
	x <- f[!is.na(v) & !is.na(f)]
	if(length(x)) return(data.frame(Vegetation=i, FSE=sort(as.numeric(tapply(x, x, length))))) else return(NULL)
}

# @knitr func_fseByRepEmp
fseByRepEmp <- function(b, vid, v.veg, yrs){
	n <- nlayers(b)
	dlist <- vector("list", n)
	for(k in 1:n){
		v.fid <- getValues(subset(b, k))
		if(!all(is.na(v.fid))){
			dl <- lapply(vid, fseByVeg, v=v.veg, f=v.fid)
			dlist[[k]] <- as.data.frame(rbindlist(dl))
			dlist[[k]]$Year <- yrs[k]
		}
	}
	d <- as.data.frame(rbindlist(dlist))
	d$Replicate <- d$Source <- "Observed"
	d <- d[,c(4,5,1,3,2)]
	d
}

# @knitr empirical_data_setup
library(raster)
source("/big_scratch/shiny/obs_fire_setup.R")
v.veg <- getValues(r)
v.veg[v.veg==3 | v.veg==4] <- 2 # 3 and 4 tree classes combine into class 2 to become 'forest', tundra types 1, 5, 6, and 7 remain as before
vid <- sort(unique(v.veg[!is.na(v.veg) & v.veg > 0]))
v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")

# @knitr run
# Process empirical data
library(data.table)
d.fse.veg <- fseByRepEmp(b=b.fid, vid=vid, v.veg=v.veg, yrs=yrs.all)
d.fse.veg$Vegetation <- v.names[d.fse.veg$Vegetation]

lapply(paste0("/big_scratch/mfleonawicz/Alf_Files_20121129/alfresco/", c("CABvsTimePlot.R", "histPrep.R", "AByearPlot.R", "fireSizePlot.R", "CABvsFSPlot.R")), source)

#### Collect ALFRESCO data
alf.fse<-as.matrix(read.table(file.path(mainDir, "FireSizeEvents.txt"), header=T))
numrep <- length(unique(alf.fse[,2]))
alf.fs <- as.matrix(read.table(file.path(mainDir,"FireSize.txt"),skip=1))[,2:(numrep + 1)]

# these functions use hardcoded inputs from another script/workspace
CABvsTimePlot(yrs, baseline.year=baseline.year, d.obs.fse=d.fse.veg) 
AByearPlot(alf.fs, d.obs.fse=d.fse.veg, yrs, domain="total.ab.ha", domain.name="total", baseline=baseline.year)
fireSizePlot(yrs, d.obs.fse=d.fse.veg)
CABvsFSPlot(yrs, d.obs.fse=d.fse.veg)

save.image(file.path(outDir, "postProcess.RData"))

sink(file=file.path(outDir, "message.txt"))
cat(
	"This message comes from the shiny user directory.\n
	See ALFRESCO calibration figures [attached].\n"
)
sink()
