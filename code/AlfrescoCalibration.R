##################################################################################################################
#### Basic plots for classic statewide AK (Seward Peninsula + Interior) or Noatak domain ALFRESCO Calibration ####
##################################################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/19/2015        ####

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
if(exists("yr.start") & exists("yr.end")) yrs <- yr.start:yr.end else yrs <- 1950:2011
if(!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if(substr(tolower(alf.domain),1,6)=="statew") alf.domain <- "Statewide" else if(substr(tolower(alf.domain),1,6)=="noatak") alf.domain <- substr(alf.domain,1,6)

#setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code")
#load(paste0("../workspaces/alfresco/Alf_Calib_Setup_112912_", alf.domain, ".RData"))
#lapply(c("CABvsTimePlot.R", "AByearPlot.R", "fireSizePlot.R", "CABvsFSPlot.R"), source)
load(paste0("/big_scratch/mfleonawicz/Alf_Calib_Setup_112912_", alf.domain, ".RData"))
lapply(paste0("/big_scratch/mfleonawicz/Alf_Files_20121129/alfresco/", c("CABvsTimePlot.R", "histPrep.R", "AByearPlot.R", "fireSizePlot.R", "CABvsFSPlot.R")), source)

#### Collect ALFRESCO data
alf.fse<-as.matrix(read.table(file.path(mainDir, "FireSizeEvents.txt"), header=T))
numrep <- length(unique(alf.fse[,2]))
alf.fs <- as.matrix(read.table(file.path(mainDir,"FireSize.txt"),skip=1))[,2:(numrep + 1)]

# these functions use hardcoded inputs from another script/workspace
CABvsTimePlot(yrs, baseline.year=baseline.year) 
AByearPlot(alf.fs, yrs, domain="total.ab.ha", domain.name="total", baseline=baseline.year)
fireSizePlot(yrs)
CABvsFSPlot(yrs)

save.image(file.path(outDir,"postProcess.RData"))
