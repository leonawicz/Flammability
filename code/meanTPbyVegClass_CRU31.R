####################################################################################
#### This R script calculates mean temperature and climate by vegetation class. ####
####################################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/12/2015        ####

# @knitr setup
comargs <- (commandArgs(TRUE))
if(length(comargs)) for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(!exists("allcavm")) allcavm <- FALSE
if(!is.logical(allcavm)) stop("Argument 'allcavm' must be logical.")

library(raster)
library(parallel)

setwd("/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/workspaces")

r.veg <- raster("../data/alf2005.cavm.merged.030212.tif")
veg.vec <- getValues(r.veg)
sort(unique(veg.vec))
veg.vec[veg.vec==3|veg.vec==4] <- 2 # for forest
if(allcavm) veg.vec[veg.vec==6|veg.vec==7] <- 5 # "cavm" all three shrub, graminoid, wetland combined

veg.vals <- if(allcavm) 5 else c(1,2,5,6,7)
veg.names <- if(allcavm) "cavm" else c("tundra","forest","shrub","graminoid","wetland")
modnames <- "cru_TS31"
scenario <- "historical"
path <- file.path("/big_scratch/mfleonawicz/Climate_1km_AKstatewide", scenario, modnames, c("pr", "tas"))
dir.create(outDir <- file.path("../data/meanTPbyVegClass", scenario, modnames), showWarnings=F, recursive=T)
dir.create(wsDir <- "meanTPbyVegClass", showWarnings=F)

### pick the years and months for the analysis
yr.start<-1901
yr.end<-2009

# @knitr func_means
f <- function(k, path, veg.vec, veg.vals, veg.names){
	paths <- list.files(path,full=T)
	ind <- which(as.numeric(substr(paths,nchar(paths)-7,nchar(paths)-4))==k)
	paths <- paths[ind]
	precip.paths <- paths[1:12]
	temp.paths <- paths[13:24]
    precip.tmp <- getValues(stack(precip.paths))
    temp.tmp <- getValues(stack(temp.paths))
    means.P.mos.veg <- means.T.mos.veg <- c()
	for(j in 1:length(veg.vals)){
		means.P.mos.veg <- cbind(means.P.mos.veg, round(colMeans(precip.tmp[veg.vec==veg.vals[j],],na.rm=T)))
		means.T.mos.veg <- cbind(means.T.mos.veg, round(colMeans(temp.tmp[veg.vec==veg.vals[j],],na.rm=T), 1))
	}	
    row.names(means.P.mos.veg) <- row.names(means.T.mos.veg) <- month.abb
    colnames(means.P.mos.veg) <- colnames(means.T.mos.veg) <- veg.names
	print(k)
    return(list(Pmeans=means.P.mos.veg, Tmeans=means.T.mos.veg, k=k-yr.start+1))
}

# @knitr func_means_boot
# not in use
f2 <- function(k, path, veg.vec, veg.vals, veg.names, boot=NULL, seed=2504){
	paths <- list.files(path,full=T)
	ind <- which(as.numeric(substr(paths,nchar(paths)-7,nchar(paths)-4))==k)
	paths <- paths[ind]
	precip.paths <- paths[1:12]
	temp.paths <- paths[13:24]
    precip.tmp <- getValues(stack(precip.paths, quick=T))
    temp.tmp <- getValues(stack(temp.paths, quick=T))
	if(!is.null(boot)){
		means.P.mos.veg.list <- means.T.mos.veg.list <- list()
		for(b in 1:boot){
		    means.P.mos.veg <- means.T.mos.veg <- c()
			for(j in 1:length(veg.vals)){
				ind2 <- !is.na(veg.vec) & veg.vec==veg.vals[j]
				pre <- precip.tmp[ind2,]
				tas <- temp.tmp[ind2,]
				samp <- sample(1:nrow(pre),n)
				means.P.mos.veg <- cbind(means.P.mos.veg, colMeans(pre[samp,],na.rm=T))
				means.T.mos.veg <- cbind(means.T.mos.veg, colMeans(tas[samp,],na.rm=T))
			}
			row.names(means.P.mos.veg) <- row.names(means.T.mos.veg) <- mo.abb
			colnames(means.P.mos.veg) <- colnames(means.T.mos.veg) <- veg.names
			means.P.mos.veg.list[[b]] <- means.P.mos.veg
			means.T.mos.veg.list[[b]] <- means.T.mos.veg
		}
	} else {	
		for(j in 1:length(veg.vals)){
			ind2 <- !is.na(veg.vec) & veg.vec==veg.vals[j]
			pre <- precip.tmp[ind2,]
			tas <- temp.tmp[ind2,]
			means.P.mos.veg <- cbind(means.P.mos.veg, colMeans(pre,na.rm=T))
			means.T.mos.veg <- cbind(means.T.mos.veg, colMeans(tas,na.rm=T))
		}
		row.names(means.P.mos.veg) <- row.names(means.T.mos.veg) <- mo.abb
		colnames(means.P.mos.veg) <- colnames(means.T.mos.veg) <- veg.names
	}	
    if(is.null(boot)){
		return(list(Pmeans=means.P.mos.veg, Tmeans=means.T.mos.veg))
	} else {
		return(list(Pmeans=means.P.mos.veg.list, Tmeans=means.T.mos.veg.list))
	}
	print(k)
}

# @knitr run
f.out <- mclapply(yr.start:yr.end, f, path=path, veg.vec=veg.vec, veg.vals=veg.vals, veg.names=veg.names, mc.cores=min(length(yr.start:yr.end), 32))
names(f.out) <- yr.start:yr.end
for(i in 1:length(veg.vals)){
  assign(paste("table.p", veg.names[i], scenario, modnames, sep="."), c())
  assign(paste("table.t", veg.names[i], scenario, modnames, sep="."), c())
}
for(j in 1:length(f.out)){
  for(i in 1:length(veg.vals)){
    assign(paste("table.p", veg.names[i], scenario, modnames, sep="."), rbind( get(paste("table.p", veg.names[i], scenario, modnames, sep=".")), c("Year"=as.numeric(names(f.out)[j]),f.out[[j]]$Pmeans[,i]) ))
    assign(paste("table.t", veg.names[i], scenario, modnames, sep="."), rbind( get(paste("table.t", veg.names[i], scenario, modnames, sep=".")), c("Year"=as.numeric(names(f.out)[j]),f.out[[j]]$Tmeans[,i]) ))
  }
}

# @knitr run_boot
#not in use

#boot <- 10 #boot <- NULL
#n <- 100
#set.seed(47)
#f.out <- mcf(f2,yr.start:yr.end)
#names(f.out) <- yr.start:yr.end
#for(i in 1:length(veg.vals)){
#	for(b in 1:length(f.out[[1]]$Pmeans)){
#	  assign(paste("table.p.",veg.names[i],".",b,sep=""), c())
#	  assign(paste("table.t.",veg.names[i],".",b,sep=""), c())
#  }
#}
#for(j in 1:length(f.out)){
#	for(b in 1:length(f.out[[1]]$Pmeans)){
#	  for(i in 1:length(veg.vals)){
#		assign(paste("table.p.",veg.names[i],".",b,sep=""), rbind( get(paste("table.p.",veg.names[i],".",b,sep="")), c("Year"=as.numeric(names(f.out)[j]),f.out[[j]]$Pmeans[[b]][,i]) ))
#		assign(paste("table.t.",veg.names[i],".",b,sep=""), rbind( get(paste("table.t.",veg.names[i],".",b,sep="")), c("Year"=as.numeric(names(f.out)[j]),f.out[[j]]$Tmeans[[b]][,i]) ))
#	  }
#  }
#}

# @knitr save
for(i in 1:length(veg.names)){
  write.table(get(paste("table.p", veg.names[i], scenario, modnames, sep=".")), paste(outDir, "/pr_", scenario, "_", modnames, "_", veg.names[i], ".csv", sep=""), row.names=F)
  write.table(get(paste("table.t", veg.names[i], scenario, modnames, sep=".")), paste(outDir, "/tas_", scenario, "_", modnames, "_", sort(veg.names)[i], ".csv", sep=""), row.names=F)
}

rm(comargs, i, j, f.out, yr.start, yr.end, veg.vals, veg.vec, path, f, f2, modnames, scenario, r.veg, veg.names, outDir, allcavm)
if(length(ls(pattern=".*.cavm.*."))) save.image(file.path(wsDir, "meanTPbyVegClass_CRU31_cavm.RData")) else save.image(file.path(wsDir, "meanTPbyVegClass_CRU31_individual.RData"))
