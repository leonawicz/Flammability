########################################################################
#### Maximum likelihood estimation of fire event size distributions ####
########################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/25/2015        ####

# @knitr setup
setwd("C:/github/Flammability/workspaces")
load("fseByVeg_df_Noatak.RData")
d.fse.veg$Domain <- "Noatak"
d <- d.fse.veg
load("fseByVeg_df_Statewide.RData")
d.fse.veg$Domain <- "Statewide"
d <- rbind(d, d.fse.veg)
rm(d.fse.veg)

d <- transform(d, Decade=Year - Year %% 10)
#veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
doms <- c("Noatak", "Statewide")

d.fc <- expand.grid(Vegetation=veg.names, Replicate=reps, Domain=doms, FC=0)
d.fc$Source <- "Modeled"
d.fc$Source[d.fc$Replicate=="Observed"] <- "Observed"

countFires <- function(i, d, x, dom){
	d <- subset(d, Vegetation==x[i] & Domain==dom, select=2)
	tapply(d$Replicate, d$Replicate, length)
}

fill_fc_df <- function(d, n.fires){
	for(i in 1:length(n.fires)){
		v <- n.fires[[i]]
		id <- names(v)
		ord <- order(as.numeric(gsub("[a-z A-Z]", "0", id)))
		d$FC[d$Vegetation==veg.names[i] & d$Replicate %in% id[ord]] <- v
	}
	d
}

n.fires.noa <- lapply(1:length(veg.names), countFires, d=d, x=veg.names, dom=doms[1])
n.fires.sw <- lapply(1:length(veg.names), countFires, d=d, x=veg.names, dom=doms[2])
d.fc <- fill_fc_df(d.fc, n.fires.noa)
d.fc <- fill_fc_df(d.fc, n.fires.sw)

library(ggplot2)
dir.create(plotDir <- "../plots/fseMLE", showWarnings=FALSE)
cbpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# @knitr eda_fc_noa
# Observed and modeled fire counts by vegetation class
p01a <- ggplot(data=subset(d.fc, Domain=="Noatak"), aes(x=Replicate, y=FC, fill=Source)) + geom_bar(stat="identity") + facet_wrap(~ Vegetation, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01a

# @knitr eda_fc_sw
p01b <- ggplot(data=subset(d.fc, Domain=="Statewide"), aes(x=Replicate, y=FC, fill=Source)) + geom_bar(stat="identity") + facet_wrap(~ Vegetation, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01b

# @knitr eda_pngs
png(paste0(plotDir, "/", doms[1], "_fireCountByVeg_.png"), res=300, height=2000, width=3000)
p01a
dev.off()

png(paste0(plotDir, "/", doms[2], "_fireCountByVeg.png"), res=300, height=2000, width=3000)
p01b
dev.off()







# @knitr mle
#n2loglik <- function(pars) -2*sum(log(dgamma(tmp[,i], pars[1], pars[2])))
#if(i<=ncol(vars.p[[j]])){
#	tmp.pars <- optim(fn=n2loglik,par=c(5,0.2))$par; tmp.pars
#} else {
#	tmp.pars <- optim(fn=n2loglik,par=c(100,2.5))$par; tmp.pars
#}
