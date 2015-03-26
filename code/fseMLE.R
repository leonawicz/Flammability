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
d <- subset(d, Year < 2010)
#veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
doms <- c("Noatak", "Statewide")

library(ggplot2)
dir.create(plotDir <- "C:/github/Flammability/plots/fseMLE", showWarnings=FALSE)
cbpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# @knitr fc_noa_veg
# Observed and modeled fire counts by vegetation class
p01a <- ggplot(data=subset(d, Domain=="Noatak"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Vegetation, ncol=1, scales="free_y") +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01a

# @knitr fc_sw_veg
p01b <- ggplot(data=subset(d, Domain=="Statewide"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Vegetation, ncol=1, scales="free_y") +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01b

# @knitr fc_noa_shrub_dec
# Observed and modeled fire counts by decade given vegetation class
p02a <- ggplot(data=subset(d, Domain=="Noatak"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Decade, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p02a

# @knitr fc_sw_forest_dec
p02b <- ggplot(data=subset(d, Domain=="Statewide"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Decade, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p02b

# @knitr eda_pngs
png(paste0(plotDir, "/", doms[1], "_fireCountByVeg.png"), res=300, height=2000, width=3000)
p01a
dev.off()

png(paste0(plotDir, "/", doms[2], "_fireCountByVeg.png"), res=300, height=2000, width=3000)
p01b
dev.off()

png(paste0(plotDir, "/", doms[1], "_fireCountShrubByDec.png"), res=300, height=2000, width=3000)
p02a
dev.off()

png(paste0(plotDir, "/", doms[2], "_fireCountForestByDec.png"), res=300, height=2000, width=3000)
p02b
dev.off()






# @knitr mle
#n2loglik <- function(pars) -2*sum(log(dgamma(tmp[,i], pars[1], pars[2])))
#if(i<=ncol(vars.p[[j]])){
#	tmp.pars <- optim(fn=n2loglik,par=c(5,0.2))$par; tmp.pars
#} else {
#	tmp.pars <- optim(fn=n2loglik,par=c(100,2.5))$par; tmp.pars
#}
