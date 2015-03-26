########################################################################
#### Maximum likelihood estimation of fire event size distributions ####
########################################################################

#### Script author:  Matthew Leonawicz ####
#### Maintainted by: Matthew Leonawicz ####
#### Last updated:   03/26/2015        ####

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
d$Decade <- paste0(d$Decade, "s")
d <- subset(d, Year < 2010)
#veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
dec <- sort(unique(d$Decade))
n.dec <- length(dec)
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
p01c <- ggplot(data=subset(d, Domain=="Noatak"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Decade, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01c

# @knitr fc_sw_forest_dec
p01d <- ggplot(data=subset(d, Domain=="Statewide"), aes(x=Replicate, fill=Source)) + geom_bar() + facet_wrap(~ Decade, ncol=1) +
	scale_fill_manual(values=cbpal) +
	theme(axis.text.x=element_text(angle=45, hjust=1))
p01d

# @knitr func_check_lnorm
# Function to assess log-normality of fire size for observed data and a sample simulation replicate
check_lnorm <- function(d, nmax.ad.test=100, verbose=TRUE, ...){
	require("nortest")
	dl <- split(d$FSE, d$Replicate)
	if(length(dl)==2) iters <- 1:2 else if(names(dl)=="Observed") iters <- 1 else iters <- 2
	id <- c("observations", "simulations")[iters]
	if(length(iters)==1) iters <- 1
	layout(matrix(1:(3*length(iters)), length(iters), byrow=T))
	for(i in iters){
		x <- dl[[i]]
		logx <- log(x + runif(length(x), -0.95, 0.95))
		hist(x, main=paste("Histrogram of", id[i]), ...)
		hist(logx, main=paste0("Histrogram of log(", id[i], " + uniform noise)"), ...)
		qqnorm(logx, main=paste("Q-Q plot:", id[i]), ...)
		qqline(logx, main=paste("Q-Q plot:", id[i]), ...)
		if(verbose) { if(length(logx) > 7) print(ad.test(sample(logx, min(length(logx), nmax.ad.test)))) else print("Sample too small for Anderson-Darling normality test.") }
	}
}

# @knitr lnorm_noa_shrub_all
# Noatak shrub observed and simulation replicate 1
d.sf <- subset(d, Domain=="Noatak" & Vegetation=="Shrub" & Replicate %in% c("Observed", "Rep 0"), select=c(2,5,7))
p02a <- check_lnorm(d.sf, col="gray40", cex.lab=1.3, cex.axis=1.3)
# @knitr lnorm_noa_shrub_decades
for(i in 1:n.dec) assign(paste0("p02", letters[i+1]), check_lnorm(subset(d.sf, Decade==dec[i]), col="gray40", cex.lab=1.3, cex.axis=1.3))

# @knitr lnorm_sw_forest_all
# Statewide forest observed and simulation replicate 1
d.sf <- subset(d, Domain=="Statewide" & Vegetation=="Forest" & Replicate %in% c("Observed", "Rep 0"), select=c(2,5,7))
p03a <- check_lnorm(d.sf, col="gray40", cex.lab=1.3, cex.axis=1.3)
# @knitr lnorm_sw_forest_decades
for(i in 1:n.dec) assign(paste0("p03", letters[i+1]), check_lnorm(subset(d.sf, Decade==dec[i]), col="gray40", cex.lab=1.3, cex.axis=1.3))




# @knitr func_eda_pngs
savePNG <-  function(files, plots, ...) for(i in 1:length(files.out)){ png(files, ...); get(plots[i], envir=.GlobalEnv); dev.off() }

# @knitr eda_pngs_01
plots <- ls(pattern=paste0("^p01"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", rep(doms, 2), "_", c(rep("fcByVeg.png",2), "fcShrubByDec", "fcForestByDec.png"))
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr eda_pngs_02
plots <- ls(pattern=paste0("^p02"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", doms[1], "_shrub_fs", c("All", dec), "_lnormPlots.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr eda_pngs_03
plots <- ls(pattern=paste0("^p03"))
files.out <- paste0(plotDir, "/", substr(plots, 1, 4), "_", doms[2], "_forest_fs", c("All", dec), "_lnormPlots.png")
savePNG(files.out, plots, res=300, height=2000, width=3000)

# @knitr mle
n2loglik <- function(fun, params=list()) -2*sum(log(do.call(fun, params)))




# @knitr fc_sw_forest_dec
#p04a <- ggplot(data=d.sf, aes(x=FSE)) + geom_histogram() + facet_wrap(~ Decade, ncol=2, scale="free_y") #+
	#scale_fill_manual(values=cbpal) +
	#theme(axis.text.x=element_text(angle=45, hjust=1))
#p04a

#if(i<=ncol(vars.p[[j]])){
#	tmp.pars <- optim(fn=n2loglik,par=c(5,0.2))$par; tmp.pars
#} else {
#	tmp.pars <- optim(fn=n2loglik,par=c(100,2.5))$par; tmp.pars
#}
