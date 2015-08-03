
setwd("C:/github/Flammability/workspaces/tpByVeg")
dir.create(plotDir <- "../../plots/tpByVeg", showWarnings=FALSE)

library(data.table)
library(dplyr)
library(ggplot2)

dlist <- vector("list", 2)
load("tpByVeg_samples100_CRU32_individual.RData")
dlist[[1]] <- d
load("tpByVeg_samples100_CMIP5_individual.RData")
dlist[[2]] <- d
d <- rbindlist(dlist)
rm(dlist)
gc()

xvar <- "Year"
yvar <- "Temperature"
veg <- c("shrub")
mod <- "GFDL-CM3"
rcp <- "rcp60"
mos <- c("Aug")
outfile <- paste(yvar, veg, mos, rcp, mod, sep="_")

d %>% filter(Month %in% mos) %>% filter(Scenario %in% c("historical", rcp)) %>%
    filter(Model %in% c("CRU32", mod)) %>% filter(Vegetation %in% veg) %>%
    filter(Var==yvar)-> dsub

g1 <- ggplot(dsub, aes(x=factor(Year), y=Val, colour=Model)) + geom_boxplot() +
    labs(x=xvar, y=yvar, title="n=100 annual sample") + theme(legend.position="bottom") +
    scale_x_discrete(breaks=seq(1950, 2100, by=10), labels=seq(1950, 2100, by=10))

g2 <- ggplot(dsub, aes(x=Year, y=Val, group=interaction(Scenario, Model, Month, Var, Vegetation, Obs), fill=Model, colour=Model)) +
    labs(x=xvar, y=yvar, title="n=100 annual sample") +
    theme(legend.position="bottom") + guides(colour=guide_legend(override.aes=list(alpha=1))) +
    geom_line(alpha=0.2) +
    scale_x_continuous(breaks=seq(1950, 2100, by=10), labels=seq(1950, 2100, by=10))

g3 <- ggplot(dsub, aes(x=Val, fill=Model)) + geom_density(position="dodge", colour=1, alpha=0.5) +
    labs(x=yvar, y="Density", title="n=100 annual sample, pooled 1950-2013 (CRU) and 2010-2099 (GCM)") + theme(legend.position="bottom")


png(file.path(plotDir, paste0("tsBoxplots_", outfile, ".png")), width=3200, height=1600, res=200)
g1
dev.off()

png(file.path(plotDir, paste0("tsByObs_", outfile, ".png")), width=3200, height=1600, res=200)
g2
dev.off()

png(file.path(plotDir, paste0("dist_", outfile, ".png")), width=2400, height=2400, res=200)
g3
dev.off()