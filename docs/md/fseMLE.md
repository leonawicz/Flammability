


##
##
## fseMLE.R

The `fseMLE.R` script performs exploratory data analysis followed by maximum likelihood estimation of fire size distributions based on historical observations and ALFRESCO simulation outputs.
Currently under development.
Preliminary EDA plots available.

## R code and results

### Setup


```r
setwd("C:/github/Flammability/workspaces")
load("fseByVeg_df_Noatak.RData")
d.fse.veg$Domain <- "Noatak"
d <- d.fse.veg
load("fseByVeg_df_Statewide.RData")
d.fse.veg$Domain <- "Statewide"
d <- rbind(d, d.fse.veg)
rm(d.fse.veg)

d <- transform(d, Decade = Year - Year%%10)
d <- subset(d, Year < 2010)
# veg.names <- unique(d.fse.veg$Vegetation)
veg.names <- c("Alpine", "Forest", "Shrub", "Graminoid", "Wetland")
n.veg <- length(veg.names)
reps <- unique(d$Replicate)
n.reps <- length(reps)
doms <- c("Noatak", "Statewide")

library(ggplot2)
dir.create(plotDir <- "C:/github/Flammability/plots/fseMLE", showWarnings = FALSE)
cbpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
    "#D55E00", "#CC79A7")
```

##
##
### Exploratory data analysis

#### Fire count by vegetation class: Noatak


```r
# Observed and modeled fire counts by vegetation class
p01a <- ggplot(data = subset(d, Domain == "Noatak"), aes(x = Replicate, fill = Source)) + 
    geom_bar() + facet_wrap(~Vegetation, ncol = 1, scales = "free_y") + scale_fill_manual(values = cbpal) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p01a
```

![](fseMLE_files/figure-html/fc_noa_veg-1.png) 

#### Fire count by vegetation class: Statewide


```r
p01b <- ggplot(data = subset(d, Domain == "Statewide"), aes(x = Replicate, fill = Source)) + 
    geom_bar() + facet_wrap(~Vegetation, ncol = 1, scales = "free_y") + scale_fill_manual(values = cbpal) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p01b
```

![](fseMLE_files/figure-html/fc_sw_veg-1.png) 

#### Fire count by decade: Noatak shrub


```r
# Observed and modeled fire counts by decade given vegetation class
p02a <- ggplot(data = subset(d, Domain == "Noatak"), aes(x = Replicate, fill = Source)) + 
    geom_bar() + facet_wrap(~Decade, ncol = 1) + scale_fill_manual(values = cbpal) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p02a
```

![](fseMLE_files/figure-html/fc_noa_shrub_dec-1.png) 

#### Fire count by decade: Statewide forest


```r
p02b <- ggplot(data = subset(d, Domain == "Statewide"), aes(x = Replicate, fill = Source)) + 
    geom_bar() + facet_wrap(~Decade, ncol = 1) + scale_fill_manual(values = cbpal) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p02b
```

![](fseMLE_files/figure-html/fc_sw_forest_dec-1.png) 



```
## png 
##   2
```

```
## png 
##   2
```

```
## png 
##   2
```

```
## png 
##   2
```


##
##
### Maximum likelihood estimation

#```{r mle}
#```
