


##
##
## fseMLE.R









### Statewide forest fire size distributions 1950 - 2009

#### Full period


```r
# statewide forest all years, all replicates
d.sub <- subset(d, Domain == "Statewide" & Vegetation == "Forest")
pars <- do_mle_fes(d.sub, dec = dec, by.decade = TRUE)
(p05a <- plot_mle_fes(d.sub, pars[1]))
```

![](fse_mle3_files/figure-html/mle_sw_forest_all-1.png) 

#### By decade


```r
for (i in 1:length(dec)) print(assign(paste0("p05", letters[i + 1]), plot_mle_fes(subset(d.sub, 
    Decade == dec[i]), pars[i + 1])))
```

![](fse_mle3_files/figure-html/mle_sw_forest_dec-1.png) ![](fse_mle3_files/figure-html/mle_sw_forest_dec-2.png) ![](fse_mle3_files/figure-html/mle_sw_forest_dec-3.png) ![](fse_mle3_files/figure-html/mle_sw_forest_dec-4.png) ![](fse_mle3_files/figure-html/mle_sw_forest_dec-5.png) ![](fse_mle3_files/figure-html/mle_sw_forest_dec-6.png) 
