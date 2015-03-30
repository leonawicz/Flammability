


##
##
## fseMLE.R









### Noatak shrub fire size distributions 1950 - 2009

Note that historically observed fire is near zero during the 1950s and 1960s for this region.

#### Full period


```r
# Noatak shrub all years, all replicates
d.sub <- subset(d, Domain == "Noatak" & Vegetation == "Shrub")
pars <- do_mle_fes(d.sub, dec = dec, by.decade = TRUE)
(p04a <- plot_mle_fes(d.sub, pars[1]))
```

![](fse_mle2_files/figure-html/mle_noa_shrub_all-1.png) 

#### By decade


```r
for (i in 1:length(dec)) print(assign(paste0("p04", letters[i + 1]), plot_mle_fes(subset(d.sub, 
    Decade == dec[i]), pars[i + 1])))
```

![](fse_mle2_files/figure-html/mle_noa_shrub_dec-1.png) ![](fse_mle2_files/figure-html/mle_noa_shrub_dec-2.png) ![](fse_mle2_files/figure-html/mle_noa_shrub_dec-3.png) ![](fse_mle2_files/figure-html/mle_noa_shrub_dec-4.png) ![](fse_mle2_files/figure-html/mle_noa_shrub_dec-5.png) ![](fse_mle2_files/figure-html/mle_noa_shrub_dec-6.png) 
