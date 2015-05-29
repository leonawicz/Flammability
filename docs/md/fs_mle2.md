


##
##
## fsMLE.R









### Noatak shrub fire size distributions 1950 - 2009

Note that historically observed fire is near zero during the 1950s and 1960s for this region.

#### Full period


```r
# Noatak shrub all years, all replicates
d.sub <- subset(d, Domain == "Noatak" & Vegetation == "Shrub")
d.sub.stats1 <- d.sub %>% group_by(Replicate) %>% summarise(x_bar = mean(logFES), 
    s = sd(logFES)) %>% mutate(Decade = "All") %>% select(Decade, Replicate, 
    x_bar, s)
d.sub.stats2 <- d.sub %>% group_by(Decade, Replicate) %>% summarise(x_bar = mean(logFES), 
    s = sd(logFES))
d.sub.stats <- rbind(d.sub.stats1, d.sub.stats2)

pars <- do_mle_fes(d.sub, parvec = c(meanlog = 1, sdlog = 1), dec = dec, by.decade = TRUE)
pars.df <- lapply(1:length(pars), function(i, x, dec) {
    x <- data.frame(do.call(rbind, args = x[[i]]))
    x$Replicate <- factor(rownames(x), levels = rownames(x))
    names(x)[1:2] <- c("mu_hat_mle", "sigma_hat_mle")
    x$Decade <- dec[i]
    rownames(x) <- NULL
    x[, c(4, 3, 1, 2)]
}, x = pars, dec = c("All", dec))
pars.df <- do.call(rbind, pars.df)

full_join(d.sub.stats, pars.df)
```

```
## Source: local data frame [231 x 6]
## 
##    Decade Replicate    x_bar        s mu_hat_mle sigma_hat_mle
## 1     All  Observed 1.937424 1.570065   1.900859      1.973995
## 2     All     Rep 0 2.228832 1.722947   2.056570      2.401957
## 3     All     Rep 1 2.254246 1.757604   2.184129      2.324397
## 4     All     Rep 2 1.934414 1.660181   1.943900      1.924191
## 5     All     Rep 3 1.858798 1.852438   1.754514      1.963088
## 6     All     Rep 4 1.847757 1.695290   1.821808      1.873508
## 7     All     Rep 5 1.501279 1.899732   1.409062      1.596920
## 8     All     Rep 6 2.333529 1.860580   2.177198      2.496054
## 9     All     Rep 7 1.791833 1.815604   1.746344      1.839091
## 10    All     Rep 8 1.360343 1.535869   1.465711      1.256118
## ..    ...       ...      ...      ...        ...           ...
```

```r
(p04a <- plot_mle_fes(d.sub, pars[1]))
```

![](fs_mle2_files/figure-html/mle_noa_shrub_all-1.png) 

#### By decade


```r
for (i in 1:length(dec)) print(assign(paste0("p04", letters[i + 1]), plot_mle_fes(subset(d.sub, 
    Decade == dec[i]), pars[i + 1])))
```

![](fs_mle2_files/figure-html/mle_noa_shrub_dec-1.png) ![](fs_mle2_files/figure-html/mle_noa_shrub_dec-2.png) ![](fs_mle2_files/figure-html/mle_noa_shrub_dec-3.png) ![](fs_mle2_files/figure-html/mle_noa_shrub_dec-4.png) ![](fs_mle2_files/figure-html/mle_noa_shrub_dec-5.png) ![](fs_mle2_files/figure-html/mle_noa_shrub_dec-6.png) 
