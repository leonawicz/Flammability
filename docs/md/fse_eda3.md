


##
##
## fseMLE.R EDA part 3
### Statewide forest fire size log-normality

Evaluate the degree of log-normality of forest fire size distributions using observed data and modeled outputs for the statewide region.
The forest class is an aggregation of black spruce, white spruce, and deciduous tree species.





#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
# Statewide forest observed and simulation replicate 1
d.sf <- subset(d, Domain == "Statewide" & Vegetation == "Forest" & Replicate %in% 
    c("Observed", "Rep 0"), select = c(2, 5, 7))
p03a <- check_lnorm(d.sf, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
p03a()
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.5504, p-value = 0.1525
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_all-1.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 2.2632, p-value = 8.894e-06
```

#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
check_lnorm_dec("p03", d.sf, dec, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.6595, p-value = 0.08272
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-1.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 4.8251, p-value = 4.978e-12
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.489, p-value = 0.2174
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-2.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 2.9442, p-value = 1.891e-07
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.2872, p-value = 0.6144
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-3.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 2.1299, p-value = 1.894e-05
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.3724, p-value = 0.4137
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-4.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 1.6118, p-value = 0.0003597
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.4013, p-value = 0.354
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-5.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 2.2208, p-value = 1.131e-05
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.9942, p-value = 0.01219
```

![](fse_eda3_files/figure-html/lnorm_sw_forest_decades-6.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 1.7813, p-value = 0.0001372
```

```
## NULL
```

```r
dev.off()
```

```
## null device 
##           1
```
