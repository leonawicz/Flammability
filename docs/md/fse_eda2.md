


##
##
## fseMLE.R EDA part 2
### Noatak shrub fire size log-normality

Evaluate the degree of log-normality of shrub fire size distributions using observed data and modeled outputs for the Noatak region.



#### Function for log-normality assessment


```r
# Functions to assess log-normality of fire size for observed data and a
# sample simulation replicate
check_lnorm <- function(d, nmax.ad.test = 100, verbose = TRUE, closure = TRUE, 
    ...) {
    f <- function() {
        require("nortest")
        dl <- split(d$FSE, d$Replicate)
        if (length(dl) == 2) 
            iters <- 1:2 else if (names(dl) == "Observed") 
            iters <- 1 else iters <- 2
        id <- c("observations", "simulations")[iters]
        if (length(iters) == 1) 
            iters <- 1
        layout(matrix(1:(3 * length(iters)), length(iters), byrow = T))
        for (i in iters) {
            x <- dl[[i]]
            logx <- log(x + runif(length(x), -0.95, 0.95))
            hist(x, main = paste("Histrogram of", id[i]), ...)
            hist(logx, main = paste0("Histrogram of log(", id[i], " + uniform noise)"), 
                ...)
            qqnorm(logx, main = paste("Q-Q plot:", id[i]), ...)
            qqline(logx, main = paste("Q-Q plot:", id[i]), ...)
            if (verbose) {
                if (length(logx) > 7) 
                  print(ad.test(sample(logx, min(length(logx), nmax.ad.test)))) else print("Sample too small for Anderson-Darling normality test.")
            }
        }
    }
    if (closure) 
        return(f) else f()
}

check_lnorm_dec <- function(id, d, dec, i.offset = 1, ...) {
    for (i in 1:length(dec)) {
        pid <- paste0(id, letters[i + i.offset])
        assign(pid, check_lnorm(subset(d, Decade == dec[i]), ...), pos = 1)
        get(pid)()
    }
    return(NULL)
}
```

#### Observed and sample replicate simulation fire sizes

The first replicate was selected.


```r
# Noatak shrub observed and simulation replicate 1
set.seed(8923)
d.sf <- subset(d, Domain == "Noatak" & Vegetation == "Shrub" & Replicate %in% 
    c("Observed", "Rep 0"), select = c(2, 5, 7))
p02a <- check_lnorm(d.sf, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
p02a()
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.553, p-value = 0.1438
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_all-1.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.3384, p-value = 0.4913
```

#### Observed and sample replicate simulation fire sizes by decade

The first replicate was selected.


```r
check_lnorm_dec("p02", d.sf, dec, col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-1.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.1061, p-value = 0.9908
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-2.png) 

```
## [1] "Sample too small for Anderson-Darling normality test."
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.8393, p-value = 0.02186
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-3.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.193, p-value = 0.8501
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.2803, p-value = 0.5522
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-4.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.4562, p-value = 0.2138
```

```
## [1] "Sample too small for Anderson-Darling normality test."
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-5.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.3408, p-value = 0.4458
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.647, p-value = 0.07041
```

![](fse_eda2_files/figure-html/lnorm_noa_shrub_decades-6.png) 

```
## 
## 	Anderson-Darling normality test
## 
## data:  sample(logx, min(length(logx), nmax.ad.test))
## A = 0.3189, p-value = 0.4727
```

```
## NULL
```
