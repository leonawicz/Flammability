


##
##
## gbm_lightning_coefficients.R

The `gbm_lightning_coefficients.R` script assembles GBM lightning strike point model historical fitted values and backcast and forecast predictions.
Historical and future values are loaded via workspace file.

The script also provides exploratory graphs of the assembled data.
It presents the discrete classification of annual lightning intensity with several visualizations
including time series, ranked and ordered values, and empirical CDF.

Plots are made of exclusively CRU 3.2 based fitted values and backcast predictions.
Similar multi-panel plots are then made comparing each of five CMIP5 GCMs' predicted values with CRU 3.2 as a comparative baseline.

The finalized compiled data table including discrete classified annual lightning intensity coefficients is saved to a new workspace to be sourced by other scripts.
For example, it is sourced by `FlammabilityMapMultipliers.R` for applying a scalar multiplier to GBM vegetation flammability maps used in ALFRESCO simulations.

## R code

### Setup


```r
setwd("C:/github/Flammability/workspaces")
dir.create(plotDir <- "../plots/lightning", showWarnings = FALSE)

load("hist_rcp60_lightning_preds.RData")

library(reshape2)
library(ggplot2)
library(data.table)
library(dplyr)
```

### Support functions


```r
get_classes1 <- function(x, y = qtiles) cut(x, breaks = c(0, y, 99999), labels = F)
get_coefficients <- function(x) sapply(x, function(y) switch(y, `1` = 0.05, 
    `2` = 0.5, `3` = 0.95))
get_classes2 <- function(x) factor(x, labels = c("Low", "Medium", "High"))

# knitr quantiles
lb <- 0.2
ub <- 0.8
qtiles <- quantile(pred.light.hist, c(lb, ub))
bins.cru <- get_classes1(pred.light.hist)
coef.cru <- get_coefficients(bins.cru)
bins.cru <- get_classes2(bins.cru)
d <- data.table(Period = "historical", Model = "CRU32", Year = 1950:2011, LightPred = pred.light.hist, 
    Rank = rank(pred.light.hist), Class = bins.cru, Coef = coef.cru, ECDF = ecdf(pred.light.hist)(pred.light.hist))
```

### Quantiles and classification



### CRU 3.2 plots


```r
(g1 <- ggplot(data = d, aes(x = Rank, y = LightPred, label = Year)) + geom_hline(yintercept = qtiles, 
    linetype = 2) + geom_point() + geom_text(aes(colour = Class), hjust = 0, 
    vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1, y = qtiles, 
    label = c(paste("qunatile =", lb), paste("quantile =", ub)), size = 3, vjust = -0.5) + 
    labs(x = "Predicted rank", y = "Predicted number of strikes", title = "1950-2011 GBM-predicted summer lightning strikes: ranked and ordered"))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-1.png) 

```r
(g2 <- ggplot(data = d, aes(x = Year, y = LightPred, label = Rank)) + geom_line() + 
    geom_hline(yintercept = qtiles, linetype = 2) + geom_point() + geom_text(aes(colour = Class), 
    hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1950, 
    y = qtiles, label = c(paste("qunatile =", lb), paste("quantile =", ub)), 
    size = 3, vjust = -0.5) + labs(x = "Year", y = "Predicted number of strikes", 
    title = "1950-2011 GBM-predicted summer lightning strikes: time series"))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-2.png) 

```r
(g3 <- ggplot(data = d, aes(x = LightPred, label = Year)) + geom_vline(xintercept = qtiles, 
    linetype = 2) + stat_ecdf() + stat_ecdf(geom = "point") + geom_text(aes(y = ECDF, 
    colour = Class), hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", 
    x = qtiles, y = 0, label = c(paste("qunatile =", lb), paste("quantile =", 
        ub)), size = 3, hjust = -0.1) + labs(x = "Predicted number of strikes", 
    y = "CDF", title = "1950-2011 GBM-predicted summer lightning strikes: empirical CDF"))
```

![](gbm_lightning_coefficients_files/figure-html/plots_cru32-3.png) 

### RCP 6.0 CMIP5 GCM comparisons with CRU 3.2


```r
d2 <- data.table(lightning.preds)
d2 <- data.table(melt(d2, measure.vars = names(d2)))
d2[, `:=`(Period, "rcp60")]
```



variable            value  Period 
-------------  ----------  -------
CCSM4           25962.751  rcp60  
CCSM4           11762.282  rcp60  
CCSM4           30404.038  rcp60  
CCSM4           35891.522  rcp60  
CCSM4           23657.608  rcp60  
CCSM4            9942.099  rcp60  
CCSM4           25051.641  rcp60  
CCSM4           25962.751  rcp60  
CCSM4           10022.518  rcp60  
CCSM4           25962.751  rcp60  
CCSM4           35973.960  rcp60  
CCSM4           24979.394  rcp60  
CCSM4           27518.329  rcp60  
CCSM4           27073.715  rcp60  
CCSM4           14227.483  rcp60  
CCSM4           15012.245  rcp60  
CCSM4           14052.403  rcp60  
CCSM4           25820.306  rcp60  
CCSM4           27693.848  rcp60  
CCSM4           12559.473  rcp60  
CCSM4           35355.595  rcp60  
CCSM4           36777.740  rcp60  
CCSM4           36385.663  rcp60  
CCSM4           25962.751  rcp60  
CCSM4           15362.057  rcp60  
CCSM4           35172.857  rcp60  
CCSM4           35967.455  rcp60  
CCSM4            9675.916  rcp60  
CCSM4           23902.194  rcp60  
CCSM4           24503.627  rcp60  
CCSM4           12265.926  rcp60  
CCSM4           26376.983  rcp60  
CCSM4           27918.833  rcp60  
CCSM4           35234.698  rcp60  
CCSM4           11537.714  rcp60  
CCSM4           33775.752  rcp60  
CCSM4           28519.264  rcp60  
CCSM4           33034.586  rcp60  
CCSM4           34003.478  rcp60  
CCSM4           24489.414  rcp60  
CCSM4           24849.554  rcp60  
CCSM4           24167.996  rcp60  
CCSM4           25879.784  rcp60  
CCSM4           20303.698  rcp60  
CCSM4           23655.700  rcp60  
CCSM4           17347.002  rcp60  
CCSM4           21579.264  rcp60  
CCSM4           34990.050  rcp60  
CCSM4           27499.287  rcp60  
CCSM4           34990.050  rcp60  
CCSM4           20303.698  rcp60  
CCSM4           22879.978  rcp60  
CCSM4           25962.751  rcp60  
CCSM4           34811.096  rcp60  
CCSM4           25395.497  rcp60  
CCSM4           34126.102  rcp60  
CCSM4           20371.259  rcp60  
CCSM4           25103.707  rcp60  
CCSM4           35355.595  rcp60  
CCSM4           26048.189  rcp60  
CCSM4           33634.672  rcp60  
CCSM4           35355.595  rcp60  
CCSM4           34990.050  rcp60  
CCSM4           33879.972  rcp60  
CCSM4           35652.964  rcp60  
CCSM4           34126.102  rcp60  
CCSM4           24757.480  rcp60  
CCSM4           34282.047  rcp60  
CCSM4           33879.972  rcp60  
CCSM4           35199.651  rcp60  
CCSM4           23145.312  rcp60  
CCSM4           34126.102  rcp60  
CCSM4           14040.588  rcp60  
CCSM4           34990.050  rcp60  
CCSM4           27499.287  rcp60  
CCSM4           35525.977  rcp60  
CCSM4           37143.285  rcp60  
CCSM4           34619.029  rcp60  
CCSM4           35525.977  rcp60  
CCSM4           34282.047  rcp60  
CCSM4           35199.651  rcp60  
CCSM4           35235.574  rcp60  
CCSM4           33946.735  rcp60  
CCSM4           35172.857  rcp60  
CCSM4           33879.972  rcp60  
CCSM4           35973.960  rcp60  
CCSM4           10991.216  rcp60  
CCSM4           34990.050  rcp60  
CCSM4           33958.854  rcp60  
CCSM4           33879.972  rcp60  
GFDL-CM3        27044.551  rcp60  
GFDL-CM3        12087.919  rcp60  
GFDL-CM3        26140.396  rcp60  
GFDL-CM3        29473.799  rcp60  
GFDL-CM3        23318.779  rcp60  
GFDL-CM3        27663.164  rcp60  
GFDL-CM3        25112.683  rcp60  
GFDL-CM3        21140.192  rcp60  
GFDL-CM3        26198.609  rcp60  
GFDL-CM3        36272.138  rcp60  
GFDL-CM3        34331.021  rcp60  
GFDL-CM3        24396.538  rcp60  
GFDL-CM3        33440.707  rcp60  
GFDL-CM3        16316.978  rcp60  
GFDL-CM3        26108.917  rcp60  
GFDL-CM3        23055.620  rcp60  
GFDL-CM3        26198.609  rcp60  
GFDL-CM3        28099.470  rcp60  
GFDL-CM3        26198.609  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        20488.036  rcp60  
GFDL-CM3        34126.102  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        24342.413  rcp60  
GFDL-CM3        34684.110  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        26656.949  rcp60  
GFDL-CM3        33946.735  rcp60  
GFDL-CM3        23745.392  rcp60  
GFDL-CM3        23796.201  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        25839.114  rcp60  
GFDL-CM3        26198.609  rcp60  
GFDL-CM3        25365.572  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        28178.838  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        26198.609  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        35906.593  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        26321.217  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        34990.050  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33811.981  rcp60  
GFDL-CM3        33798.455  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        34412.539  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33946.735  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33811.981  rcp60  
GFDL-CM3        34205.018  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33946.735  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GFDL-CM3        33879.972  rcp60  
GISS-E2-R       25395.497  rcp60  
GISS-E2-R       17641.758  rcp60  
GISS-E2-R       24648.456  rcp60  
GISS-E2-R       17923.273  rcp60  
GISS-E2-R       16804.013  rcp60  
GISS-E2-R       27693.848  rcp60  
GISS-E2-R       10884.313  rcp60  
GISS-E2-R       12787.387  rcp60  
GISS-E2-R       25185.284  rcp60  
GISS-E2-R       21972.686  rcp60  
GISS-E2-R       14188.901  rcp60  
GISS-E2-R        9196.747  rcp60  
GISS-E2-R       22674.612  rcp60  
GISS-E2-R       20442.771  rcp60  
GISS-E2-R       17891.591  rcp60  
GISS-E2-R       10599.450  rcp60  
GISS-E2-R       16982.678  rcp60  
GISS-E2-R       36855.716  rcp60  
GISS-E2-R       24434.450  rcp60  
GISS-E2-R       19044.546  rcp60  
GISS-E2-R       33830.573  rcp60  
GISS-E2-R       19692.792  rcp60  
GISS-E2-R       14431.342  rcp60  
GISS-E2-R       13396.819  rcp60  
GISS-E2-R       14285.727  rcp60  
GISS-E2-R       31271.696  rcp60  
GISS-E2-R       12065.659  rcp60  
GISS-E2-R       35132.957  rcp60  
GISS-E2-R       34166.340  rcp60  
GISS-E2-R       30562.666  rcp60  
GISS-E2-R       23559.508  rcp60  
GISS-E2-R       11681.504  rcp60  
GISS-E2-R       31167.574  rcp60  
GISS-E2-R       33637.933  rcp60  
GISS-E2-R       28085.732  rcp60  
GISS-E2-R       17616.814  rcp60  
GISS-E2-R        9209.605  rcp60  
GISS-E2-R       35948.316  rcp60  
GISS-E2-R       12753.626  rcp60  
GISS-E2-R       19965.378  rcp60  
GISS-E2-R       25069.329  rcp60  
GISS-E2-R       23999.239  rcp60  
GISS-E2-R       27877.824  rcp60  
GISS-E2-R       34102.678  rcp60  
GISS-E2-R       33798.455  rcp60  
GISS-E2-R       12738.947  rcp60  
GISS-E2-R       35143.668  rcp60  
GISS-E2-R       25103.707  rcp60  
GISS-E2-R       21056.222  rcp60  
GISS-E2-R       34275.557  rcp60  
GISS-E2-R       26687.062  rcp60  
GISS-E2-R       35906.593  rcp60  
GISS-E2-R       33855.433  rcp60  
GISS-E2-R       21056.222  rcp60  
GISS-E2-R       24456.131  rcp60  
GISS-E2-R       35906.593  rcp60  
GISS-E2-R       24873.521  rcp60  
GISS-E2-R       26196.376  rcp60  
GISS-E2-R       26813.111  rcp60  
GISS-E2-R       35884.318  rcp60  
GISS-E2-R       21531.352  rcp60  
GISS-E2-R       29800.591  rcp60  
GISS-E2-R       21495.293  rcp60  
GISS-E2-R       35172.857  rcp60  
GISS-E2-R       28557.376  rcp60  
GISS-E2-R       33879.972  rcp60  
GISS-E2-R       21140.192  rcp60  
GISS-E2-R       33798.455  rcp60  
GISS-E2-R       21266.900  rcp60  
GISS-E2-R       25958.655  rcp60  
GISS-E2-R       24633.439  rcp60  
GISS-E2-R       34137.027  rcp60  
GISS-E2-R       24836.469  rcp60  
GISS-E2-R       26108.917  rcp60  
GISS-E2-R       22930.368  rcp60  
GISS-E2-R       26082.741  rcp60  
GISS-E2-R       22912.149  rcp60  
GISS-E2-R       21531.352  rcp60  
GISS-E2-R       23875.990  rcp60  
GISS-E2-R       33440.707  rcp60  
GISS-E2-R       20804.422  rcp60  
GISS-E2-R       23923.902  rcp60  
GISS-E2-R       25099.779  rcp60  
GISS-E2-R       27499.287  rcp60  
GISS-E2-R       33818.904  rcp60  
GISS-E2-R       34282.047  rcp60  
GISS-E2-R       19965.185  rcp60  
GISS-E2-R       33719.368  rcp60  
GISS-E2-R       21056.222  rcp60  
GISS-E2-R       20769.708  rcp60  
IPSL-CM5A-LR    17050.318  rcp60  
IPSL-CM5A-LR    17447.386  rcp60  
IPSL-CM5A-LR     9838.555  rcp60  
IPSL-CM5A-LR    33811.981  rcp60  
IPSL-CM5A-LR    37143.285  rcp60  
IPSL-CM5A-LR     9215.099  rcp60  
IPSL-CM5A-LR    24221.158  rcp60  
IPSL-CM5A-LR    28455.621  rcp60  
IPSL-CM5A-LR    34692.362  rcp60  
IPSL-CM5A-LR    22381.063  rcp60  
IPSL-CM5A-LR    33879.972  rcp60  
IPSL-CM5A-LR    24339.940  rcp60  
IPSL-CM5A-LR    35172.857  rcp60  
IPSL-CM5A-LR    25627.984  rcp60  
IPSL-CM5A-LR    23593.355  rcp60  
IPSL-CM5A-LR    12012.844  rcp60  
IPSL-CM5A-LR    23902.662  rcp60  
IPSL-CM5A-LR    25559.226  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    35199.651  rcp60  
IPSL-CM5A-LR    10062.431  rcp60  
IPSL-CM5A-LR    35355.595  rcp60  
IPSL-CM5A-LR    34746.016  rcp60  
IPSL-CM5A-LR    34017.791  rcp60  
IPSL-CM5A-LR    25166.680  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    36777.740  rcp60  
IPSL-CM5A-LR    27693.848  rcp60  
IPSL-CM5A-LR    35143.668  rcp60  
IPSL-CM5A-LR    11846.253  rcp60  
IPSL-CM5A-LR    35172.857  rcp60  
IPSL-CM5A-LR    21888.715  rcp60  
IPSL-CM5A-LR    11945.956  rcp60  
IPSL-CM5A-LR    12734.579  rcp60  
IPSL-CM5A-LR    21795.423  rcp60  
IPSL-CM5A-LR    36777.740  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    35906.593  rcp60  
IPSL-CM5A-LR    31969.086  rcp60  
IPSL-CM5A-LR    34017.791  rcp60  
IPSL-CM5A-LR    33879.972  rcp60  
IPSL-CM5A-LR    36286.393  rcp60  
IPSL-CM5A-LR    33371.841  rcp60  
IPSL-CM5A-LR    25067.560  rcp60  
IPSL-CM5A-LR    11115.756  rcp60  
IPSL-CM5A-LR    21314.812  rcp60  
IPSL-CM5A-LR    36805.208  rcp60  
IPSL-CM5A-LR    18589.369  rcp60  
IPSL-CM5A-LR    36196.274  rcp60  
IPSL-CM5A-LR    36018.509  rcp60  
IPSL-CM5A-LR    23869.230  rcp60  
IPSL-CM5A-LR    36308.667  rcp60  
IPSL-CM5A-LR    34412.539  rcp60  
IPSL-CM5A-LR    33798.455  rcp60  
IPSL-CM5A-LR    35652.087  rcp60  
IPSL-CM5A-LR    24239.176  rcp60  
IPSL-CM5A-LR    27499.287  rcp60  
IPSL-CM5A-LR    21888.715  rcp60  
IPSL-CM5A-LR    32124.203  rcp60  
IPSL-CM5A-LR    23273.539  rcp60  
IPSL-CM5A-LR    35355.595  rcp60  
IPSL-CM5A-LR    35906.593  rcp60  
IPSL-CM5A-LR    34126.102  rcp60  
IPSL-CM5A-LR    33288.060  rcp60  
IPSL-CM5A-LR    33879.972  rcp60  
IPSL-CM5A-LR    35199.651  rcp60  
IPSL-CM5A-LR    28320.825  rcp60  
IPSL-CM5A-LR    33879.972  rcp60  
IPSL-CM5A-LR    33811.981  rcp60  
IPSL-CM5A-LR    35355.595  rcp60  
IPSL-CM5A-LR    23402.675  rcp60  
IPSL-CM5A-LR    34307.434  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    24746.535  rcp60  
IPSL-CM5A-LR    35172.857  rcp60  
IPSL-CM5A-LR    33798.455  rcp60  
IPSL-CM5A-LR    25879.784  rcp60  
IPSL-CM5A-LR    35906.593  rcp60  
IPSL-CM5A-LR    33798.455  rcp60  
IPSL-CM5A-LR    33798.455  rcp60  
IPSL-CM5A-LR    34177.526  rcp60  
IPSL-CM5A-LR    35143.668  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    24720.505  rcp60  
IPSL-CM5A-LR    21230.842  rcp60  
IPSL-CM5A-LR    34166.071  rcp60  
IPSL-CM5A-LR    11345.894  rcp60  
IPSL-CM5A-LR    35172.857  rcp60  
IPSL-CM5A-LR    34990.050  rcp60  
IPSL-CM5A-LR    34017.791  rcp60  
MRI-CGCM3       11762.282  rcp60  
MRI-CGCM3       13157.436  rcp60  
MRI-CGCM3       23804.279  rcp60  
MRI-CGCM3       11510.482  rcp60  
MRI-CGCM3       16055.908  rcp60  
MRI-CGCM3       26074.345  rcp60  
MRI-CGCM3       17331.084  rcp60  
MRI-CGCM3       24152.493  rcp60  
MRI-CGCM3       26944.271  rcp60  
MRI-CGCM3       28387.708  rcp60  
MRI-CGCM3       19270.973  rcp60  
MRI-CGCM3       15310.515  rcp60  
MRI-CGCM3       11199.727  rcp60  
MRI-CGCM3       16109.317  rcp60  
MRI-CGCM3       14296.526  rcp60  
MRI-CGCM3        9600.301  rcp60  
MRI-CGCM3       13701.424  rcp60  
MRI-CGCM3       19120.824  rcp60  
MRI-CGCM3       17021.813  rcp60  
MRI-CGCM3       15873.647  rcp60  
MRI-CGCM3       33340.668  rcp60  
MRI-CGCM3       24260.081  rcp60  
MRI-CGCM3       17615.929  rcp60  
MRI-CGCM3       27667.320  rcp60  
MRI-CGCM3       10322.556  rcp60  
MRI-CGCM3       14619.031  rcp60  
MRI-CGCM3       35551.045  rcp60  
MRI-CGCM3       23410.734  rcp60  
MRI-CGCM3       13049.858  rcp60  
MRI-CGCM3       17830.398  rcp60  
MRI-CGCM3       10076.683  rcp60  
MRI-CGCM3       18571.163  rcp60  
MRI-CGCM3       17616.814  rcp60  
MRI-CGCM3       15730.606  rcp60  
MRI-CGCM3       13587.549  rcp60  
MRI-CGCM3       26630.687  rcp60  
MRI-CGCM3       12265.926  rcp60  
MRI-CGCM3       23698.241  rcp60  
MRI-CGCM3       26340.008  rcp60  
MRI-CGCM3       22638.307  rcp60  
MRI-CGCM3       26728.236  rcp60  
MRI-CGCM3        8952.381  rcp60  
MRI-CGCM3       33879.972  rcp60  
MRI-CGCM3       12494.332  rcp60  
MRI-CGCM3        9684.735  rcp60  
MRI-CGCM3        9464.065  rcp60  
MRI-CGCM3       27283.817  rcp60  
MRI-CGCM3       28041.458  rcp60  
MRI-CGCM3       23921.718  rcp60  
MRI-CGCM3       22402.148  rcp60  
MRI-CGCM3       26198.609  rcp60  
MRI-CGCM3       13173.713  rcp60  
MRI-CGCM3       10664.512  rcp60  
MRI-CGCM3       20515.714  rcp60  
MRI-CGCM3       24579.106  rcp60  
MRI-CGCM3       16042.526  rcp60  
MRI-CGCM3       24171.953  rcp60  
MRI-CGCM3       25559.226  rcp60  
MRI-CGCM3       16642.352  rcp60  
MRI-CGCM3       27012.415  rcp60  
MRI-CGCM3       25056.964  rcp60  
MRI-CGCM3       26799.366  rcp60  
MRI-CGCM3       35444.211  rcp60  
MRI-CGCM3       35143.668  rcp60  
MRI-CGCM3       33879.972  rcp60  
MRI-CGCM3       33453.359  rcp60  
MRI-CGCM3       10325.652  rcp60  
MRI-CGCM3       35884.318  rcp60  
MRI-CGCM3       24482.404  rcp60  
MRI-CGCM3       23263.733  rcp60  
MRI-CGCM3       19965.378  rcp60  
MRI-CGCM3       26010.542  rcp60  
MRI-CGCM3       20404.450  rcp60  
MRI-CGCM3       24957.644  rcp60  
MRI-CGCM3       23745.392  rcp60  
MRI-CGCM3       21428.067  rcp60  
MRI-CGCM3       24579.106  rcp60  
MRI-CGCM3       15076.026  rcp60  
MRI-CGCM3       23923.902  rcp60  
MRI-CGCM3       20492.838  rcp60  
MRI-CGCM3       34123.501  rcp60  
MRI-CGCM3       34044.585  rcp60  
MRI-CGCM3       35906.593  rcp60  
MRI-CGCM3       33217.237  rcp60  
MRI-CGCM3       12767.298  rcp60  
MRI-CGCM3       33879.972  rcp60  
MRI-CGCM3       20440.508  rcp60  
MRI-CGCM3       30079.093  rcp60  
MRI-CGCM3       25367.967  rcp60  
MRI-CGCM3       20162.411  rcp60  

```r
d2[, `:=`(Year, 2010:2099)]
```



variable            value  Period    Year
-------------  ----------  -------  -----
CCSM4           25962.751  rcp60     2010
CCSM4           11762.282  rcp60     2011
CCSM4           30404.038  rcp60     2012
CCSM4           35891.522  rcp60     2013
CCSM4           23657.608  rcp60     2014
CCSM4            9942.099  rcp60     2015
CCSM4           25051.641  rcp60     2016
CCSM4           25962.751  rcp60     2017
CCSM4           10022.518  rcp60     2018
CCSM4           25962.751  rcp60     2019
CCSM4           35973.960  rcp60     2020
CCSM4           24979.394  rcp60     2021
CCSM4           27518.329  rcp60     2022
CCSM4           27073.715  rcp60     2023
CCSM4           14227.483  rcp60     2024
CCSM4           15012.245  rcp60     2025
CCSM4           14052.403  rcp60     2026
CCSM4           25820.306  rcp60     2027
CCSM4           27693.848  rcp60     2028
CCSM4           12559.473  rcp60     2029
CCSM4           35355.595  rcp60     2030
CCSM4           36777.740  rcp60     2031
CCSM4           36385.663  rcp60     2032
CCSM4           25962.751  rcp60     2033
CCSM4           15362.057  rcp60     2034
CCSM4           35172.857  rcp60     2035
CCSM4           35967.455  rcp60     2036
CCSM4            9675.916  rcp60     2037
CCSM4           23902.194  rcp60     2038
CCSM4           24503.627  rcp60     2039
CCSM4           12265.926  rcp60     2040
CCSM4           26376.983  rcp60     2041
CCSM4           27918.833  rcp60     2042
CCSM4           35234.698  rcp60     2043
CCSM4           11537.714  rcp60     2044
CCSM4           33775.752  rcp60     2045
CCSM4           28519.264  rcp60     2046
CCSM4           33034.586  rcp60     2047
CCSM4           34003.478  rcp60     2048
CCSM4           24489.414  rcp60     2049
CCSM4           24849.554  rcp60     2050
CCSM4           24167.996  rcp60     2051
CCSM4           25879.784  rcp60     2052
CCSM4           20303.698  rcp60     2053
CCSM4           23655.700  rcp60     2054
CCSM4           17347.002  rcp60     2055
CCSM4           21579.264  rcp60     2056
CCSM4           34990.050  rcp60     2057
CCSM4           27499.287  rcp60     2058
CCSM4           34990.050  rcp60     2059
CCSM4           20303.698  rcp60     2060
CCSM4           22879.978  rcp60     2061
CCSM4           25962.751  rcp60     2062
CCSM4           34811.096  rcp60     2063
CCSM4           25395.497  rcp60     2064
CCSM4           34126.102  rcp60     2065
CCSM4           20371.259  rcp60     2066
CCSM4           25103.707  rcp60     2067
CCSM4           35355.595  rcp60     2068
CCSM4           26048.189  rcp60     2069
CCSM4           33634.672  rcp60     2070
CCSM4           35355.595  rcp60     2071
CCSM4           34990.050  rcp60     2072
CCSM4           33879.972  rcp60     2073
CCSM4           35652.964  rcp60     2074
CCSM4           34126.102  rcp60     2075
CCSM4           24757.480  rcp60     2076
CCSM4           34282.047  rcp60     2077
CCSM4           33879.972  rcp60     2078
CCSM4           35199.651  rcp60     2079
CCSM4           23145.312  rcp60     2080
CCSM4           34126.102  rcp60     2081
CCSM4           14040.588  rcp60     2082
CCSM4           34990.050  rcp60     2083
CCSM4           27499.287  rcp60     2084
CCSM4           35525.977  rcp60     2085
CCSM4           37143.285  rcp60     2086
CCSM4           34619.029  rcp60     2087
CCSM4           35525.977  rcp60     2088
CCSM4           34282.047  rcp60     2089
CCSM4           35199.651  rcp60     2090
CCSM4           35235.574  rcp60     2091
CCSM4           33946.735  rcp60     2092
CCSM4           35172.857  rcp60     2093
CCSM4           33879.972  rcp60     2094
CCSM4           35973.960  rcp60     2095
CCSM4           10991.216  rcp60     2096
CCSM4           34990.050  rcp60     2097
CCSM4           33958.854  rcp60     2098
CCSM4           33879.972  rcp60     2099
GFDL-CM3        27044.551  rcp60     2010
GFDL-CM3        12087.919  rcp60     2011
GFDL-CM3        26140.396  rcp60     2012
GFDL-CM3        29473.799  rcp60     2013
GFDL-CM3        23318.779  rcp60     2014
GFDL-CM3        27663.164  rcp60     2015
GFDL-CM3        25112.683  rcp60     2016
GFDL-CM3        21140.192  rcp60     2017
GFDL-CM3        26198.609  rcp60     2018
GFDL-CM3        36272.138  rcp60     2019
GFDL-CM3        34331.021  rcp60     2020
GFDL-CM3        24396.538  rcp60     2021
GFDL-CM3        33440.707  rcp60     2022
GFDL-CM3        16316.978  rcp60     2023
GFDL-CM3        26108.917  rcp60     2024
GFDL-CM3        23055.620  rcp60     2025
GFDL-CM3        26198.609  rcp60     2026
GFDL-CM3        28099.470  rcp60     2027
GFDL-CM3        26198.609  rcp60     2028
GFDL-CM3        34990.050  rcp60     2029
GFDL-CM3        20488.036  rcp60     2030
GFDL-CM3        34126.102  rcp60     2031
GFDL-CM3        33798.455  rcp60     2032
GFDL-CM3        24342.413  rcp60     2033
GFDL-CM3        34684.110  rcp60     2034
GFDL-CM3        33879.972  rcp60     2035
GFDL-CM3        26656.949  rcp60     2036
GFDL-CM3        33946.735  rcp60     2037
GFDL-CM3        23745.392  rcp60     2038
GFDL-CM3        23796.201  rcp60     2039
GFDL-CM3        34990.050  rcp60     2040
GFDL-CM3        25839.114  rcp60     2041
GFDL-CM3        26198.609  rcp60     2042
GFDL-CM3        25365.572  rcp60     2043
GFDL-CM3        34990.050  rcp60     2044
GFDL-CM3        33879.972  rcp60     2045
GFDL-CM3        33879.972  rcp60     2046
GFDL-CM3        33879.972  rcp60     2047
GFDL-CM3        33798.455  rcp60     2048
GFDL-CM3        33879.972  rcp60     2049
GFDL-CM3        28178.838  rcp60     2050
GFDL-CM3        34990.050  rcp60     2051
GFDL-CM3        33879.972  rcp60     2052
GFDL-CM3        33879.972  rcp60     2053
GFDL-CM3        26198.609  rcp60     2054
GFDL-CM3        33798.455  rcp60     2055
GFDL-CM3        33879.972  rcp60     2056
GFDL-CM3        34990.050  rcp60     2057
GFDL-CM3        33879.972  rcp60     2058
GFDL-CM3        35906.593  rcp60     2059
GFDL-CM3        33879.972  rcp60     2060
GFDL-CM3        26321.217  rcp60     2061
GFDL-CM3        33879.972  rcp60     2062
GFDL-CM3        33879.972  rcp60     2063
GFDL-CM3        33879.972  rcp60     2064
GFDL-CM3        33879.972  rcp60     2065
GFDL-CM3        33879.972  rcp60     2066
GFDL-CM3        33879.972  rcp60     2067
GFDL-CM3        33879.972  rcp60     2068
GFDL-CM3        34990.050  rcp60     2069
GFDL-CM3        33879.972  rcp60     2070
GFDL-CM3        33879.972  rcp60     2071
GFDL-CM3        33879.972  rcp60     2072
GFDL-CM3        33798.455  rcp60     2073
GFDL-CM3        33879.972  rcp60     2074
GFDL-CM3        33798.455  rcp60     2075
GFDL-CM3        33879.972  rcp60     2076
GFDL-CM3        33879.972  rcp60     2077
GFDL-CM3        33879.972  rcp60     2078
GFDL-CM3        33879.972  rcp60     2079
GFDL-CM3        33798.455  rcp60     2080
GFDL-CM3        33811.981  rcp60     2081
GFDL-CM3        33798.455  rcp60     2082
GFDL-CM3        33879.972  rcp60     2083
GFDL-CM3        34412.539  rcp60     2084
GFDL-CM3        33879.972  rcp60     2085
GFDL-CM3        33946.735  rcp60     2086
GFDL-CM3        33879.972  rcp60     2087
GFDL-CM3        33879.972  rcp60     2088
GFDL-CM3        33879.972  rcp60     2089
GFDL-CM3        33811.981  rcp60     2090
GFDL-CM3        34205.018  rcp60     2091
GFDL-CM3        33879.972  rcp60     2092
GFDL-CM3        33946.735  rcp60     2093
GFDL-CM3        33879.972  rcp60     2094
GFDL-CM3        33879.972  rcp60     2095
GFDL-CM3        33879.972  rcp60     2096
GFDL-CM3        33879.972  rcp60     2097
GFDL-CM3        33879.972  rcp60     2098
GFDL-CM3        33879.972  rcp60     2099
GISS-E2-R       25395.497  rcp60     2010
GISS-E2-R       17641.758  rcp60     2011
GISS-E2-R       24648.456  rcp60     2012
GISS-E2-R       17923.273  rcp60     2013
GISS-E2-R       16804.013  rcp60     2014
GISS-E2-R       27693.848  rcp60     2015
GISS-E2-R       10884.313  rcp60     2016
GISS-E2-R       12787.387  rcp60     2017
GISS-E2-R       25185.284  rcp60     2018
GISS-E2-R       21972.686  rcp60     2019
GISS-E2-R       14188.901  rcp60     2020
GISS-E2-R        9196.747  rcp60     2021
GISS-E2-R       22674.612  rcp60     2022
GISS-E2-R       20442.771  rcp60     2023
GISS-E2-R       17891.591  rcp60     2024
GISS-E2-R       10599.450  rcp60     2025
GISS-E2-R       16982.678  rcp60     2026
GISS-E2-R       36855.716  rcp60     2027
GISS-E2-R       24434.450  rcp60     2028
GISS-E2-R       19044.546  rcp60     2029
GISS-E2-R       33830.573  rcp60     2030
GISS-E2-R       19692.792  rcp60     2031
GISS-E2-R       14431.342  rcp60     2032
GISS-E2-R       13396.819  rcp60     2033
GISS-E2-R       14285.727  rcp60     2034
GISS-E2-R       31271.696  rcp60     2035
GISS-E2-R       12065.659  rcp60     2036
GISS-E2-R       35132.957  rcp60     2037
GISS-E2-R       34166.340  rcp60     2038
GISS-E2-R       30562.666  rcp60     2039
GISS-E2-R       23559.508  rcp60     2040
GISS-E2-R       11681.504  rcp60     2041
GISS-E2-R       31167.574  rcp60     2042
GISS-E2-R       33637.933  rcp60     2043
GISS-E2-R       28085.732  rcp60     2044
GISS-E2-R       17616.814  rcp60     2045
GISS-E2-R        9209.605  rcp60     2046
GISS-E2-R       35948.316  rcp60     2047
GISS-E2-R       12753.626  rcp60     2048
GISS-E2-R       19965.378  rcp60     2049
GISS-E2-R       25069.329  rcp60     2050
GISS-E2-R       23999.239  rcp60     2051
GISS-E2-R       27877.824  rcp60     2052
GISS-E2-R       34102.678  rcp60     2053
GISS-E2-R       33798.455  rcp60     2054
GISS-E2-R       12738.947  rcp60     2055
GISS-E2-R       35143.668  rcp60     2056
GISS-E2-R       25103.707  rcp60     2057
GISS-E2-R       21056.222  rcp60     2058
GISS-E2-R       34275.557  rcp60     2059
GISS-E2-R       26687.062  rcp60     2060
GISS-E2-R       35906.593  rcp60     2061
GISS-E2-R       33855.433  rcp60     2062
GISS-E2-R       21056.222  rcp60     2063
GISS-E2-R       24456.131  rcp60     2064
GISS-E2-R       35906.593  rcp60     2065
GISS-E2-R       24873.521  rcp60     2066
GISS-E2-R       26196.376  rcp60     2067
GISS-E2-R       26813.111  rcp60     2068
GISS-E2-R       35884.318  rcp60     2069
GISS-E2-R       21531.352  rcp60     2070
GISS-E2-R       29800.591  rcp60     2071
GISS-E2-R       21495.293  rcp60     2072
GISS-E2-R       35172.857  rcp60     2073
GISS-E2-R       28557.376  rcp60     2074
GISS-E2-R       33879.972  rcp60     2075
GISS-E2-R       21140.192  rcp60     2076
GISS-E2-R       33798.455  rcp60     2077
GISS-E2-R       21266.900  rcp60     2078
GISS-E2-R       25958.655  rcp60     2079
GISS-E2-R       24633.439  rcp60     2080
GISS-E2-R       34137.027  rcp60     2081
GISS-E2-R       24836.469  rcp60     2082
GISS-E2-R       26108.917  rcp60     2083
GISS-E2-R       22930.368  rcp60     2084
GISS-E2-R       26082.741  rcp60     2085
GISS-E2-R       22912.149  rcp60     2086
GISS-E2-R       21531.352  rcp60     2087
GISS-E2-R       23875.990  rcp60     2088
GISS-E2-R       33440.707  rcp60     2089
GISS-E2-R       20804.422  rcp60     2090
GISS-E2-R       23923.902  rcp60     2091
GISS-E2-R       25099.779  rcp60     2092
GISS-E2-R       27499.287  rcp60     2093
GISS-E2-R       33818.904  rcp60     2094
GISS-E2-R       34282.047  rcp60     2095
GISS-E2-R       19965.185  rcp60     2096
GISS-E2-R       33719.368  rcp60     2097
GISS-E2-R       21056.222  rcp60     2098
GISS-E2-R       20769.708  rcp60     2099
IPSL-CM5A-LR    17050.318  rcp60     2010
IPSL-CM5A-LR    17447.386  rcp60     2011
IPSL-CM5A-LR     9838.555  rcp60     2012
IPSL-CM5A-LR    33811.981  rcp60     2013
IPSL-CM5A-LR    37143.285  rcp60     2014
IPSL-CM5A-LR     9215.099  rcp60     2015
IPSL-CM5A-LR    24221.158  rcp60     2016
IPSL-CM5A-LR    28455.621  rcp60     2017
IPSL-CM5A-LR    34692.362  rcp60     2018
IPSL-CM5A-LR    22381.063  rcp60     2019
IPSL-CM5A-LR    33879.972  rcp60     2020
IPSL-CM5A-LR    24339.940  rcp60     2021
IPSL-CM5A-LR    35172.857  rcp60     2022
IPSL-CM5A-LR    25627.984  rcp60     2023
IPSL-CM5A-LR    23593.355  rcp60     2024
IPSL-CM5A-LR    12012.844  rcp60     2025
IPSL-CM5A-LR    23902.662  rcp60     2026
IPSL-CM5A-LR    25559.226  rcp60     2027
IPSL-CM5A-LR    34990.050  rcp60     2028
IPSL-CM5A-LR    35199.651  rcp60     2029
IPSL-CM5A-LR    10062.431  rcp60     2030
IPSL-CM5A-LR    35355.595  rcp60     2031
IPSL-CM5A-LR    34746.016  rcp60     2032
IPSL-CM5A-LR    34017.791  rcp60     2033
IPSL-CM5A-LR    25166.680  rcp60     2034
IPSL-CM5A-LR    34990.050  rcp60     2035
IPSL-CM5A-LR    36777.740  rcp60     2036
IPSL-CM5A-LR    27693.848  rcp60     2037
IPSL-CM5A-LR    35143.668  rcp60     2038
IPSL-CM5A-LR    11846.253  rcp60     2039
IPSL-CM5A-LR    35172.857  rcp60     2040
IPSL-CM5A-LR    21888.715  rcp60     2041
IPSL-CM5A-LR    11945.956  rcp60     2042
IPSL-CM5A-LR    12734.579  rcp60     2043
IPSL-CM5A-LR    21795.423  rcp60     2044
IPSL-CM5A-LR    36777.740  rcp60     2045
IPSL-CM5A-LR    34990.050  rcp60     2046
IPSL-CM5A-LR    35906.593  rcp60     2047
IPSL-CM5A-LR    31969.086  rcp60     2048
IPSL-CM5A-LR    34017.791  rcp60     2049
IPSL-CM5A-LR    33879.972  rcp60     2050
IPSL-CM5A-LR    36286.393  rcp60     2051
IPSL-CM5A-LR    33371.841  rcp60     2052
IPSL-CM5A-LR    25067.560  rcp60     2053
IPSL-CM5A-LR    11115.756  rcp60     2054
IPSL-CM5A-LR    21314.812  rcp60     2055
IPSL-CM5A-LR    36805.208  rcp60     2056
IPSL-CM5A-LR    18589.369  rcp60     2057
IPSL-CM5A-LR    36196.274  rcp60     2058
IPSL-CM5A-LR    36018.509  rcp60     2059
IPSL-CM5A-LR    23869.230  rcp60     2060
IPSL-CM5A-LR    36308.667  rcp60     2061
IPSL-CM5A-LR    34412.539  rcp60     2062
IPSL-CM5A-LR    33798.455  rcp60     2063
IPSL-CM5A-LR    35652.087  rcp60     2064
IPSL-CM5A-LR    24239.176  rcp60     2065
IPSL-CM5A-LR    27499.287  rcp60     2066
IPSL-CM5A-LR    21888.715  rcp60     2067
IPSL-CM5A-LR    32124.203  rcp60     2068
IPSL-CM5A-LR    23273.539  rcp60     2069
IPSL-CM5A-LR    35355.595  rcp60     2070
IPSL-CM5A-LR    35906.593  rcp60     2071
IPSL-CM5A-LR    34126.102  rcp60     2072
IPSL-CM5A-LR    33288.060  rcp60     2073
IPSL-CM5A-LR    33879.972  rcp60     2074
IPSL-CM5A-LR    35199.651  rcp60     2075
IPSL-CM5A-LR    28320.825  rcp60     2076
IPSL-CM5A-LR    33879.972  rcp60     2077
IPSL-CM5A-LR    33811.981  rcp60     2078
IPSL-CM5A-LR    35355.595  rcp60     2079
IPSL-CM5A-LR    23402.675  rcp60     2080
IPSL-CM5A-LR    34307.434  rcp60     2081
IPSL-CM5A-LR    34990.050  rcp60     2082
IPSL-CM5A-LR    24746.535  rcp60     2083
IPSL-CM5A-LR    35172.857  rcp60     2084
IPSL-CM5A-LR    33798.455  rcp60     2085
IPSL-CM5A-LR    25879.784  rcp60     2086
IPSL-CM5A-LR    35906.593  rcp60     2087
IPSL-CM5A-LR    33798.455  rcp60     2088
IPSL-CM5A-LR    33798.455  rcp60     2089
IPSL-CM5A-LR    34177.526  rcp60     2090
IPSL-CM5A-LR    35143.668  rcp60     2091
IPSL-CM5A-LR    34990.050  rcp60     2092
IPSL-CM5A-LR    24720.505  rcp60     2093
IPSL-CM5A-LR    21230.842  rcp60     2094
IPSL-CM5A-LR    34166.071  rcp60     2095
IPSL-CM5A-LR    11345.894  rcp60     2096
IPSL-CM5A-LR    35172.857  rcp60     2097
IPSL-CM5A-LR    34990.050  rcp60     2098
IPSL-CM5A-LR    34017.791  rcp60     2099
MRI-CGCM3       11762.282  rcp60     2010
MRI-CGCM3       13157.436  rcp60     2011
MRI-CGCM3       23804.279  rcp60     2012
MRI-CGCM3       11510.482  rcp60     2013
MRI-CGCM3       16055.908  rcp60     2014
MRI-CGCM3       26074.345  rcp60     2015
MRI-CGCM3       17331.084  rcp60     2016
MRI-CGCM3       24152.493  rcp60     2017
MRI-CGCM3       26944.271  rcp60     2018
MRI-CGCM3       28387.708  rcp60     2019
MRI-CGCM3       19270.973  rcp60     2020
MRI-CGCM3       15310.515  rcp60     2021
MRI-CGCM3       11199.727  rcp60     2022
MRI-CGCM3       16109.317  rcp60     2023
MRI-CGCM3       14296.526  rcp60     2024
MRI-CGCM3        9600.301  rcp60     2025
MRI-CGCM3       13701.424  rcp60     2026
MRI-CGCM3       19120.824  rcp60     2027
MRI-CGCM3       17021.813  rcp60     2028
MRI-CGCM3       15873.647  rcp60     2029
MRI-CGCM3       33340.668  rcp60     2030
MRI-CGCM3       24260.081  rcp60     2031
MRI-CGCM3       17615.929  rcp60     2032
MRI-CGCM3       27667.320  rcp60     2033
MRI-CGCM3       10322.556  rcp60     2034
MRI-CGCM3       14619.031  rcp60     2035
MRI-CGCM3       35551.045  rcp60     2036
MRI-CGCM3       23410.734  rcp60     2037
MRI-CGCM3       13049.858  rcp60     2038
MRI-CGCM3       17830.398  rcp60     2039
MRI-CGCM3       10076.683  rcp60     2040
MRI-CGCM3       18571.163  rcp60     2041
MRI-CGCM3       17616.814  rcp60     2042
MRI-CGCM3       15730.606  rcp60     2043
MRI-CGCM3       13587.549  rcp60     2044
MRI-CGCM3       26630.687  rcp60     2045
MRI-CGCM3       12265.926  rcp60     2046
MRI-CGCM3       23698.241  rcp60     2047
MRI-CGCM3       26340.008  rcp60     2048
MRI-CGCM3       22638.307  rcp60     2049
MRI-CGCM3       26728.236  rcp60     2050
MRI-CGCM3        8952.381  rcp60     2051
MRI-CGCM3       33879.972  rcp60     2052
MRI-CGCM3       12494.332  rcp60     2053
MRI-CGCM3        9684.735  rcp60     2054
MRI-CGCM3        9464.065  rcp60     2055
MRI-CGCM3       27283.817  rcp60     2056
MRI-CGCM3       28041.458  rcp60     2057
MRI-CGCM3       23921.718  rcp60     2058
MRI-CGCM3       22402.148  rcp60     2059
MRI-CGCM3       26198.609  rcp60     2060
MRI-CGCM3       13173.713  rcp60     2061
MRI-CGCM3       10664.512  rcp60     2062
MRI-CGCM3       20515.714  rcp60     2063
MRI-CGCM3       24579.106  rcp60     2064
MRI-CGCM3       16042.526  rcp60     2065
MRI-CGCM3       24171.953  rcp60     2066
MRI-CGCM3       25559.226  rcp60     2067
MRI-CGCM3       16642.352  rcp60     2068
MRI-CGCM3       27012.415  rcp60     2069
MRI-CGCM3       25056.964  rcp60     2070
MRI-CGCM3       26799.366  rcp60     2071
MRI-CGCM3       35444.211  rcp60     2072
MRI-CGCM3       35143.668  rcp60     2073
MRI-CGCM3       33879.972  rcp60     2074
MRI-CGCM3       33453.359  rcp60     2075
MRI-CGCM3       10325.652  rcp60     2076
MRI-CGCM3       35884.318  rcp60     2077
MRI-CGCM3       24482.404  rcp60     2078
MRI-CGCM3       23263.733  rcp60     2079
MRI-CGCM3       19965.378  rcp60     2080
MRI-CGCM3       26010.542  rcp60     2081
MRI-CGCM3       20404.450  rcp60     2082
MRI-CGCM3       24957.644  rcp60     2083
MRI-CGCM3       23745.392  rcp60     2084
MRI-CGCM3       21428.067  rcp60     2085
MRI-CGCM3       24579.106  rcp60     2086
MRI-CGCM3       15076.026  rcp60     2087
MRI-CGCM3       23923.902  rcp60     2088
MRI-CGCM3       20492.838  rcp60     2089
MRI-CGCM3       34123.501  rcp60     2090
MRI-CGCM3       34044.585  rcp60     2091
MRI-CGCM3       35906.593  rcp60     2092
MRI-CGCM3       33217.237  rcp60     2093
MRI-CGCM3       12767.298  rcp60     2094
MRI-CGCM3       33879.972  rcp60     2095
MRI-CGCM3       20440.508  rcp60     2096
MRI-CGCM3       30079.093  rcp60     2097
MRI-CGCM3       25367.967  rcp60     2098
MRI-CGCM3       20162.411  rcp60     2099

```r
setnames(d2, c("Model", "LightPred", "Period", "Year"))
d2 <- d2 %>% group_by(Model) %>% mutate(Rank = rank(LightPred), Class = get_classes1(LightPred), 
    Coef = get_coefficients(Class), ECDF = ecdf(LightPred)(LightPred)) %>% mutate(Class = get_classes2(Class)) %>% 
    setcolorder(names(d))

d.all <- rbind(d, d2)

(g4 <- ggplot(data = d.all, aes(x = Rank, y = LightPred, label = Year)) + geom_hline(yintercept = qtiles, 
    linetype = 2) + geom_point() + geom_text(aes(colour = Class), hjust = 0, 
    vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1, y = qtiles, 
    label = c(paste("CRU32\nqunatile =", lb), paste("CRU32\nquantile =", ub)), 
    size = 3, vjust = -0.5) + labs(x = "Predicted rank", y = "Predicted number of strikes", 
    title = "1950-2011 CRU32 and 2010-2099 GCM GBM-predicted summer lightning strikes: ranked and ordered") + 
    facet_wrap(~Model, ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-1.png) 

```r
(g5 <- ggplot(data = d.all, aes(x = Year, y = LightPred, label = Rank)) + geom_line() + 
    geom_hline(yintercept = qtiles, linetype = 2) + geom_point() + geom_text(aes(colour = Class), 
    hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", x = 1950, 
    y = qtiles, label = c(paste("CRU32\nqunatile =", lb), paste("CRU32\nquantile =", 
        ub)), size = 3, vjust = -0.5) + labs(x = "Year", y = "Predicted number of strikes", 
    title = "1950-2011 CRU32 and 2010-2099 GCM GBM-predicted summer lightning strikes: time series") + 
    facet_wrap(~Model, ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-2.png) 

```r
(g6 <- ggplot(data = d.all, aes(x = LightPred, label = Year)) + geom_vline(xintercept = qtiles, 
    linetype = 2) + stat_ecdf() + stat_ecdf(geom = "point") + geom_text(aes(y = ECDF, 
    colour = Class), hjust = 0, vjust = 0, size = 3, show_guide = F) + annotate("text", 
    x = qtiles, y = 0, label = c(paste("CRU32\nqunatile =", lb), paste("CRU32\nquantile =", 
        ub)), size = 3, hjust = -0.1, vjust = -0.25) + labs(x = "Predicted number of strikes", 
    y = "CDF", title = "1950-2011 CRU32 and 2010-2099 GCM GBM-predicted summer lightning strikes: empirical CDF") + 
    facet_wrap(~Model, ncol = 2))
```

![](gbm_lightning_coefficients_files/figure-html/plots_gcm-3.png) 

### Save outputs


