






##
##
## Exploratory plots of marginal and conditional flammability distributions.

Each plot is broken out by vegetation class within the CAVM tundra region as well as each of three component classes, shrub, graminoid, and wetland tundra.

### Marginal distribution

The first plot shows a density function proportional to joint probability distribution for flammability in time and space,
marginalized with repsect to time. The time period is 1950 - 2013. The second plot is the same, but truncated to a maximum of 0.12 flammability.

![](cavm_flam_dist_files/figure-html/plot01a-1.png) 

![](cavm_flam_dist_files/figure-html/plot01b-1.png) 

### Conditional distribution

This plot shows multiple overlapping spatial flammability distributions conditioned on each year, also truncated at 0.12.

![](cavm_flam_dist_files/figure-html/plot02-1.png) 

The table below provides distribution quantiles by vegetation class.


Table: Critical values associated with flammability by vegetation class

Vegetation     Pct05    Pct10    Pct25    Pct50    Pct75    Pct90    Pct95
-----------  -------  -------  -------  -------  -------  -------  -------
CAVM          0.0159   0.0180   0.0201   0.0241   0.0295   0.0449   0.0638
shrub         0.0069   0.0078   0.0094   0.0115   0.0155   0.0216   0.0239
graminoid     0.0064   0.0069   0.0084   0.0116   0.0179   0.0310   0.0364
wetland       0.0055   0.0059   0.0067   0.0082   0.0096   0.0164   0.0196
