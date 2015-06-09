






##
##
## Exploratory plots of marginal and conditional flammability distributions.

Each plot is broken out by vegetation class within the CAVM tundra region as well as each of three component classes, shrub, graminoid, and wetland tundra.

### Marginal distribution

The first plot shows a density function proportional to joint probability distribution for flammability in time and space,
marginalized with repsect to time. The time period is 1950 - 2009. The second plot is the same, but truncated to a maximum of 0.03 flammability.

![](cavm_flam_dist_files/figure-html/plot01a-1.png) 

![](cavm_flam_dist_files/figure-html/plot01b-1.png) 

### Conditional distribution

This plot shows multiple overlapping spatial flammability distributions conditioned on each year, also truncated at 0.03.

![](cavm_flam_dist_files/figure-html/plot02-1.png) 

The table below provides distribution quantiles by vegetation class.


Table: Critical values associated with flammability by vegetation class

Vegetation     Pct05    Pct10    Pct25    Pct50    Pct75    Pct90    Pct95
-----------  -------  -------  -------  -------  -------  -------  -------
CAVM          0.0026   0.0031   0.0043   0.0062   0.0091   0.0164   0.0289
shrub         0.0051   0.0054   0.0060   0.0071   0.0090   0.0126   0.0188
graminoid     0.0082   0.0083   0.0089   0.0101   0.0136   0.0209   0.0320
wetland       0.0040   0.0042   0.0047   0.0055   0.0068   0.0087   0.0108
