
##
##
## Climate data prep

SNAP's 2-km PRISM downscaled CRU 3.2 historical climate data is resampled to the ALFRESCO 1-km resolution and "statewide" extent.
The "statewide" ALFRESCO vegetation cover map is used to extract climate data conditional on underlying vegetation class.

Vegetation classes are forest, alpine tundra, shrub tundra, graminoid tundra, and wetland tundra.
The forest classification is a union of black spruce, white spruce, and deciduous classes in the vegetation cover map.
Additionally, a single tundra class pertaining to the "CAVM" region of the map is a union of shrub, graminoid, and wetland tundra.
These three tundra classes occur only in the CAVM region of the map, whereas alpine tundra and forest occur exclusive outside the CAVM region.

Temperature and precipitation data are extracted as means and samples (n=100 and n=1,000) for each vegetation class.
For example, all pixels in the vegetation map which are classified as forest are used to compute a mean value for temperature and precipitation associated with that class.
Means are calculated for each month of the year, for the period 1950 - 2013.
Temperature and precipitation samples are also compiled for each month over the same period.

Simple random samples of sizes 100 and 1,000 are taken for both climate variables.
The sampling frame from which random draws are made is the set of map pixels representing the intersection of a specific vegetation class and existing temperature and precipitation data.
A random seed is set only at the beginning of the sampling procedure, which proceeds by year.
This ensures that pixels sampled are consistent across twelve consecutive months, but are permitted to vary each year.
This allows seasonal variables to be computed from monthly variables while reducing the possibility of spatially biased samples. 

The following **R** scripts shown in subsequent sections are used to prepare climate data for the Flammability project.

*    `clim_resample_2km_1km.R`
*    `clim_1km_clip2ak.R`
*    `tpByVeg_CRU32.R`
*    `tpByVeg_CMIP5.R`
