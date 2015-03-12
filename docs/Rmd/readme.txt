#######################################################################################################################
#### This readme describes the methods used to create a final vegetation classification map and the use of this    ####
#### map to obtain spatial means of monthly temperature and precipitation for specific vegetation classifications. ####
#######################################################################################################################

#### Author: 		Matthew Leonawicz ####
#### Last updated:  03/01/2012        ####

#### Creating a new vegetation classification map layer ####

#### Source data:
(1) Original CAVM layer
(2) Alfresco layer

These two maps have different classification codes.
CAVM ranges from 0 to 21, not all levels occurring in the map.
CAVM+Alfresco ranges from 0 to 4. A given code in one map does not correspond to the same vegetation class in the other map.
Therefore, integer codes in the CAVM are reclassed such that shrub, graminoid, and wetland correspond to classes 5, 6, and 7, respectively in both maps.
In the CAVM, all remaining vegetation classes are reclassed to NA.

Using the R programming language and the R "raster" package, the two maps are combined using the merge() command.
This is done in such a way that
(1) the CAVM layer has priority over the CAVM+Alfresco layer for non-NA cells,
so that the resulting map takes on the shrub, graminoid, and wetland values (5, 6 and 7) from the CAVM,
(2) but does not retain priority for NA cells,
so that the resulting map then falls back on the Alfresco layer for non-NA data.

This yields a map layer that is essentially the Alfresco layer over most of the spatial domain,
replaced with the values in CAVM layer only in cells defined by the CAVM layer to be shrub, graminoid, or wetland.

Since the CAVM layer does not account for inconsistency in fuel availability across the landscape,
after these layers are combined, all cells with class "0" in the Alfresco layer, are reset to zero,
since the initial merge with CAVM undesireably replaces some 0-cells with 5,6 or 7.

Final Classification codes match that of the CAVN+Alfresco layer:
1 = Current TU
2 = Black Spruce
3 = White Spruce
4 = Deciduous
5 = Shrub Tundra
6 = Grammanoid Tundra
7 = Wetland Tundra


#### Obtaining spatial means of monthly mean temperature and monthly total precipitation for specific vegetation classes ####

#### Source data:
(1) Historical CRU TS 3.1, temperature and precipitation, Jan 1901 - Dec 2009
(2) Future GCM, a1b scenario, temperature and precipitation, Jan 2001 - Dec 2100,
for the following five models:
cccma_cgcm3_1
gfdl_cm2_1
miroc3_2_medres
mpi_echam5
ukmo_hadcm3

To obtain spatial means of temperature and precipitation among the spatial cells corresponding to a given vegetation class,
the above datasets are resampled using nearest neighbor from 800m PRISM-adjusted CRU and GCM to 1km-resolution to match the vegetation map created above.

Once the vegetation map and climate datasets match cell to cell (matching origin, cell center, resolution, extent, etc),
temperature and precipitation values are extracted from all cells corresponding to a specific vegetation class (shrub, graminoid, wetland, etc.)
The mean of this vector of values is calculated for each month and year in a specified period and written to a .txt file.
An additional table is produced for "CAVM" which provides temperature and precipitation means for the total CAVM region consisting of shrub, graminoid, or wetland cells.
