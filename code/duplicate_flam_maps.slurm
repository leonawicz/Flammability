#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=1
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=FlamBackcast
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU31\', model=\'GFDL-CM3\', etc., required;
#### $3 mapset, passed as mapset=\'3models_tif\', etc., required;
#### $4 out, passed as out=\'3models_loop1950to2009_tif_scaled', etc., required, mapset and out may be the same;
#### $5 yearloop, passed as yearloop=1901:1949, etc., required;
#### $6 n, passed as n=40, etc., optional, defaults to 40, see code for details on use;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/duplicate_flam_maps.R $1 $2 $3 $4 $5 $6
