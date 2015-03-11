#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU31\', model=\'GFDL-CM3\', etc., required;
#### $3 recommended optional years as years=1901:2009, defaults to 1901:2009 for CRU and 2010:2099 for GBMs if not specified;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/gbm_flam_prep.R $1 $2 $3
