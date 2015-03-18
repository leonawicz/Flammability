#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=1
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=FlamMultipliers
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU31\', model=\'GFDL-CM3\', etc., required;
#### $3 mapset, passed as mapset=\'3models_tif\' or mapset=\'5models_tif\', required;
#### $4 cp2scratch, passed as TRUE or FALSE, optional, defaults to TRUE;
#### $5 cp_originals, passed as TRUE or FALSE, optional, defaults to TRUE;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/FlammabilityMapMultipliers.R $1 $2 $3 $4 $5
