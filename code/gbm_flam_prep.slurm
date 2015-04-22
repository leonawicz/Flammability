#!/bin/sh
####SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=1
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=gbmFlamPrep
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU31\', model=\'GFDL-CM3\', etc., required;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/gbm_flam_prep.R $1 $2
