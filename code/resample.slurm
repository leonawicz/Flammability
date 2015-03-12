#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas09
#SBATCH --job-name=resamp2km1km
#SBATCH --account=snap
#SBATCH -p main

Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/CMIP5_resample_2km_1km.R
