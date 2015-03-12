#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --job-name=clip2ak
#SBATCH --account=snap
#SBATCH -p main

Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/CMIP5_Climate_1km/1km_clip_to_AK.R
