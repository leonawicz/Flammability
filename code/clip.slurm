#!/bin/sh
## @knitr clip.slurm
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --job-name=clip2ak
#SBATCH --account=snap
#SBATCH -p main

#### $1 optional argument, cru=TRUE, defaults to FALSE if unspecified;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/clim_1km_clip2ak.R $1
