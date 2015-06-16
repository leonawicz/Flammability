#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --job-name=tpvcGCM
#SBATCH --account=snap
#SBATCH -p main

#### $1 optional argument, allcavm=TRUE, defaults to FALSE if unspecified;
Rscript /workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code/tpByVeg_CMIP5.R $1
