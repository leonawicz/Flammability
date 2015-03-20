#!/bin/sh
#SBATCH --reservation=shiny_2
####SBATCH --exclusive
#SBATCH --ntasks=32
#SBATCH --nodes=1
#SBATCH --account=snap
#SBATCH -p main

echo $1
echo $2
echo $3
echo $4
echo $5
echo $6
echo $7
echo $8
echo $9
echo ${10}
echo ${11}

Rscript /big_scratch/shiny/AlfrescoCalibration.R main=$1 out=$2 input=$2 alf.domain=$4 baseline.year=${11}
Rscript /big_scratch/shiny/AlfrescoFRP.R input=$2 out=$2 alf.domain=$4 $6 $7 $8 $9 ${10}
/big_scratch/shiny/mailPNGs.sh $5 $3 $2/FRP/message.txt


