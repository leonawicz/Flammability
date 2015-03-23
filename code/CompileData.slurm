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
echo ${12}

Rscript /big_scratch/shiny/AlfrescoCalibration.R main=$1 out=$2 input=$2 alf.domain=$4 baseline.year=${12}

if [ $6 -eq 1 ]
then
Rscript /big_scratch/shiny/AlfrescoFRP.R input=$2 out=$2 alf.domain=$4 $7 $8 $9 ${10} ${11}
fi

/big_scratch/shiny/mailPNGs.sh $5 $3 $2/message.txt


