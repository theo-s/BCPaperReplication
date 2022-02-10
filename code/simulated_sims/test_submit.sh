#!/bin/bash

#SBATCH --job-name=bc_sims
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=7-00:00:00
#SBATCH --array=1-2

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "test_params.txt")
n=$(echo $LINE | cut -d ' ' -f 1)
d=$(echo $LINE | cut -d ' ' -f 2)
seed=$(echo $LINE | cut -d ' ' -f 3)
ex=$(echo $LINE | cut -d ' ' -f 4)

Rscript simul.R --n "$n" --d "$d" --B "100" --seed "$seed" --ex "$ex"
