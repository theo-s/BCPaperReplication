#!/bin/bash

#SBATCH --job-name=x_learner_sims
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=7-00:00:00
#SBATCH --array=1-20
#SBATCH --partition=yugroup

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "code/simulated_sims/xl_params.txt")
n=$(echo $LINE | cut -d ' ' -f 1)
d=$(echo $LINE | cut -d ' ' -f 2)
B=$(echo $LINE | cut -d ' ' -f 3)
seed=$(echo $LINE | cut -d ' ' -f 4)
ex=$(echo $LINE | cut -d ' ' -f 5)
es=$(echo $LINE | cut -d ' ' -f 6)

Rscript code/simulted_sims/simul.R --n "$n" --d "$d" --B "$B" --seed "$seed" --ex "$ex" --es "$es"
