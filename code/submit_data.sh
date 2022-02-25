#!/bin/bash

#SBATCH --job-name=db_coverage
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=10-00:00:00
#SBATCH --array=1-5

IN=$(sed -n ${SLURM_ARRAY_TASK_ID}p "code/data_inspired_sims/params.txt")
snr=$(echo $IN | cut -d ' ' -f 1)
B=$(echo $IN | cut -d ' ' -f 2)
seed=$(echo $IN | cut -d ' ' -f 3)
ex=$(echo $IN | cut -d ' ' -f 4)
es=$(echo $IN | cut -d ' ' -f 5)
f=$(echo $IN | cut -d ' ' -f 6)

Rscript code/data_inspired_sims/run_sims.R --snr "$snr" --B "$B" --seed "$seed" --ex "$ex" --es "$es" --f "$f"
