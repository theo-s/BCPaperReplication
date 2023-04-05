#!/bin/bash
#SBATCH --job-name=snr_high
#SBATCH --mail-type=ALL
#SBATCH --mail-user=theo_s@berkeley.edu
#SBATCH -o high.out #File to which standard out will be written
#SBATCH -e high.err #File to which standard err will be written
#SBATCH -p high
##SBATCH --mem-per-cpu=16g
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12

for seed in {1..100}
do
  echo $seed
  sleep 1
  Rscript code/prediction_sims/run_simul.R --seed "$seed" --snr 3
done

echo All high Sims done

