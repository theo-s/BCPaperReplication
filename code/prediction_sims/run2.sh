#!/bin/bash
#!/bin/bash
#SBATCH --job-name=snr_medhigh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=theo_s@berkeley.edu
#SBATCH -o medhigh.out #File to which standard out will be written
#SBATCH -e medhigh.err #File to which standard err will be written
#SBATCH -p high
##SBATCH --mem-per-cpu=16g
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12

for seed in {1..100}
do
  echo $seed
  sleep 1
  Rscript code/prediction_sims/run_simul.R --seed "$seed" --snr 2
done

echo All med high Sims done

