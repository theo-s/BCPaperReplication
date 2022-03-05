#!/bin/bash

for seed in {1..100}
do
  echo $seed
  sleep 1
  Rscript code/prediction_sims/run_simul.R --seed "$seed"
done

echo All bart Exp1 done

