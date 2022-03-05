#!/bin/bash

for seed in {3..100}
do
  echo $seed
  sleep 1
  Rscript code/6-Stability/run_stability_simul.R --seed "$seed"
done

echo ALL Stability sims done done

