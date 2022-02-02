#!/bin/bash

for seed in {1..100}
do
  Rscript run_simul.R --seed "$seed"
done

echo All bart Exp1 done

