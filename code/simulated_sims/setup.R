library(data.table)

# Reads in the number of estimators, experiments, sample sizes, dimensions,
# and seeds and sets up all of the experiments in a master.txt file
source("define_estimators.R")
source("define_experiments.R")

#num_estimators <- 1:length(estimator_list)
num_experiments <- 3:length(experiment_list)
seeds <- 1:100
p <- c(100)
n <- c(1000)

# Expand the grid and save it
grid <- expand.grid(n,p,seeds,num_experiments)
grid <- grid[order(grid$Var2),]

fwrite(grid,"params.txt", sep=" ",col.names = FALSE, row.names = FALSE)
