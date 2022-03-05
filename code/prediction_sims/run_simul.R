# Evaluate the bias of several types of random forest fits on 5 simulated data sets
# from the Breiman adaptive bagging for debiasing paper
library(Rforestry)

if (!interactive()){
  library(argparse)
  pars <- ArgumentParser()

  pars$add_argument("--seed", type = "double", default = 1, help = "random seed")
  args <- pars$parse_args()

  seed <- args$seed
  n <- args$n
} else {
  seed <- 101
}

source("code/prediction_sims/define_experiments_breiman.R")

n <- 1000

# Cycle through the data sets and get the predictions from several RF variants on
# each one
for (data_i in 6){#1:length(names(experiment_list))) {
  data <- readRDS(file = paste0("code/prediction_sims/data/",names(experiment_list)[data_i], ".RDS"))

  # Get test data
  x_test <- data$x
  y_test <- data$y_true


  # Generate training data
  data_func <- experiment_list[[data_i]][[1]]
  y_func <- experiment_list[[data_i]][[2]]

  data <- data_func(n, seed)
  x_train <- data$x
  y_train <- y_func(data, seed)


  # Now train the different models
  rf <- forestry(x = x_train,
                  y = y_train,
                  scale = FALSE,
                  OOBhonest = TRUE,
                  ntree = 2000
                  )

  filename = paste0("code/prediction_sims/results/Exp",data_i,"seed",seed,".RDS")
  res <- data.frame(rf = predict(rf, newdata = x_test),
                    lin = correctedPredict(rf, newdata = x_test),
                    rflin = correctedPredict(rf, newdata = x_test, nrounds = 1))

  print(paste0("Seed ",seed," Exp ", data_i))
  print(head(data.frame(res, y_test = y_test)))

  saveRDS(res, file = filename)
}
