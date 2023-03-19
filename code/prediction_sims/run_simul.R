# Evaluate the bias of several types of random forest fits on 5 simulated data sets
# from the Breiman adaptive bagging for debiasing paper
library(Rforestry)

if (!interactive()){
  library(argparse)
  pars <- ArgumentParser()

  pars$add_argument("--seed", type = "double", default = 1, help = "random seed")
  pars$add_argument("--snr", type = "double", default = 0, help = "signal to noise ratio")
  args <- pars$parse_args()

  seed <- args$seed
  snr <- args$snr
} else {
  seed <- 101
  snr <- 0
}

source("code/prediction_sims/define_experiments_breiman.R")
source("code/6-Stability/define_stability_estimators.R")

n <- 1000

# Cycle through the data sets and get the predictions from several RF variants on
# each one
for (data_i in 1:length(names(experiment_list))) {
  data <- readRDS(file = paste0("code/prediction_sims/data/",names(experiment_list)[data_i], ".RDS"))

  # Get test data
  x_test <- data$x
  y_test <- data$y_true


  # Generate training data
  data_func <- experiment_list[[data_i]][[1]]
  y_func <- experiment_list[[data_i]][[2]]

  data <- data_func(n, seed)
  x_train <- data$x

  # Set default SNR which is different for different data sets
  if (snr == 0) {
    y_train <- y_func(data, seed)
  } else {
    y_train <- y_func(data, seed, snr = snr)
  }



  # Now train the different models
  honest.rf <- forestry(x = x_train,
                        y = y_train,
                        scale = FALSE,
                        OOBhonest = TRUE,
                        ntree = 1000)

  standard.rf <- forestry(x = x_train,
                          y = y_train,
                          scale = FALSE,
                          ntree = 1000)

  filename = paste0("code/prediction_sims/results/Exp",data_i,"seed",seed,"snr",snr,".RDS")
  res <- data.frame(hon.none = predict(honest.rf, newdata = x_test),
                    hon.lin = GeneralCorrectedPredict(honest.rf,
                                                  Xtest = x_test,
                                                  method1 = "none",
                                                  method2 = "ols"),
                    hon.rflin = GeneralCorrectedPredict(honest.rf,
                                                    Xtest = x_test,
                                                    method1 = "rf",
                                                    method2 = "ols"),
                    std.none = predict(standard.rf, newdata = x_test),
                    std.lin = GeneralCorrectedPredict(standard.rf,
                                                      Xtest = x_test,
                                                      method1 = "none",
                                                      method2 = "ols"),
                    std.lin = GeneralCorrectedPredict(standard.rf,
                                                      Xtest = x_test,
                                                      method1 = "rf",
                                                      method2 = "ols")
                    )

  print(paste0("Seed ",seed," Exp ", data_i, " SNR ", snr))
  print(head(data.frame(res, y_test = y_test)))

  saveRDS(res, file = filename)
}
