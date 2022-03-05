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
source("code/6-Stability/define_stability_estimators.R")

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

  method_list = expand.grid(c("none","rf","xgboost","bart"),
                            c("none","ols","loess","spline"))

  # Now train the different models
  rf <- forestry(x = x_train,
                 y = y_train,
                 scale = FALSE,
                 OOBhonest = TRUE,
                 ntree = 1000
  )

  filename = paste0("code/6-Stability/results/Exp",data_i,"seed",seed,".RDS")

  results_df <- data.frame(matrix(data=NA,ncol=nrow(method_list),nrow = nrow(x_test)))
  colnames(results_df) <- apply(method_list, MARGIN = 1, FUN=function(x){return(paste0(x[1],".",x[2]))})

  for (i in 1:nrow(method_list)) {
    pred_i <- try(GeneralCorrectedPredict(rf,
                                          Xtest = x_test,
                                          method1 = method_list$Var1[i],
                                          method2 = method_list$Var2[i]))
    results_df[,i] <- pred_i
  }

  print(paste0("Seed ",seed," Exp ", data_i))
  print(head(data.frame(results_df, y_test = y_test)))

  saveRDS(results_df, file = filename)
}
