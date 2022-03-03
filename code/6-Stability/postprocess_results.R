library(xtable)
library(dplyr)
library(reshape)
library(tidyr)

source("code/prediction_sims/define_experiments_breiman.R")
source("code/6-Stability/define_stability_estimators.R")
method_list = expand.grid(c("none","rf","xgboost","bart"),
                          c("none","ols","loess","spline"))
exp <- 1

get_results <- function(exp = 1) {
  # Read in the data
  data <- readRDS(paste0("code/prediction_sims/data/Exp",exp,".RDS"))

  preds_list <- list()
  for (i in 1:nrow(method_list)) {
    preds_list[[i]] <- matrix(nrow = 100, ncol = 1e3)
  }

  for (seed in  1:100) {
    # print(seed)
    # Read in the true outcomes
    res <- readRDS(paste0("code/6-Stability/results/Exp",exp,"seed",seed,".RDS"))

    # Save the preds
    for (i in 1:nrow(method_list)) {
      preds_list[[i]][seed,] <- res[[i]]
    }
  }


  # Get variance of preds
  variances <- list()
  for (i in 1:nrow(method_list)) {
    variances[[i]] <-  mean(apply(preds_list[[i]], MARGIN = 2, FUN = sd))
  }

  bias_list <- list()
  for (i in 1:nrow(method_list)) {
    bias_list[[i]] <-  mean(abs(apply(preds_list[[i]], MARGIN = 2, FUN = mean) - data$y_true))
  }

  return(data.frame(Exp = rep(exp, nrow(method_list)),
                    Es = apply(method_list,1,function(x){return(paste0(x[1],".",x[2]))}),
                    Var = unlist(variances),
                    Bias = unlist(bias_list)))
}


results <- data.frame(matrix(NA,ncol=4,nrow=0))
colnames(results) <- c("Exp", "Es", "Var", "Bias")

for (i in 1:5) {
  cur_res <- get_results(i)
  results <- rbind(results,cur_res)
}

colnames(results) <- c("Experiment #", "Estimator", "sqrt(Variance)","|Bias|")

results[,2] <- rep(c("Random Forest","Random Forest (linear debiase)", "Random Forest (linear + rf debiase)"),max(results[,1]))

results_bias <- results[,c(1,2,4)]
results_var <- results[,c(1,2,3)]

results_bias %>%
  pivot_wider(names_from = "Estimator",values_from = "|Bias|") -> results_bias

results_var %>%
  pivot_wider(names_from = "Estimator",values_from = "sqrt(Variance)") -> results_var

xtable(results_bias, caption = "|Bias| for estimators across all experiments")
xtable(results_var, caption = "sqrt(Variance) for estimators across all experiments")
