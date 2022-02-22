library(xtable)
library(dplyr)
library(reshape)
library(tidyr)
source("define_experiments_breiman.R")
exp <- 1

get_results <- function(exp = 1) {
  # Read in the data
  data <- readRDS(paste0("data/Exp",exp,".RDS"))

  preds_1 <- matrix(nrow = 100, ncol = 1e3)
  preds_2 <- matrix(nrow = 100, ncol = 1e3)
  preds_3 <- matrix(nrow = 100, ncol = 1e3)

  for (seed in  1:100) {
    # print(seed)
    # Read in the true outcomes
    res <- readRDS(paste0("results/Exp",exp,"seed",seed,".RDS"))

    # Save the preds
    preds_1[seed,] <- res$rf
    preds_2[seed,] <- res$lin
    preds_3[seed,] <- res$rflin
  }


  # Get variance of preds
  var_1 <- mean(apply(preds_1, MARGIN = 2, FUN = sd))
  var_2 <- mean(apply(preds_2, MARGIN = 2, FUN = sd))
  var_3 <- mean(apply(preds_3, MARGIN = 2, FUN = sd))

  # Get biases
  bias_1 <- mean(abs(apply(preds_1, MARGIN = 2, FUN = mean) - data$y_true))
  bias_2 <- mean(abs(apply(preds_2, MARGIN = 2, FUN = mean) - data$y_true))
  bias_3 <- mean(abs(apply(preds_3, MARGIN = 2, FUN = mean) - data$y_true))

  return(data.frame(Exp = rep(exp, 3),
                    Es = c("RF", "RF_lin", "RF_step_lin"),
                    Var = c(var_1,var_2,var_3),
                    Bias = c(bias_1,bias_2,bias_3)))
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