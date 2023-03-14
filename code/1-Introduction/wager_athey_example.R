library(ggplot2)
library(dplyr)
library(Rforestry)
library(reshape2)
library(distillML)
# Run this 100 times

# Define the DGP from
nonlinear_outcome <- function(X){
  2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}

# Generate Data
set.seed(23234243)
n <- 500
p <- 10
x_test <- matrix(runif(n*p), ncol = p)
y_test <- nonlinear_outcome(X = x_test)
data_test <- data.frame(x_test)
data_test$y <- y_test

all_data <- data.frame(x = x_test[,1],
                       Truth = y_test)

source("code/6-Stability/define_stability_estimators.R")
source("code/6-Stability/xgb_helper.R")

for (mtry in c(3)) {

  n_reps <- 100

  all_preds <- matrix(ncol = n_reps, nrow = 500)
  all_corrected_preds <- matrix(ncol = n_reps, nrow = 500)
  # all_xgb_preds <- matrix(ncol = n_reps, nrow = 500)
  for (iter in 1:n_reps) {
    set.seed(iter)

    x_train <- matrix(runif(n*p), ncol = p)
    y_train <- nonlinear_outcome(X = x_train)
    data <- data.frame(x_train)
    data$y <- y_train

    rf <- forestry(x = data[,-ncol(data)],
                   y = data[,ncol(data)] + rnorm(n = 500, sd = 1),
                   mtry = mtry,
                   seed = iter,
                   OOBhonest = TRUE,
                   monotoneAvg = TRUE)

    # xgb <- helper.xgb(Xobs = data[,-ncol(data)],
    #                   Yobs = data[,ncol(data)] + rnorm(n = 500, sd = 1))

    preds <- predict(rf, newdata = x_test)
    # preds.xgb <- pred.xgb(xgb, newdata = x_test)

    correct.preds <- GeneralCorrectedPredict(rf,
                                             Xtest = x_test,
                                             method1 = "rf",
                                             method2 = "ols")

    all_preds[,iter] <- preds
    all_corrected_preds[,iter] <- correct.preds
    # all_xgb_preds[,iter] <- preds.xgb
  }

  mean_preds <- apply(all_preds, MARGIN = 1, FUN = mean)
  mean_corrected_preds <- apply(all_corrected_preds, MARGIN = 1, FUN = mean)
  # mean_xgb_preds <- apply(all_xgb_preds, MARGIN = 1, FUN = mean)

  all_data <- cbind(all_data, mean_preds, mean_corrected_preds)#, mean_xgb_preds)
}

colnames(all_data) <- c("x", "Truth","Random Forest","Debiased Random Forest")#,"xgboost")


# Look at mean pointwise bias^2 of the random forest predictions
mean((mean_preds - y_test)**2)
# Look at mean pointwise bias^2 of the corrected predictions
mean((mean_corrected_preds - y_test)**2)
# Look at mean pointwise bias^2 of the xgboost predictions
#mean((mean_xgb_preds - y_test)**2)


# Look at the fits themselves

# Just with standard predictions ===============================================
forest_predictor <- Predictor$new(model = rf,
                                  data=data.frame(x_test, y = y_test),
                                  y="y",
                                  task = "regression")
forest_interpreter <- Interpreter$new(forest_predictor,
                                      grid.size = 25)


# xgboost_predictor <- Predictor$new(model = xgb,
#                                    data=data.frame(x_test, y = y_test),
#                                    y="y",
#                                    task = "regression",
#                                    predict.func = pred.xgb)
# xgboost_interpreter <- Interpreter$new(xgboost_predictor,
#                                        grid.size = 25)

plots.std <- plot(forest_interpreter, features = c("X1","X2"))
# plots.xgb <- plot(xgboost_interpreter, features = c("X1","X2"))

# With debiased predictions  ===================================================

# Wrapper to make PDP functions for forest using corrected predictions
# NOTE, this is just an OLS correction, rf + OLS takes a long time to run when
# making PDPs
pred.func <- function(rf, newdata) {
  return(GeneralCorrectedPredict(rf,
                                 Xtest = newdata,
                                 method1 = "none",
                                 method2 = "ols"))
}


corrected_forest_predictor <- Predictor$new(model = rf,
                                  data=data.frame(x_test, y = y_test),
                                  y="y",
                                  predict.func = pred.func,
                                  task = "regression")
corrected_forest_interpreter <- Interpreter$new(corrected_forest_predictor,
                                      grid.size = 25)
plots.bc <- plot(corrected_forest_interpreter, features = c("X1","X2"))


# Finally use the true outcome function ========================================
pred.func.true <- function(rf, newdata) {
  return(nonlinear_outcome(newdata))
}


true_predictor <- Predictor$new(model = rf,
                                  data=data.frame(x_test, y = y_test),
                                  y="y",
                                  predict.func = pred.func.true,
                                  task = "regression")
true_interpreter <- Interpreter$new(true_predictor,
                                      grid.size = 25)


plots.true <- plot(true_interpreter, features = c("X1","X2"))

plots.smooth.rf <- plot(corrected_forest_interpreter, features = c("X1","X2"), smooth = TRUE)
plots.smooth.std <- plot(forest_interpreter, features = c("X1","X2"), smooth = TRUE)
#plots.smooth.xgb <- plot(xgboost_interpreter, features = c("X1","X2"), smooth = TRUE)
# plots.true$X1
# plots.true$X2
# plots.true$X1.X2
library(ggpubr)

# Now compare PDPs side by side:
ggarrange(plots.true$X1,plots.bc$X1, plots.std$X1,
          ncol = 3,
          common.legend = TRUE,
          align = "h",
          labels=c("True PDP", "Corrected PDP (rf)", "Standard PDP (rf)"))

ggarrange(plots.true$X1,plots.smooth.rf$X1, plots.smooth.std$X1,
          ncol = 3,
          common.legend = TRUE,
          align = "h",
          labels=c("True PDP", "Smoothed PDP (corrected rf)", "Smoothed PDP (rf)"))



ggarrange(plots.true$X2,plots.bc$X2, plots.std$X2,plots.xgb$X2,plots.smooth.xgb$X2,plots.smooth.rf$X2,
          ncol = 6,
          common.legend = TRUE,
          labels=c( "True PDP", "Corrected PDP (rf)", "Standard PDP (rf)", "Standard PDP (xgb)","Smoothed (xgb)","Smoothed (rf)"))
#
# ggarrange(plots.true$X1.X2, plots.bc$X1.X2, plots.std$X1.X2,plots.xgb$X1.X2,
#           ncol = 4,
#           common.legend = TRUE,
#           labels=c("True PDP", "Corrected PDP (rf)", "Standard PDP (rf)", "Standard PDP (xgb)","Smoothed (xgb)","Smoothed (rf)"))
#



