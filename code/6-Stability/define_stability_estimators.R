library(bartMachine)
library(dbarts)
library(cfcausal)
library(causalToolbox)
library(Rforestry)
library(grf)
library(splines)
library(ggplot2)

source("code/6-Stability/xgb_helper.R")
estimator_list <- list()

# A helper function that produces bias corrected predictions
# Takes an object corresponding to the original model,
# a data frame to do out of sample predictions on, and
# two methods corresponding to the algorithms to use in the
# first and second bias correction regressions.
#
# Param object: The random forest object we would like to produce
#               bias corrected predictions for.
#
# Param Xtest: The testing data we want to return predictions for.
#
# Param method1: The method to use for the first bias correction regression.
#                options include c("rf","xgboost","bart","none"). If "rf"
#                is used, the out of bag predictions will be used for the
#                predictions. If "none" is used, only the second correction
#                method will be applied. Default is "none"
#
# Param method2: The second method used for the bias correction regression.
#                The choices include: c("ols","loess","spline","none").
#                the default is to use OLS. If "none" is selected, only the
#                method given in method1 will be used.
#
# Param grid_length: A parameter that only is used when xgboost is selected for
#                    method1. This is the number of hyperparameter configurations
#                    that will be tried for XGboost by the caret package.
GeneralCorrectedPredict <- function(object,
                                    Xtest,
                                    method1 = "none",
                                    method2 = "ols",
                                    grid_length = 50)
{
  if (!(method1 %in% c("rf","xgboost","bart","none"))){
    stop("Method 1 must be one of rf, xgboost, bart, none")
  }
  if (!(method2 %in% c("ols","loess","spline","none"))){
    stop("Method 1 must be one of ols, loess, spline, none")
  }

  # get the training X and Y
  Xtrain <- object@processed_dta$processed_x
  Ytrain <- object@processed_dta$y
  Ytrain_pred <- predict(object, aggregation = "oob")
  Ytest_pred <- predict(object, Xtest)

  data_train <- data.frame(Ypred = Ytrain_pred, Ypred_corrected = Ytrain_pred)
  data_test <- data.frame(Ypred = Ytest_pred, Ypred_corrected = Ytest_pred)

  if (method1 == "rf") {
    # Fit a honest random forest on the training predictions + outcomes and
    # then predict on Xtest
    fit <- forestry(x = data.frame(V1 = Ytrain_pred),
                    y = Ytrain)
    data_test$Ypred_corrected <- predict(fit,
                                         newdata = data.frame(V1 = data_test$Ypred))
    data_train$Ypred_corrected <- predict(fit,
                                          newdata = data.frame(V1 = data_train$Ypred),
                                          aggregation = "oob")
  } else if (method1 == "xgboost") {
    fit <- helper.xgb(Xobs = data.frame(V1 = Ytrain_pred),
                      Yobs = Ytrain,
                      tune_length = grid_length)

    data_test$Ypred_corrected <- pred.xgb(fit,
                                          newdata = data.frame(V1 = data_test$Ypred))

    data_train$Ypred_corrected <- pred.xgb(fit,
                                          newdata = data.frame(V1 = data_train$Ypred))
  } else if (method1 == "bart") {
    fit <- bart(x.train = data.frame(V1 = Ytrain_pred),
                y.train = Ytrain,
                keeptrees = TRUE)

    data_test$Ypred_corrected <- pred.bart(fit,
                                           newdata = data.frame(V1 = data_test$Ypred))

    data_train$Ypred_corrected <- pred.bart(fit,
                                            newdata = data.frame(V1 = data_train$Ypred))
  }

  # Now run the second regression step
  if (method2 == "ols") {
    fit <- lm(Y ~ ., data = data.frame(Y = Ytrain, V1=data_train$Ypred_corrected))

    data_test$Ypred_corrected <- predict(fit,
                                         newdata = data.frame(V1 = data_test$Ypred_corrected))

    data_train$Ypred_corrected <- predict(fit,
                                          newdata = data.frame(V1 = data_train$Ypred_corrected))

  } else if (method2 == "loess") {
    fit <- loess(Y ~ ., data = data.frame(Y = Ytrain, V1 = data_train$Ypred_corrected))

    data_test$Ypred_corrected <- predict(fit,
                                         newdata = data.frame(V1 = data_test$Ypred_corrected))

    data_train$Ypred_corrected <- predict(fit,
                                          newdata = data.frame(V1 = data_train$Ypred_corrected))

  } else if (method2 == "spline") {
    # try using 9 knots with equivalent density
    knots <- unname(quantile(data_train$Ypred_corrected, probs = c(1:9/10)))
    fit <- lm(Y ~ ns(V1, knots = knots),
                         data=data.frame(Y = Ytrain,
                                         V1 = data_train$Ypred_corrected))

    data_test$Ypred_corrected <- unname(predict(fit,
                                         newdata = data.frame(V1 = data_test$Ypred_corrected)))

    data_train$Ypred_corrected <- unname(predict(fit,
                                          newdata = data.frame(V1 = data_train$Ypred_corrected)))

  }

  return(data_test$Ypred_corrected)
}

data <- iris[1:100,]

fit <- loess(Sepal.Length ~ Petal.Width, data = data)

Xtest = iris[101:150,-1]
predict(fit, newdata = iris$Petal.Width[1:50])








