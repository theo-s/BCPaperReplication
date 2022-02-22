library(bartMachine)
library(cfcausal)
library(causalToolbox)
library(Rforestry)
library(grf)
library(splines)
library(ggplot2)

source("6-Stability/xgb_helper.R")
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




}

data <- iris[1:100,]

fit <- loess(Sepal.Length ~ Petal.Width, data = data)

predict(fit, newdata = iris$Petal.Width[1:50])








