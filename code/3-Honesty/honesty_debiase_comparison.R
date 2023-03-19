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

# Runs iterations of the bias correction using the forest, object, as the initial
# model. aggregation controls the aggregation used for constructing the forest
iterative_correction <- function(object,
                                 xnew,
                                 iterations = 10,
                                 aggregation = "oob") {
  preds <- list("in.sample" = list(),
                "x.new" = list())
  for (iter_i in 1:iterations) {
    preds.insample.i <- predict(object, newdata = object)
  }


}

# The purpose of this simulation is to show why oob aggregation is required for
# evaluating the predictions of the honest random forest

x_train <- matrix(runif(n*p), ncol = p)
y_train <- nonlinear_outcome(X = x_train)
data <- data.frame(x_train)
data$y <- y_train


for (iter in 1:10) {
  rf.std <- forestry(x = data[,-ncol(data)],
                     y = data[,ncol(data)] + rnorm(n = 500, sd = 1),
                     seed = iter)

  rf.honest <- forestry(x = data[,-ncol(data)],
                        y = data[,ncol(data)] + rnorm(n = 500, sd = 1),
                        OOBHonest = TRUE,
                        seed = iter)

  preds.std
}



