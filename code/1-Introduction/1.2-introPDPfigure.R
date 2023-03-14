library(ggplot2)
library(dplyr)
library(Rforestry)
library(reshape2)
library(distillML)
library(ggpubr)
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

set.seed(100)
x_train <- matrix(runif(n*p), ncol = p)
y_train <- nonlinear_outcome(X = x_train)
data <- data.frame(x_train)
data$y <- y_train


rf <- forestry(x = data[,-ncol(data)],
               y = data[,ncol(data)] + rnorm(n = 500, sd = 1),
               mtry = 3,
               seed = 100,
               OOBhonest = TRUE)


# Just with standard predictions ===============================================
forest_predictor <- Predictor$new(model = rf,
                                  data=data.frame(x_test, y = y_test),
                                  y="y",
                                  task = "regression")
forest_interpreter <- Interpreter$new(forest_predictor,
                                      grid.size = 25)
plots.std <- plot(forest_interpreter, features = c("X1","X2"))


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

# Now compare PDPs side by side:
ggarrange(plots.true$X1 + ylim(0,4.5),
          plots.bc$X1 + ylim(0,4.5),
          plots.std$X1 + ylim(0,4.5),
          ncol = 3,
          common.legend = TRUE,
          label.x = .2,
          label.y = .95,
          font.label = list(size = 10),
          align = "h",
          labels=c("True Outcome", "Corrected RF", "Standard RF"))
ggsave("~/Dropbox/BCPaperReplication/figures/1-Introduction/aw_example_pdp.pdf",
       height=5, width = 9)
