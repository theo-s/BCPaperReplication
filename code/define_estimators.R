library(bartMachine)
library(cfcausal)
library(causalToolbox)
library(Rforestry)
library(grf)

estimator_list <- list()

source("code/define_helpers.R")

## Get counterfactual intervals by S-Learner with RF
## The vector corrections contains the corrections to use for the
## forest. Possible corrections are "none" and "bc1" through "bc6"
estimator_list[["s_rf"]] <- function(X, Y, T, Xtest,
                                              B = 500, corrections=c("none")){
  if (B == 0){
    df_tau <- df_Y <- list(cr = NA, len = NA)
    return(list(tau = df_tau, Y1 = df_Y))
  }

  sl_rf <- causalToolbox::S_RF(feat = X, tr = T, yobs = Y, nthread = 0,
                               mu.forestry =
                                 list(
                                   relevant.Variable = 1:ncol(X),
                                   ntree = 1000,
                                   replace = TRUE,
                                   sample.fraction = 1,
                                   mtry = ncol(X),
                                   nodesizeSpl = 5,
                                   nodesizeAvg = 5,
                                   nodesizeStrictSpl = 1,
                                   nodesizeStrictAvg = 1,
                                   splitratio = 1,
                                   middleSplit = FALSE,
                                   OOBhonest = TRUE
                                 ))

  # Loop through the possible corrections
  return_list <- list()

  CI_list <- slearner_CI(sl_rf, Xtest, B = B,
                    verbose = TRUE, nthread = 0,
                    correction = corrections)

  for (correction_type in corrections) {
    cate_esti_rf <- EstimateCorrectedCATE(theObject = sl_rf,
                                          feature_new = Xtest,
                                          correction = correction_type)

    return_list[[correction_type]] <- list(tau = CI_list[[correction_type]][,2:3], preds = cate_esti_rf)
  }

  return(return_list)
}

## Get counterfactual intervals by Causal Forest
estimator_list[["causalForest"]] <- function(X, Y, T, Xtest,
                                             B = 50){
  fit <- grf::causal_forest(X, Y, T)
  pred <- predict(fit, Xtest, estimate.variance = TRUE)
  CI <- data.frame(low = pred[, 1] - 1.96 * sqrt(pred[, 2]),
                   high = pred[, 1] + 1.96 * sqrt(pred[, 2]))

  return(list(tau = CI, preds = pred))
}


## Get counterfactual intervals by BART
estimator_list[["bart"]] <- function(X, Y, T, Xtest,
                                     B = 50){

  library(bartCause)
  X <- as.data.frame(X)
  Xtest <- as.data.frame(Xtest)
  fit <- bartCause::bartc(response = Y,
                          treatment = T,
                          confounders = X,
                          keepTrees = TRUE)
  # Get predictions for CATE for each test observation
  preds <- predict(fit, newdata = Xtest, type = "icate")

  # Create credible intervals from 2.5% and 97.5% of the poterior draws
  CI.lower <- apply(preds, 2, function(x)
    quantile(x, c(.025)))

  CI.upper <- apply(preds, 2, function(x)
    quantile(x, c(.975)))

  CI <- data.frame(low = CI.lower,
                   high = CI.upper)

  return(list(tau = CI, preds = apply(preds, 2, mean)))
}


estimator_list[["x_rf"]] <- function(X, Y, T, Xtest,
                                     B = 500, corrections=c("none")){
  if (B == 0){
    df_tau <- df_Y <- list(cr = NA, len = NA)
    return(list(tau = df_tau, Y1 = df_Y))
  }
  
  x_rf <- causalToolbox::X_RF(feat = X, tr = T, yobs = Y, nthread = 0, 
                              correction = list("nrounds" = 1, "linear" = FALSE))
  
  # Loop through the possible corrections
  return_list <- list()
  
  CI_list <- slearner_CI(x_rf, Xtest, B = B,
                         verbose = TRUE, nthread = 0)
  
  return(CI_list)
}

estimator_list[["x_rf_none"]] <- function(X, Y, T, Xtest,
                                     B = 500, corrections=c("none")){
  if (B == 0){
    df_tau <- df_Y <- list(cr = NA, len = NA)
    return(list(tau = df_tau, Y1 = df_Y))
  }
  
  x_rf <- causalToolbox::X_RF(feat = X, tr = T, yobs = Y, nthread = 0)
  
  # Loop through the possible corrections
  return_list <- list()
  
  CI_list <- slearner_CI(x_rf, Xtest, B = B,
                         verbose = TRUE, nthread = 0)
  
  return(CI_list)
}

