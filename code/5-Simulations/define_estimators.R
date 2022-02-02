library(bartMachine)
library(cfcausal)
library(causalToolbox)
library(Rforestry)
library(grf)

estimator_list <- list()

source("define_helpers.R")

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_none"]] <- function(X, Y, T, Xtest,
                                              B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(theObject = sl_rf,
                                        feature_new = Xtest,
                                        correction = "none")

  CI <- slearner_CI(sl_rf, Xtest, B = B,
                    verbose = TRUE, nthread = 0,
                    correction = "none")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc1"]] <- function(X, Y, T, Xtest,
                                         B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(theObject = sl_rf,
                                        feature_new = Xtest,
                                        correction = "bc1")

  CI <- slearner_CI(sl_rf, Xtest, B = B,
                    verbose = TRUE, nthread = 0,
                    correction = "bc1")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc2"]] <- function(X, Y, T, Xtest,
                                         B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(sl_rf, Xtest,
                                              correction = "bc2")
  CI <- slearner_CI(sl_rf, Xtest, B = B,
                              verbose = TRUE, nthread = 0,
                              correction = "bc2")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc3"]] <- function(X, Y, T, Xtest,
                                             B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(sl_rf, Xtest,
                                              correction = "bc3")
  CI <- slearner_CI(sl_rf, Xtest, B = B,
                              verbose = TRUE, nthread = 0,
                              correction = "bc3")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc4"]] <- function(X, Y, T, Xtest,
                                             B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(sl_rf, Xtest,
                                              correction = "bc4")
  CI <- slearner_CI(sl_rf, Xtest, B = B,
                              verbose = TRUE, nthread = 0,
                              correction = "bc4")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc5"]] <- function(X, Y, T, Xtest,
                                             B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(sl_rf, Xtest,
                                        correction = "bc5")
  CI <- slearner_CI(sl_rf, Xtest, B = B,
                    verbose = TRUE, nthread = 0,
                    correction = "bc5")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
}

## Get counterfactual intervals by S-Learner with RF
estimator_list[["slearner_bc6"]] <- function(X, Y, T, Xtest,
                                             B = 50){
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
  cate_esti_rf <- EstimateCorrectedCATE(sl_rf, Xtest,
                                        correction = "bc6")
  CI <- slearner_CI(sl_rf, Xtest, B = B,
                    verbose = TRUE, nthread = 0,
                    correction = "bc6")[, 2:3]
  return(list(tau = CI, preds = cate_esti_rf))
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
  Y[T == 0] <- NA # Include this line as it was written in the 
  		  # cfcausal package, this means that BART is only 
		  # trained on the treatment observations, and basically
		  # assumes the control outcome is zero (which is true in this case)
		  # This gives BART a huge advantage over RF when RF is trained
		  # on both cases and has to learn the importance of the 
		  # treatment indicator
  ids <- !is.na(Y)
  X <- as.data.frame(X)
  Xtest <- as.data.frame(Xtest)
  fit <- bartMachine::bartMachine(X[ids, ], Y[ids],
                                  verbose = FALSE)
  CI_tau <- bartMachine::calc_credible_intervals(fit, new_data = Xtest, ci_conf = 0.95)
  preds <- predict(fit, new_data = Xtest)
  return(list(tau = CI_tau, preds = preds))
}

## Get counterfactual intervals by weighted split CQR
# estimator_list[["conformal"]] <- function(X, Y, Xtest, outfun, quantiles){
#   res <- cfcausal::conformalCf(X, Y, quantiles = quantiles, outfun = outfun, psfun = NULL, useCV = FALSE)
#   CI <- predict(res, Xtest, alpha = 0.05)
#   return(list(tau = CI, preds = NA))
# }

