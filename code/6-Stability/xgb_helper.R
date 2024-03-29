# Fits an XGboost model trained with the caret package for hyperparameter tuning
helper.xgb <- function(Xobs,
                       Yobs,
                       tune_length = 20,
                       cv_folds = 5,
                       note = NA) {

  library(xgboost)
  library(caret)
  xgb_grid <- data.frame(
    nrounds = 50,
    max_depth = sample(1:8, size = tune_length, replace = TRUE),
    eta = sample(c(.2, .3, .1, .05, 01, .001), size = tune_length, replace = TRUE),
    gamma = sample(0:3, size = tune_length, replace = TRUE),
    colsample_bytree = sample(c(0.4, 0.7, 1.0), size = tune_length, replace = TRUE),
    min_child_weight = sample(c(0.5, 1, 1.5), size = tune_length, replace = TRUE),
    subsample = .5
  )

  fitControl <- trainControl(
    method = "adaptive_cv",
    ## fold CV based on supplied number of folds
    number = cv_folds,
    ## repeated 4 times
    repeats = 4,
    verboseIter = TRUE,
    allowParallel = TRUE,
    adaptive = list(
      min = 3,
      alpha = 0.01,
      method = "gls",
      complete = FALSE
    )
  )

  print("Training XGboost")
  xg_fit <- train(
    y = Yobs,
    x = Xobs,
    method = "xgbTree",
    metric = "RMSE",
    tuneGrid = xgb_grid,
    tuneLength = tune_length,
    trControl = fitControl
  )

  #print("Saving params")

  # Save Tuning parameters ---------------------------------------------------
  # dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  # saveRDS(
  #   object = list(xg_fit),
  #   file = paste0("replicationCode/tuningParam/xgboost", note, ".RDS")
  # )

  return(list("xg_fit" = xg_fit))
}

# Predicts using a trained XGboost model
pred.xgb <- function(estimator, newdata) {
  fit <- estimator[[1]]
  return(predict(fit$finalModel, newdata = as.matrix(newdata)))
}

pred.bart <- function(estimator, newdata) {
  s <- predict(estimator, newdata)
  return(apply(s, 2, mean))
  #return(s) for now return entire posterior sample
}
