# Define list of different experimental setups.
# Each experiment consists of functions to generate X, tau, SD, propensity score,
# and the error distribution
# All data should have outcomes Y, Treatment effect Tau, Treatment Tr
experiment_list <- list()


## Experiment 1 ACIC 2018 Challenge data with synthetic outcomes
experiment_list[["Experiment1"]] <- function(
  n = 1000,
  seed = 1,
  snr = 1
) {
  # Generate 1 run of confidence intervals based on a sample
  # of the training data in # ACIC_Train.RDS
  # Save the results of the
  set.seed(seed)

  # Sample the training data
  train_data <- readRDS("data/ACIC_Train.RDS")
  test_data <- readRDS("data/ACIC_Test.RDS")
  sample_idx <- sample(1:nrow(train_data), size = n, replace = TRUE)

  # Add noise to correspond to the SNR
  noise_sd <- sqrt(snr) / sd(train_data$Tau)
  train_data$Y <- test_data$Y + rnorm(n = nrow(train_data), sd = noise_sd)

  return(list("Train" = train_data[sample_idx,], "Test" = test_data))
}

## Experiment 2 GOTV Data with the T Learner full sample estimate as the True Tau
experiment_list[["Experiment2"]] <- function(
  n = 1000,
  seed = 1,
  snr = 1
) {

  set.seed(seed)

  # Sample the training data
  train_data <- readRDS("data/GOTV_Train.RDS")
  test_data <- readRDS("data/GOTV_Test.RDS")
  sample_idx <- sample(1:nrow(train_data), size = n, replace = TRUE)

  # Add noise to correspond to the SNR
  noise_sd <- sqrt(snr) / sd(train_data$Tau)
  train_data$Y <- test_data$Y + rnorm(n = nrow(train_data), sd = noise_sd)

  return(list("Train" = train_data[sample_idx,], "Test" = test_data))
}

## Experiment 3 Transphobia data with T Learner full sample estimate as the True Tau
experiment_list[["Experiment3"]] <- function(
  n = 1000,
  seed = 1,
  snr = 1
) {

  # Set Seed
  set.seed(seed)

  # Sample the training data
  train_data <- readRDS("data/TRAN_Train.RDS")
  test_data <- readRDS("data/TRAN_Test.RDS")
  sample_idx <- sample(1:nrow(train_data), size = n, replace = TRUE)

  # Add noise to correspond to the SNR
  noise_sd <- sqrt(snr) / sd(train_data$Tau)
  train_data$Y <- test_data$Y + rnorm(n = nrow(train_data), sd = noise_sd)

  return(list("Train" = train_data[sample_idx,], "Test" = test_data))
}

