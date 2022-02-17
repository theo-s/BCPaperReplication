# Define list of different experimental setups.
# Each experiment consists of functions to generate X, tau, SD, propensity score,
# and the error distribution
# All data should have outcomes Y, Treatment effect Tau, Treatment Tr
experiment_list <- list()


## Ex 1 ACIC 2018 Challenge data with synthetic outcomes
experiment_list[["Ex1"]] <- function(
  n = 1000,
  seed = 1,
  snr = 1
) {
  # Generate 1 run of confidence intervals based on a sample
  # of the training data in # ACIC_Train.RDS
  # Save the results of the
  set.seed(seed)

  # Sample the training data
  test_data <- readRDS("data/ACIC_Test.RDS")
  sample_idx <- sample(1:nrow(test_data), size = n, replace = TRUE)
  train_data <- test_data[sample_idx,]

  train_data$Y <- ifelse(train_data$Tr,
                         train_data$Y + train_data$Tau,
                         train_data$Y)
  # Add noise to correspond to the SNR
  noise_sd <- sd(train_data$Tau) / sqrt(snr)
  train_data$Y <- test_data$Y + rnorm(n = nrow(train_data), sd = noise_sd)

  return(list("Train" = train_data[sample_idx,], "Test" = test_data))
}

## Ex 2 GOTV Data with the T Learner full sample estimate as the True Tau
experiment_list[["Ex2"]] <- function(
  n = 22946,
  seed = 1,
  snr = 1
) {

  set.seed(seed)

  # Sample the training data
  test_data <- readRDS("data/GOTV_Test.RDS")
  sample_idx <- sample(1:nrow(test_data), size = n, replace = TRUE)

  train_data <- test_data[sample_idx,]

  # Add noise to correspond to the SNR
  noise_sd <- sd(train_data$Tau) / sqrt(snr)
  noise_outcome <- rbinom(p = noise_sd, n = nrow(train_data), size = 1)
  fn <- function(x) {
    if (x == 1) {
      return(0)
    } else if (x==0) {
      return(1)
    }
  }

  # If that outcome should be noised, we flip the outcome
  noised_y <- ifelse(noise_outcome == 1, sapply(train_data$Y,fn), train_data$Y)

  train_data$Y <- noised_y

  return(list("Train" = train_data, "Test" = test_data))
}

## Ex 3 Transphobia data with T Learner full sample estimate as the True Tau
experiment_list[["Ex3"]] <- function(
  n = 150,
  seed = 1,
  snr = 1
) {

  # Set Seed
  set.seed(seed)

  # Sample the training data
  test_data <- readRDS("data/tra_Test.RDS")
  sample_idx <- sample(1:nrow(test_data), size = n, replace = TRUE)
  train_data <- test_data[sample_idx,]

  # Add noise to correspond to the SNR
  noise_sd <- sd(train_data$Tau) / sqrt(snr)
  train_data$Y <- test_data$Y + rnorm(n = nrow(train_data), sd = noise_sd)

  return(list("Train" = train_data, "Test" = test_data))
}

