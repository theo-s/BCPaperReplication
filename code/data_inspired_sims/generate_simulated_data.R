library(dplyr)
library(digest)

# Data Set 1: ASIC 2018 Semi Synthetic data set ================================


# This is a semi synthetic data set inspired by simulations
# in https://arxiv.org/abs/2201.12692
raw_data <- read.csv(file = "~/Dropbox/BCPaperReplication/data/ACIC2018synthetic_data.csv")


tau_true <- function(X) {
  return(0.228 + ifelse(X$X1 < 0.07, .05, 0) + ifelse(X$X2 < -0.69, .05, 0) +
           ifelse(X$C1 %in% c(1,13,14), -.08,0))
}

# Pad the data with extra columns
extra_cols <- matrix(runif(nrow(raw_data)*90), ncol = 90)
data <- cbind(raw_data, extra_cols)
colnames(data) <- c(colnames(data)[1:13], paste0("X",6:95))

# Add the true treatment effect to the data
data$Tau <- tau_true(data)

set.seed(382023)
# We then sample 1000 points to use as the validation set from the
test_idx <- sample(1:nrow(data), replace = FALSE, size = 1000)
test_data <- data[test_idx,]

saveRDS(object = test_data, file = "data/ACIC_Test.RDS")
# Save the true treatment effect for the ASIC semi synthetic data

train_idx <- (1:nrow(data))[!(1:nrow(data) %in% test_idx)]
intersect(train_idx, test_idx)

train_data <- data[train_idx,]

saveRDS(object = test_data, file = "data/ACIC_Train.RDS")


# Data Set 2: GOTV Data with simulated outcomes ================================
social <- readRDS("data/gotv.RDS")



# Now we train a random forest on both training and test data in order to
# create the baseline tau for the simulation



