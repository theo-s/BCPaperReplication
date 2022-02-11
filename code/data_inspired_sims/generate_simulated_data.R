library(dplyr)
library(digest)
library(Rforestry)

# Data Set 1: ASIC 2018 Semi Synthetic data set ================================
#
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
#
# This has real covariates from the gotv
library(Rforestry)
social <- readRDS("data/gotvSMALL.RDS")

# Add cumulative voting features + rename some covariates
social$CVH <- apply(social[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == 1, 1, sum)
social$CGVH <- apply(social[, c("g2000", "g2002")] == 1, 1, sum)
social$CPVH <- apply(social[, c("p2000", "p2002", "p2004")] == 1, 1, sum)
colnames(social) <- c("sex","g2000","g2002","p2000","p2002","p2004","age","Tr",
                      "Y","CVH","CGVH","CPVH")

# Now we train a random forest on both training and test data in order to
# create the baseline tau for the simulation
forest.treat <- forestry(
  x = social %>% filter(Tr == 1) %>% dplyr::select(-Tr,-Y),
  y = social %>% filter(Tr == 1) %>% dplyr::select(Y) %>% .[,1],
  ntree = 1000,
  seed = 11
)

forest.control <- forestry(
  x = social %>% filter(Tr == 0) %>% dplyr::select(-Tr,-Y),
  y = social %>% filter(Tr == 0) %>% dplyr::select(Y) %>% .[,1],
  ntree = 1000,
  seed = 11
)

Y0 <- predict(forest.control, newdata = social %>% dplyr::select(-Tr,-Y))
Y1 <- predict(forest.treat, newdata = social %>% dplyr::select(-Tr,-Y))

set.seed(11)
# Truncate probabilities before flipping coins
Y0 <-  ifelse(Y0 < .01, .1, ifelse(Y0 > .99, .99, Y0))
Y1 <-  ifelse(Y1 < .01, .1, ifelse(Y1 > .99, .99, Y1))

Y0_binary <- rbinom(p = Y0, n = length(Y0), size = 1)
Y1_binary <- rbinom(p = Y1, n = length(Y1), size = 1)

social$Y <- ifelse(social$Tr, Y1_binary,Y0_binary)
social$Tau <- Y1 - Y0

saveRDS(object = social, file = "data/gotv_Test.RDS")

saveForestry(forest.control, filename = "data/gotv_forest_control.Rda")
saveForestry(forest.treat, filename = "data/gotv_forest_treat.Rda")



# Data set 3: transphobia data set using T Learners as the true outcomes =======
tra <- readRDS("data/cleaned_transphobia.rds")

# Cleaning is the same as done in the X Learner paper, see data/clean_transphobia.R
# for details and data/transphobia.rda for the raw data
mean(tra$Y[tra$Tr == 1]) - mean(tra$Y[tra$Tr == 0])


forest.treat <- forestry(
  x = tra %>% filter(Tr == 1) %>% dplyr::select(-Tr,-Y),
  y = tra %>% filter(Tr == 1) %>% dplyr::select(Y) %>% .[,1],
  ntree = 1000,
  seed = 11
)

forest.control <- forestry(
  x = tra %>% filter(Tr == 0) %>% dplyr::select(-Tr,-Y),
  y = tra %>% filter(Tr == 0) %>% dplyr::select(Y) %>% .[,1],
  ntree = 1000,
  seed = 11
)

Y0 <- predict(forest.control, newdata = tra %>% dplyr::select(-Tr,-Y))
Y1 <- predict(forest.treat, newdata = tra %>% dplyr::select(-Tr,-Y))

tra$Y <- ifelse(tra$Tr, Y1,Y0)
tra$Tau <- Y1 - Y0

saveRDS(object = tra, file = "data/tra_Test.RDS")

# Save both base learners
saveForestry(forest.control, filename = "data/tra_forest_control.Rda")
saveForestry(forest.treat, filename = "data/tra_forest_treat.Rda")
