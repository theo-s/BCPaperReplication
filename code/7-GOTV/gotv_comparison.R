library(ggplot2)
library(dplyr)
library(Rforestry)

set.seed(2328)

# Read in data and keep the synthetic Tau that we use in other experiments
# Sample the training data
n <- 40e3
snr <- .5
test_data <- readRDS("data/gotv_Test.RDS")
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

full_data <- (list("Train" = train_data, "Test" = test_data))


# Train the S Learner and get the predicted CATE using standard S Learner ======
S_RF <- forestry(x = full_data$Train %>% select(-Y, -Tau),
                 y = full_data$Train$Y,
                 ntree = 2000,
                 seed = 2328,
                 OOBhonest = TRUE)

pred_cate <- predict(S_RF, newdata = cbind(full_data$Test %>% select(-Y,-Tr,-Tau), Tr = 1)) -
  predict(S_RF, newdata = cbind(full_data$Test %>% select(-Y,-Tr,-Tau), Tr = 0))


# Estimate corrected CATE with bias corrected S Learner
pred_cate_corrected <-  Rforestry::correctedPredict(S_RF,
                                                    cbind(full_data$Test %>% select(-Y,-Tr,-Tau), Tr = 1),
                                                    #nrounds = 1,
                                                    linear = TRUE,
                                                    observations = which(full_data$Train$Tr==1)) -
                        Rforestry::correctedPredict(S_RF,
                                                    cbind(full_data$Test %>% select(-Y,-Tr,-Tau), Tr = 0),
                                                    #nrounds = 1,
                                                    linear = TRUE,
                                                    observations = which(full_data$Train$Tr==0))



# compare RMSE
sqrt(mean((pred_cate_corrected - full_data$Test$Tau)**2))
sqrt(mean((pred_cate - full_data$Test$Tau)**2))


mean(pred_cate_corrected)
mean(pred_cate)
mean(full_data$Test$Tau)

data.frame(PredictedCate = pred_cate) %>%
  ggplot(aes(x = PredictedCate))+
  geom_histogram(bins = 150)+
  xlim(c(-.07,.12))+
  geom_vline(xintercept = mean(pred_cate), color = "blue")+
  theme_classic()+
  labs(x = "Estimated CATE", y = "Number of Potential Voters")

ggsave(filename = "figures/gotv_cate.pdf", height = 4, width = 6)

data.frame(PredictedCate = pred_cate_corrected) %>%
  ggplot(aes(x = PredictedCate))+
  geom_histogram(bins = 150)+
  xlim(c(-.07,.12))+
  geom_vline(xintercept = mean(pred_cate_corrected), color = "blue")+
  theme_classic()+
  labs(x = "Estimated CATE (debiased)", y = "Number of Potential Voters")
ggsave(filename = "figures/gotv_cate_debiased.pdf", height = 4, width = 6)

data.frame(PredictedCate = full_data$Test$Tau) %>%
  ggplot(aes(x = PredictedCate))+
  geom_histogram(bins = 150)+
  xlim(c(-.07,.12))+
  geom_vline(xintercept = mean(full_data$Test$Tau), color = "blue")+
  theme_classic()+
  labs(x = "True CATE", y = "Number of Potential Voters")

ggsave(filename = "figures/gotv_cate_true.pdf", height = 4, width = 6)
# ggsave(filename = "figures/gotv_cate.pdf", height = 4, width = 6)

# Costs for standard CATE estimates ============================================
# Cost of a letter stamp: $.58 for now don't account for labor costs.

# For Voters with a CATE > 1% (43% of potential voters), we need the votes to be worth .58/.01 = ~ $58
length(which(pred_cate >= .01))/length(pred_cate)


# so for Voters with a CATE > 1.5% (20% of potential voters), we need the votes to be worth .58/.015 = ~ $39
length(which(pred_cate >= .02))/length(pred_cate)


# For Voters with a CATE > 2% (9% of potential voters), we need the votes to be worth .58/.02 = ~ $29
length(which(pred_cate >= .05))/length(pred_cate)

length(which(pred_cate < 0))/length(pred_cate)



# Costs for debiased CATE estimates ============================================

# For Voters with a CATE > 1% (43% of potential voters), we need the votes to be worth .58/.01 = ~ $58
length(which(pred_cate_corrected >= .01))/length(pred_cate_corrected)


# so for Voters with a CATE > 1.5% (20% of potential voters), we need the votes to be worth .58/.015 = ~ $39
length(which(pred_cate_corrected >= .02))/length(pred_cate_corrected)


# For Voters with a CATE > 2% (9% of potential voters), we need the votes to be worth .58/.02 = ~ $29
length(which(pred_cate_corrected >= .05))/length(pred_cate_corrected)

length(which(pred_cate_corrected < 0))/length(pred_cate_corrected)


# True CATE Distribution =======================================================

# For Voters with a CATE > 1% (43% of potential voters), we need the votes to be worth .58/.01 = ~ $58
length(which(full_data$Test$Tau >= .01))/length(full_data$Test$Tau)


# so for Voters with a CATE > 1.5% (20% of potential voters), we need the votes to be worth .58/.015 = ~ $39
length(which(full_data$Test$Tau >= .02))/length(full_data$Test$Tau)


# For Voters with a CATE > 2% (9% of potential voters), we need the votes to be worth .58/.02 = ~ $29
length(which(full_data$Test$Tau >= .05))/length(full_data$Test$Tau)

length(which(full_data$Test$Tau < 0))/length(full_data$Test$Tau)


