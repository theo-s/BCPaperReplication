library(ggplot2)
library(dplyr)
library(Rforestry)

set.seed(2328)

# Read in data and remove the synthetic Tau that we use in other experiments
gotv_data <- readRDS(file = "data/gotv_Test.RDS")
gotv_data <- gotv_data %>% dplyr::select(-Tau)

# Train the S Learner and get the predicted CATE
S_RF <- forestry(x = gotv_data %>% select(-Y),
                 y = gotv_data$Y,
                 ntree = 2000,
                 OOBhonest = TRUE)

pred_cate <- predict(S_RF, newdata = cbind(gotv_data %>% select(-Y,-Tr), Tr = 1), aggregation = "oob") -
  predict(S_RF, newdata = cbind(gotv_data %>% select(-Y,-Tr), Tr = 0), aggregation = "oob")

data.frame(PredictedCate = pred_cate) %>%
  ggplot(aes(x = PredictedCate))+
  geom_histogram(bins = 150)+
  xlim(c(-.05,.075))+
  geom_vline(xintercept = mean(pred_cate), color = "blue")+
  theme_classic()+
  labs(x = "Estimated CATE", y = "Number of Potential Voters")


# Cost of a letter stamp: $.58 for now don't account for labor costs.

# For Voters with a CATE > 1% (43% of potential voters), we need the votes to be worth .58/.01 = ~ $58
length(which(pred_cate >= .01))/length(pred_cate)


# so for Voters with a CATE > 1.5% (20% of potential voters), we need the votes to be worth .58/.015 = ~ $39
length(which(pred_cate >= .015))/length(pred_cate)


# For Voters with a CATE > 2% (9% of potential voters), we need the votes to be worth .58/.02 = ~ $29
length(which(pred_cate >= .02))/length(pred_cate)

