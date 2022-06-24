library(ggplot2)
library(dplyr)
library(Rforestry)

packageVersion("Rforestry")

set.seed(2328)

social <- readRDS("data/gotvSMALL.RDS")

# Add cumulative voting features + rename some covariates
social$CVH <- apply(social[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == 1, 1, sum)
social$CGVH <- apply(social[, c("g2000", "g2002")] == 1, 1, sum)
social$CPVH <- apply(social[, c("p2000", "p2002", "p2004")] == 1, 1, sum)
colnames(social) <- c("sex","g2000","g2002","p2000","p2002","p2004","age","Tr",
                      "Y","CVH","CGVH","CPVH")

features <- social %>%
  dplyr::select(-Tr, -Y)

outcome <- social$Y
Tr <- social$Tr

# Now train S Learner ==========================================================
S_RF <- forestry(x = cbind(features, Tr),
                 y = outcome,
                 scale = FALSE,
                 seed = 101,
                 OOBhonest = TRUE)


# Estimate the CATE using standard and corrected predictions ===================
pred_cate <- predict(S_RF, newdata = cbind(features, Tr = 1), aggregation = "oob") -
  predict(S_RF, newdata = cbind(features, Tr = 0), aggergation = "oob")


pred_cate_corrected <-  Rforestry::correctedPredict(S_RF,
                                                    cbind(features, Tr = 1),
                                                    #nrounds = 1,
                                                    linear = TRUE,
                                                    observations = which(Tr==1)) -
  Rforestry::correctedPredict(S_RF,
                              cbind(features, Tr = 0),
                              #nrounds = 1,
                              linear = TRUE,
                              observations = which(Tr==0))

# Analysis =====================================================================
mean(pred_cate_corrected)
mean(pred_cate)

pred_diffs <- pred_cate_corrected - pred_cate

data.frame(PredictedCate = pred_diffs) %>%
  ggplot(aes(x = PredictedCate))+
  geom_histogram(bins = 150)+
  geom_vline(xintercept = mean(pred_diffs), color = "blue")+
  theme_classic()+
  labs(x = "CATE (Debiased) - CATE (standard)", y = "Number of Potential Voters")

# Now compare the cost of the campaigns
# Suppose a vote is worth $89, so we contact any voter with a CATE > .7%


length(which(pred_cate>.007)) / length(pred_cate)
length(which(pred_cate_corrected>.007)) / length(pred_cate)

# Compare the total cost of the two campaigns
length(which(pred_cate>.007))*.625
length(which(pred_cate_corrected>.007))*.625


# Using the two CATE estimates, how many voters do we expect to pick up
# if we send a mailer to each voter with a positive payoff
sum(pred_cate[pred_cate>.007])
sum(pred_cate_corrected[pred_cate_corrected > .007])


# Compare the cost per vote
length(which(pred_cate>.007)) * .625 / sum(pred_cate[pred_cate > .007])
length(which(pred_cate_corrected>.007)) * .625  / sum(pred_cate_corrected[pred_cate_corrected > .007])
