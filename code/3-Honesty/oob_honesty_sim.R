library(Rforestry)
library(grf)
library(ranger)
library(dbarts)
packageVersion("Rforestry")


# The aim of this experiment is to explore the failure of honesty in preventing
# random forest from highly overfitting a purely noisy outcome. With a simple
# data generating process, we compare several versions of honesty, and
# implementations from the Rforestry and GRF packages.
#
# ==============================================================================


# Define Experiments ===========================================================

bart_standard <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"

  f0 <- bart(x.train = X0,
             y.train = Y0,
             verbose = FALSE,
             keeptrees = TRUE)

  preds <- predict(f0, newdata = X0)
  p0 <- apply(preds, MARGIN = 2, mean)

  return(list("bart" = cor(p0, Y0)))
}

# Runs GRF and forestry and returns the correlation to the true outcome.
no_honesty <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry(y=Y0,
                 x=X0,
                 ntree=500
  )

  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = FALSE,
                               num.trees = 500,
                               sample.fraction = .75,
                               ci.group.size = 1,
                               min.node.size = 3,
                               alpha = 0)
  gp0 <- predict(g0, X0)
  p0 <- predict(f0,X0)

  # return the correlations
  return(list("forestry" = cor(p0,Y0), "GRF" = unname(cor(gp0,Y0)[1])))
}

# Splits the sample, and then returns the results of no honesty GRF and forestry
# on the test set. This is a reality check, as neither algorithm should
# know anything about the test set outcomes
no_honesty_sample_split <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry(y=Y0,
                 x=X0,
                 ntree=500
  )

  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = FALSE,
                               num.trees = 500,
                               sample.fraction = .75,
                               ci.group.size = 1,
                               min.node.size = 3,
                               alpha = 0)

  # Now create new test set
  X2 <- matrix(nrow=n, ncol=1)
  X2[,1] <- rnorm(n)
  Y2 <- rnorm(n)
  colnames(X0) <- "X0"
  gp0 <- predict(g0, X2)
  p0 <- predict(f0,X2)

  # return the correlations
  return(list("forestry" = cor(p0,Y2), "GRF" = unname(cor(gp0,Y2)[1])))
}

# Runs GRF and forestry with honesty and returns the correlation to the true outcome.
honesty <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry(y=Y0,
                 x=X0,
                 splitratio = .8,
                 ntree=500)

  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = TRUE,
                               honesty.fraction = .8,
                               honesty.prune.leaves = TRUE,
                               num.trees = 500,
                               sample.fraction = 1,
                               ci.group.size = 1,
                               min.node.size = 3,
                               alpha = 0)
  gp0 <- predict(g0, X0)
  p0 <- predict(f0,X0)

  # return the correlations
  return(list("forestry" = cor(p0,Y0), "GRF" = unname(cor(gp0,Y0)[1])))
}



# Runs GRF and forestry with honesty and returns the correlation to the true outcome.
honesty_let_empty <- function(n = 10000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry::forestry(y=Y0,
                           x=X0,
                           splitratio = .5,
                           nodesizeSpl = 10,
                           replace = TRUE,
                           sample.fraction = 1,
                           nodesizeStrictSpl = 10,
                           nodesizeStrictAvg = 1,
                           nodesizeAvg = 1,
                           ntree=500)

  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = TRUE,
                               honesty.fraction = .5,
                               honesty.prune.leaves = FALSE,
                               num.trees = 500,
                               sample.fraction = 1,
                               ci.group.size = 1,
                               min.node.size = 10,
                               alpha = 0)
  gp0 <- predict(g0, X0)
  p0 <- predict(f0,X0)

  # return the correlations
  return(list("forestry" = cor(p0,Y0), "GRF" = unname(cor(gp0,Y0)[1])))
}


# Runs GRF and forestry with OOB predictions
no_honesty_OOBpreds <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry(y=Y0,
                 x=X0,
                 ntree=500
  )

  # If I do sample.fraction = 1, is GRF still using without replacement?
  # this would be very weird. But currently it seems like OOB preds only work
  # with sample.fraction < 1.g0
  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = FALSE,
                               num.trees = 500,
                               sample.fraction = .75,
                               ci.group.size = 1,
                               min.node.size = 3,
                               alpha = 0)
  gp0 <- predict(g0, newdata = NULL)
  p0 <- predict(f0, aggregation = "oob")

  # return the correlations
  if(any(sapply(p0,is.nan)) || any(sapply(gp0,is.nan))) {
    return(list("forestry" = NA, "GRF" = NA))
  } else {
    return(list("forestry" = cor(p0,Y0), "GRF" = unname(cor(gp0,Y0)[1])))
  }
}


# Runs GRF and forestry with OOB predictions
honesty_OOBpreds <- function(n = 1000) {
  X0 <- matrix(nrow=n, ncol=1)
  X0[,1] <- rnorm(n)
  Y0 <- rnorm(n)
  #cor(X0,Y0)
  colnames(X0) <- "X0"
  # compare GRF and forestry, here we try to run with the exact same settings
  f0 <- forestry(y=Y0,
                 x=X0,
                 OOBhonest = TRUE,
                 ntree=500
  )

  # If I do sample.fraction = 1, is GRF still using without replacement?
  # this would be very weird. But currently it seems like OOB preds only work
  # with sample.fraction < 1.g0
  g0 <- grf::regression_forest(X = X0,
                               Y = Y0,
                               honesty = TRUE,
                               honesty.fraction = .5,
                               honesty.prune.leaves = FALSE,
                               num.trees = 500,
                               sample.fraction = .5,
                               ci.group.size = 1,
                               min.node.size = 3,
                               alpha = 0)

  gp0 <- predict(g0, newdata = X0)
  p0 <- predict(f0, newdata = X0, aggregation = "oob")

  # return the correlations
  if(any(sapply(p0,is.nan)) || any(sapply(gp0,is.nan))) {
    return(list("forestry" = NA, "GRF" = NA))
  } else {
    return(list("forestry" = cor(p0,Y0), "GRF" = unname(cor(gp0,Y0)[1])))
  }
}

# Now we run some experiments
reps = c(1:100)

results_rf <- data.frame(rep = reps)
results_rf$no_honesty <- NA
results_rf$honesty <- NA
results_rf$sample_split <- NA
results_rf$honesty_empty <- NA
results_rf$no_honest_oob <- NA
results_rf$honest_oob <- NA

results_grf <- data.frame(rep = reps)
results_grf$no_honesty <- NA
results_grf$honesty <- NA
results_grf$sample_split <- NA
results_grf$honesty_empty <- NA
results_grf$no_honest_oob <- NA
results_grf$honest_oob <- NA

results_bart <- data.frame(rep = reps)
results_bart$bart <- NA

for (rep_i in reps) {
  print(paste0("Rep ", rep_i))


  print("Experiment 1")
  res <- no_honesty(n = 1000)
  results_rf$no_honesty[which(results_rf$rep == rep_i)] <- res["forestry"]$forestry
  results_grf$no_honesty[which(results_grf$rep == rep_i)] <- res["GRF"]$GRF

  print("Experiment 2")
  res <- honesty(n = 1000)
  results_rf$honesty[which(results_rf$rep == rep_i)] <- res["forestry"]$forestry
  results_grf$honesty[which(results_grf$rep == rep_i)] <- res["GRF"]$GRF

  print("Experiment 3")
  res <- no_honesty_sample_split(n = 1000)
  results_rf$sample_split[which(results_rf$rep == rep_i)] <- res["forestry"]$forestry
  results_grf$sample_split[which(results_grf$rep == rep_i)] <- res["GRF"]$GRF

  #print("Experiment 4")
  #res <- honesty_let_empty(n = 10000)
  #results_rf$honesty_empty[which(results_rf$rep == rep_i)] <-res["forestry"]$forestry
  #results_grf$honesty_empty[which(results_grf$rep == rep_i)] <-res["GRF"]$GRF


  print("Experiment 5")
  res <- bart_standard(n=1000)
  results_bart$bart[which(results_rf$rep == rep_i)] <- res["bart"]

  print("Experiment 6")
  res <- no_honesty_OOBpreds(n = 1000)
  results_rf$no_honest_oob[which(results_rf$rep == rep_i)] <- res["forestry"]$forestry
  results_grf$no_honest_oob[which(results_grf$rep == rep_i)] <- res["GRF"]$GRF

  print("Experiment 7")
  res <- honesty_OOBpreds(n = 1000)
  results_rf$honest_oob[which(results_rf$rep == rep_i)] <- res["forestry"]$forestry
  results_grf$honest_oob[which(results_grf$rep == rep_i)] <- res["GRF"]$GRF

}

print(colMeans(results_grf))
print(colMeans(results_rf))
# write.csv(results_grf, file = "grf_cors.csv")
# write.csv(results_rf, file = "rf_cors.csv")

rf_results <- colMeans(results_rf)
rf_results <- rf_results[c(-1,-5,-6)]
data <- data.frame( matrix(rf_results[1:2],ncol=2,nrow=1), bart = mean(unlist(results_bart$bart)),matrix(rf_results[3:4],ncol=2,nrow=1))
colnames(data) <- c("RF No Honesty", "RF Honesty","BART", "RF Sample Split", "RF OOB Honesty")

data %>%
  melt() %>%
  dplyr::arrange(-value) %>%
  ggplot(aes(x = variable, y = value))+
  geom_point()+
  labs(x = "", y = "Estimated Correlation")+
  geom_hline(yintercept=0, linetype="dashed", color = "blue")+
  theme_classic()

ggsave(filename = "figures/oob_honesty.pdf", width = 6,height = 4)

saveRDS(results_rf, file = "results/oob_honest_res.RDS")

