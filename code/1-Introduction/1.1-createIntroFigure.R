library(ggplot2)
library(dplyr)
library(Rforestry)
library(reshape2)

# Run this 100 times

# Generate Data
set.seed(23234243)
n <- 500
p <- 10
x_test <- matrix(runif(n*p), ncol = p)
x_test[,1] <- 2*x_test[,1]
x_test[,2] <- .9*x_test[,2]
y_test <- 2 * x_test[,1] + 3*x_test[,2] + .1 *x_test[,3]#sin(x_test[,1]*2*pi)
data_test <- data.frame(x_test)
data_test$y <- y_test

all_data <- data.frame(x = x_test[,1],
                       Truth = y_test)

source("code/6-Stability/define_stability_estimators.R")

for (mtry in c(3)) {

  n_reps <- 100

  all_preds <- matrix(ncol = n_reps, nrow = 500)
  all_corrected_preds <- matrix(ncol = n_reps, nrow = 500)
  for (iter in 1:n_reps) {
    set.seed(iter)

    x_train <- matrix(runif(n*p), ncol = p)
    x_train[,1] <- 2*x_train[,1]
    x_train[,2] <- .9*x_train[,2]
    y_train <- 2*x_train[,1] + 3*x_train[,2] + .1 *x_train[,3]#sin(x_train[,1]*2*pi)
    data <- data.frame(x_train)
    data$y <- y_train

    rf <- forestry(x = data[,-ncol(data)],
                   y = data[,ncol(data)] + rnorm(n = 500, sd = 2*sd(data[,ncol(data)])),
                   mtry = mtry,
                   seed = iter,
                   OOBhonest = TRUE)

    preds <- predict(rf, newdata = x_test)

    correct.preds <- GeneralCorrectedPredict(rf,
                                             Xtest = x_test,
                                             method1 = "none",
                                             method2 = "ols")

    # correct.preds <- correctedPredict(rf,
    #                                   newdata = x_test,
    #                                   nrounds = 1,
    #                                   linear = FALSE)

    all_preds[,iter] <- preds
    all_corrected_preds[,iter] <- correct.preds
  }

  mean_preds <- apply(all_preds, MARGIN = 1, FUN = mean)
  mean_corrected_preds <- apply(all_corrected_preds, MARGIN = 1, FUN = mean)

  all_data <- cbind(all_data, mean_preds, mean_corrected_preds)
}

colnames(all_data) <- c("x", "Truth","Random Forest","Debiased Random Forest")



data.frame(Truth = all_data$Truth,
           Predictions = all_data$`Random Forest`) %>%
  ggplot(aes(x = Truth, y = Predictions))+
  geom_point()+
  xlim(min(min(all_data$Truth),min(all_data$`Random Forest`)),max(max(all_data$Truth),max(all_data$`Random Forest`)) )+
  ylim(min(min(all_data$Truth),min(all_data$`Random Forest`)),max(max(all_data$Truth),max(all_data$`Random Forest`)) )+
  geom_abline(intercept = c(0,0), slope = 1)+
  theme_classic()

# ggsave(file = paste0("~/Dropbox/BCPaperReplication/figures/simple_linear.pdf"), width = 4, height = 4,
#        dpi = 800)

data.frame(Truth = all_data$Truth,
           DebiasedPredictions = all_data$`Debiased Random Forest`) %>%
  ggplot(aes(x = Truth, y = DebiasedPredictions))+
  geom_point()+
  xlim(min(min(all_data$Truth),min(all_data$`Debiased Random Forest`)),max(max(all_data$Truth),max(all_data$`Debiased Random Forest`)) )+
  ylim(min(min(all_data$Truth),min(all_data$`Debiased Random Forest`)),max(max(all_data$Truth),max(all_data$`Debiased Random Forest`)) )+
  geom_abline(intercept = c(0,0), slope = 1)+
  theme_classic()
# ggsave(file = paste0("~/Dropbox/BCPaperReplication/figures/simple_linear_correct.pdf"), width = 4, height = 4,
#        dpi = 800)

# Now see the RF fit
all_data %>%
  melt(id = "x") %>%
  dplyr::rename(Legend = variable, Truth = value) %>%
  mutate(`Predictions` = as.character(Legend)) %>%
  ggplot(aes(x = x, y = Truth, color = `Predictions`, linetype = `Predictions`,
             size = `Predictions`, alpha = `Predictions`)) +
  geom_line() +
  scale_linetype_manual(values = c("Truth" = "dashed",
                                   "Random Forest" = "solid",
                                   "Debiased Random Forest" = "solid")) +
  scale_size_manual(values = c("Truth" = .9,
                               "Random Forest" = .7,
                               "Debiased Random Forest" = .7)) +
  scale_color_manual(values = c("Truth" = "black",
                                "Random Forest" = "steelblue1",
                                "Debiased Random Forest" = "steelblue4")) +
  # scale_color_brewer()+
  scale_alpha_manual(values = c("Truth" = 1,
                                "Random Forest" = .8,
                                "Debiased Random Forest" = .8)) +
  xlab(label = "X1")+
  ylab(label = "Outcome")+
  ggeasy::easy_remove_legend_title()+
  theme_classic()

ggsave(file = paste0("~/Dropbox/BCPaperReplication/figures/intro_mtry.pdf"), width = 5.5, height = 3,
       dpi = 800)

