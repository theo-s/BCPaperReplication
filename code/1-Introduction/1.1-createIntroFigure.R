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
y_test <- sin(x_test[,1]*2*pi)
data_test <- data.frame(x_test)
data_test$y <- y_test

all_data <- data.frame(x = x_test[,1],
                       Truth = y_test)

for (mtry in c(10,5,3,1)) {


  n_reps <- 100

  all_preds <- matrix(ncol = n_reps, nrow = 500)
  for (iter in 1:n_reps) {
    set.seed(iter)

    x_train <- matrix(runif(n*p), ncol = p)
    y_train <- sin(x_train[,1]*2*pi)
    data <- data.frame(x_train)
    data$y <- y_train

    rf <- forestry(x = data[,-ncol(data)],
                   y = data[,ncol(data)] + rnorm(n = 500, sd = .5*sd(data[,ncol(data)])),
                   mtry = mtry,
                   seed = iter,
                   OOBhonest = TRUE)

    preds <- predict(rf, newdata = x_test)
    all_preds[,iter] <- preds
  }

  mean_preds <- apply(all_preds, MARGIN = 1, FUN = mean)

  all_data <- cbind(all_data, mean_preds)
}

colnames(all_data) <- c("x", "Truth","mtry=1","mtry=.5","mtry=.3","mtry=.1")

# Now see the RF fit
all_data %>%
  melt(id = "x") %>%
  dplyr::rename(Legend = variable, Truth = value) %>%
  mutate(`mtry Fraction` = as.character(Legend)) %>%
  ggplot(aes(x = x, y = Truth, color = `mtry Fraction`, linetype = `mtry Fraction`,
             size = `mtry Fraction`, alpha = `mtry Fraction`)) +
  geom_line() +
  scale_linetype_manual(values = c("Truth" = "dashed",
                                   "mtry=1" = "solid",
                                   "mtry=.5" = "solid",
                                   "mtry=.3" = "solid",
                                   "mtry=.1" = "solid")) +
  scale_size_manual(values = c("Truth" = .9,
                               "mtry=1" = .7,
                               "mtry=.5" = .7,
                               "mtry=.3" = .7,
                               "mtry=.1" = .7)) +
  scale_color_manual(values = c("Truth" = "black",
                                "mtry=1" = "steelblue4",
                                "mtry=.5" = "steelblue3",
                                "mtry=.3" = "steelblue2",
                                "mtry=.1" = "steelblue1")) +
  # scale_color_brewer()+
  scale_alpha_manual(values = c("Truth" = 1,
                                "mtry=1"= .8,
                                "mtry=.5" = .8,
                                "mtry=.3" = .8,
                                "mtry=.1" = .8)) +
  xlab(label = "X1")+
  ylab(label = "Outcome")+
  theme_classic()

ggsave(file = paste0("~/Dropbox/BCPaperReplication/figures/intro_mtry.pdf"), width = 5.5, height = 3,
       dpi = 800)

