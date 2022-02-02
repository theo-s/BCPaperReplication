library(ggplot2)
library(dplyr)
library(Rforestry)
library(reshape2)

# Generate Data
n <- 500
p <- 5
x <- matrix(runif(n*p), ncol = p)
y <- sin(x[,1]*2*pi)
data <- data.frame(x)
data$y <- y

rf <- forestry(x = data[,-ncol(data)],
               y = data[,ncol(data)] + rnorm(n = 500, sd = .5*sd(data[,ncol(data)])),
               mtry = 1)

preds <- predict(rf, newdata = data[,-ncol(data)], aggregation = "oob")

all_data <- data.frame(x = x[,1],
                       Truth = y,
                       Predictions = preds)

# Now see the BART fit
all_data %>%
  melt(id = "x") %>%
  dplyr::rename(Legend = variable, Truth = value) %>%
  mutate(Legend = as.character(Legend)) %>%
  ggplot(aes(x = x, y = Truth, color = Legend, linetype = Legend,
             size = Legend, alpha = Legend)) +
  geom_line() +
  scale_linetype_manual(values = c("Truth" = "dashed",
                                   "Predictions" = "solid")) +
  scale_size_manual(values = c("Truth" = .5,
                               "Predictions" = .7)) +
  scale_color_manual(values = c("Truth" = "black",
                                "Predictions" = "blue")) +
  scale_alpha_manual(values = c("Truth" = 1,
                                "Predictions" = .4)) +
  xlab(label = "X1")+
  ylab(label = "Outcome")+
  theme_classic()

ggsave()
