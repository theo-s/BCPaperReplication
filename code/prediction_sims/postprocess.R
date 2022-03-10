library(xtable)
library(dplyr)
library(reshape)
library(tidyr)
library(gridExtra)
library(cowplot)
source("code/prediction_sims/define_experiments_breiman.R")
exp <- 1

get_results <- function(exp = 1) {
  # Read in the data
  data <- readRDS(paste0("code/prediction_sims/data/Exp",exp,".RDS"))

  preds_1 <- matrix(nrow = 100, ncol = 1e3)
  preds_2 <- matrix(nrow = 100, ncol = 1e3)
  preds_3 <- matrix(nrow = 100, ncol = 1e3)

  for (seed in  1:100) {
    # print(seed)
    # Read in the true outcomes
    res <- readRDS(paste0("code/prediction_sims/results/Exp",exp,"seed",seed,".RDS"))

    # Save the preds
    preds_1[seed,] <- res$rf
    preds_2[seed,] <- res$lin
    preds_3[seed,] <- res$rflin
  }


  # Get variance of preds
  var_1 <- mean(apply(preds_1, MARGIN = 2, FUN = sd))
  var_2 <- mean(apply(preds_2, MARGIN = 2, FUN = sd))
  var_3 <- mean(apply(preds_3, MARGIN = 2, FUN = sd))

  # Get biases
  bias_1 <- mean(abs(apply(preds_1, MARGIN = 2, FUN = mean) - data$y_true))
  bias_2 <- mean(abs(apply(preds_2, MARGIN = 2, FUN = mean) - data$y_true))
  bias_3 <- mean(abs(apply(preds_3, MARGIN = 2, FUN = mean) - data$y_true))

  return(data.frame(Exp = rep(exp, 3),
                    Es = c("none.none", "none.ols", "rf.ols"),
                    Var = c(var_1,var_2,var_3),
                    Bias = c(bias_1,bias_2,bias_3)))
}


results <- data.frame(matrix(NA,ncol=4,nrow=0))
colnames(results) <- c("Exp", "Es", "Var", "Bias")

for (i in 1:6) {
  cur_res <- get_results(i)
  results <- rbind(results,cur_res)
}
results$Es <- ifelse(results$Es == "none.none", "no correction",results$Es)
colnames(results) <- c("Experiment", "Estimator", "SE","|Bias|")

#results[,2] <- rep(c("None","linear BC", "linear + nonlinear BC"),max(results[,1]))

# Make Xtable tables
results_bias <- results[,c(1,2,4)]
results_var <- results[,c(1,2,3)]

results_bias %>%
  pivot_wider(names_from = "Estimator",values_from = "|Bias|") -> results_bias

results_var %>%
  pivot_wider(names_from = "Estimator",values_from = "SE") -> results_var

xtable(results_bias, caption = "|Bias| for estimators across all experiments")
xtable(results_var, caption = "SE for estimators across all experiments")


# Make figures for bias + Se
plots <- list()
for (exp in 1:6) {
  results %>%
    filter(Experiment == exp) %>%
    dplyr::select(-SE) %>%
    arrange(-`|Bias|`) %>%
    melt(id = c("Estimator","Experiment")) %>%
    dplyr::select(-Experiment) %>%
    # filter(variable == "|Bias|") %>%
    ggplot(aes(fill = variable, y = value, x = Estimator))+
    geom_point()+
    # facet_wrap(~Experiment)+
    labs(y = "", x = "")+
    theme_classic()+
    ggeasy::easy_rotate_x_labels()+
    ggeasy::easy_add_legend_title("Error Term")+
    ggeasy::easy_remove_legend()+
    ggtitle(paste0("Experiment  ",exp))+
    scale_fill_manual(values = c("steelblue3","steelblue4")) -> plot
  #legend <- get_legend(plot)
  plots[[exp]] <- plot
}

grid.arrange(plots[[1]]+labs(y = "|Bias|", x = ""), plots[[2]], plots[[3]],
             plots[[4]], plots[[5]], plots[[6]], nrow = 1) -> plot_final

ggsave(plot = plot_final, filename = paste0("figures/prediction_summary.pdf"), height = 4,width = 13)

