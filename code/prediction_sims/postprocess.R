library(xtable)
library(dplyr)
library(reshape)
library(tidyr)
library(gridExtra)
library(cowplot)
source("code/prediction_sims/define_experiments_breiman.R")
exp <- 1

get_results <- function(exp = 1,snr=0) {
  # Read in the data
  data <- readRDS(paste0("code/prediction_sims/data/Exp",exp,".RDS"))

  all_preds <- list()
  for (count in 1:6) {
    all_preds[[count]] = matrix(nrow = 100, ncol = 1e3)
  }

  for (seed in  1:100) {
    # print(seed)
    # Read in the true outcomes
    res <- readRDS(paste0("code/prediction_sims/results/Exp",exp,"seed",seed,"snr",snr,".RDS"))

    # Save the preds
    for (count in 1:6) {
      all_preds[[count]][seed,] = res[,count]
    }
  }


  # Get variance of preds
  all_vars <- list()
  for (count in 1:6) {
    all_vars[[count]] = mean(apply(all_preds[[count]], MARGIN = 2, FUN = sd))
  }

  # Get biases
  all_bias <- list()
  for (count in 1:6) {
    all_bias[[count]] = mean(abs(apply(all_preds[[count]], MARGIN = 2, FUN = mean) - data$y_true))
  }

  return(data.frame(Exp = rep(exp, 6),
                    Es = c("hon.none","hon.lin","hon.rflin","std.none","std.lin","std.rflin"),
                    Var = unlist(all_vars),
                    Bias = unlist(all_bias)))
}


results <- data.frame(matrix(NA,ncol=4,nrow=0))
colnames(results) <- c("Exp", "Es", "Var", "Bias")

for (i in 1:6) {
  cur_res <- get_results(i)
  results <- rbind(results,cur_res)
}

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

level_order <- c("std.none", "hon.none", "std.lin",
                 "hon.lin", "std.rflin","hon.rflin") 

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
    ggplot(aes(fill = variable, y = value, x = factor(Estimator, level=level_order),alpha = Estimator %in% c("hon.none",
                                                                                                             "hon.lin",
                                                                                                             "hon.rflin")))+
    geom_point(size=2)+
    scale_alpha_manual(values = c(1, 0.5), guide = FALSE)+
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

