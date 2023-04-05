library(xtable)
library(dplyr)
library(ggplot2)
library(reshape)
library(tidyr)
library(gridExtra)
library(cowplot)
source("code/prediction_sims/define_experiments_breiman.R")
exp <- 1

gplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

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
                    Bias = unlist(all_bias),
                    SNR = snr))
}


results <- data.frame(matrix(NA,ncol=5,nrow=0))
colnames(results) <- c("Exp", "Es", "Var", "Bias","SNR")

for (i in 1:6) {
  for (snr_i in c(3,2,1,.5,.3)) {
    cur_res <- get_results(exp=i, snr = snr_i)
    results <- rbind(results,cur_res)
  }
}

colnames(results) <- c("Experiment", "Estimator", "SE","|Bias|","SNR")

#results[,2] <- rep(c("None","linear BC", "linear + nonlinear BC"),max(results[,1]))

# Make Xtable tables
results_bias <- results[,c(1,2,4,5)]
results_var <- results[,c(1,2,3,5)]

results_bias %>%
  pivot_wider(names_from = "Estimator",values_from = "|Bias|") -> results_bias

results_var %>%
  pivot_wider(names_from = "Estimator",values_from = "SE") -> results_var

xtable(results_bias, caption = "|Bias| for estimators across all experiments")
xtable(results_var, caption = "SE for estimators across all experiments")

level_order <- c("std.none", "hon.none", "std.lin",
                 "hon.lin", "std.rflin","hon.rflin") 

results$SNR <- ifelse(results$SNR == .5,
                           0, 
                           ifelse(results$SNR == .3,
                                      -1, results$SNR))

# Make figures for bias 
plots <- list()
for (exp in 1:6) {
  results %>%
    mutate(honesty = Estimator %in% c("hon.none","hon.lin","hon.rflin")) %>% 
    filter(Experiment == exp) %>%
    dplyr::select(-SE) %>%
    arrange(-`|Bias|`) %>%
    melt(id = c("Estimator","Experiment","SNR","honesty")) %>%
    dplyr::select(-Experiment) %>%
    # filter(variable == "|Bias|") %>%
    ggplot(aes(fill = variable, y = value, x = SNR, 
               color = factor(Estimator, level=level_order),
               linetype = factor(Estimator, level=level_order)))+
    geom_line(linewidth = 1)+
    scale_linetype_manual(values = rep(c("solid", "dashed"), 3)) +
    scale_color_manual(values = c(rep(c("black"),2),
                                  rep(c("blue"),2),
                                  rep(c("green"),2)))+
    # facet_wrap(~Experiment)+
    labs(y = "", x = "")+
    theme_classic()+
    ggeasy::easy_add_legend_title("Estimator")+
    #ggeasy::easy_remove_legend()+
    scale_x_continuous(breaks=c(-1:3),labels=c('SNR = .3', 
                                               'SNR = .5', 
                                               'SNR = 1', 
                                               'SNR = 2',
                                               'SNR = 3'))+
    ggeasy::easy_rotate_x_labels()+
    ggtitle(paste0("Experiment  ",exp)) -> plot
  legend <- get_legend(plot)
  plots[[exp]] <- plot
}

grid.arrange(plots[[2]]+theme(legend.position="none")+labs(y = "|Bias|", x = ""), 
             plots[[3]]+ theme(legend.position="none"),
             plots[[4]]+ theme(legend.position="none"),
             plots[[5]]+ theme(legend.position="none"),
             plots[[6]]+ theme(legend.position="none"),legend, nrow = 1) -> plot_final

ggsave(plot = plot_final, filename = paste0("figures/prediction_snr_bias_summary.pdf"), height = 4,width = 13)




# Make figures for Standard error
plots <- list()
for (exp in 1:6) {
  results %>%
    mutate(honesty = Estimator %in% c("hon.none","hon.lin","hon.rflin")) %>% 
    filter(Experiment == exp) %>%
    dplyr::select(-`|Bias|`) %>%
    arrange(-SE) %>%
    melt(id = c("Estimator","Experiment","SNR","honesty")) %>%
    dplyr::select(-Experiment) %>%
    # filter(variable == "|Bias|") %>%
    ggplot(aes(fill = variable, y = value, x = SNR, 
               color = factor(Estimator, level=level_order),
               linetype = factor(Estimator, level=level_order)))+
    geom_line(linewidth = 1)+
    scale_linetype_manual(values = rep(c("solid", "dashed"), 3)) +
    scale_color_manual(values = c(rep(c("black"),2),
                                  rep(c("blue"),2),
                                  rep(c("green"),2)))+
    # facet_wrap(~Experiment)+
    labs(y = "", x = "")+
    theme_classic()+
    ggeasy::easy_add_legend_title("Estimator")+
    #ggeasy::easy_remove_legend()+
    scale_x_continuous(breaks=c(-1:3),labels=c('SNR = .3', 
                                               'SNR = .5', 
                                               'SNR = 1', 
                                               'SNR = 2',
                                               'SNR = 3'))+
    ggeasy::easy_rotate_x_labels()+
    ggtitle(paste0("Experiment  ",exp)) -> plot
  legend <- get_legend(plot)
  plots[[exp]] <- plot
}

grid.arrange(plots[[2]]+theme(legend.position="none")+labs(y = "SE", x = ""), 
             plots[[3]]+ theme(legend.position="none"),
             plots[[4]]+ theme(legend.position="none"),
             plots[[5]]+ theme(legend.position="none"),
             plots[[6]]+ theme(legend.position="none"),legend, nrow = 1) -> plot_final

ggsave(plot = plot_final, filename = paste0("figures/prediction_snr_se_summary.pdf"), height = 4,width = 13)

