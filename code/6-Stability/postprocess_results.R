library(xtable)
library(dplyr)
library(reshape)
library(tidyr)

source("code/prediction_sims/define_experiments_breiman.R")
source("code/6-Stability/define_stability_estimators.R")
method_list = expand.grid(c("none","rf","xgboost","bart"),
                          c("none","ols","loess","spline"))
exp <- 1

get_results <- function(exp = 1) {
  # Read in the data
  data <- readRDS(paste0("code/prediction_sims/data/Exp",exp,".RDS"))

  preds_list <- list()
  for (i in 1:nrow(method_list)) {
    preds_list[[i]] <- matrix(nrow = 100, ncol = 1e3)
  }

  for (seed in  1:100) {
    # print(seed)
    # Read in the true outcomes
    res <- readRDS(paste0("code/6-Stability/results/Exp",exp,"seed",seed,".RDS"))

    # Save the preds
    for (i in 1:nrow(method_list)) {
      preds_list[[i]][seed,] <- res[[i]] %>% as.numeric()
    }
  }

  # Get variance of preds
  variances <- list()
  for (i in 1:nrow(method_list)) {
    variances[[i]] <-  mean(apply(preds_list[[i]], MARGIN = 2, FUN = sd, na.rm = TRUE), na.rm = TRUE)
  }

  bias_list <- list()
  for (i in 1:nrow(method_list)) {
    bias_list[[i]] <-  mean(abs(apply(preds_list[[i]], MARGIN = 2, FUN = mean, na.rm = TRUE) - data$y_true), na.rm = TRUE)
  }

  return(data.frame(Exp = rep(exp, nrow(method_list)),
                    Es = apply(method_list,1,function(x){return(paste0(x[1],".",x[2]))}),
                    Var = unlist(variances),
                    Bias = unlist(bias_list)))
}


results <- data.frame(matrix(NA,ncol=4,nrow=0))
colnames(results) <- c("Exp", "Es", "Var", "Bias")

for (i in 1:length(experiment_list)) {
  cur_res <- get_results(i)
  results <- rbind(results,cur_res)
}

results$Es <- ifelse(results$Es == "none.none", "no correction",results$Es)

colnames(results) <- c("Experiment", "Estimator", "SE","|Bias|")



for (exp in 1:length(experiment_list)) {
  results %>%
    filter(Experiment == exp) %>%
    select(-SE) %>%
    arrange(-`|Bias|`) %>%
    melt(id = c("Estimator","Experiment")) %>%
    dplyr::select(-Experiment) -> cur_data
    cur_data$Estimator <- factor(cur_data$Estimator, levels = cur_data$Estimator[1:16])
    cur_data %>%
    ggplot(aes(fill = variable, y = value, x = Estimator))+
    geom_point()+
    labs(y = "|Bias|", x = "")+
    theme_classic()+
    ggeasy::easy_rotate_x_labels()+
    #ggeasy::easy_add_legend_title("Error Term")+
    ggeasy::easy_remove_legend()+
    ggtitle(paste0("Experiment ",exp))+
    scale_fill_manual(values = c("steelblue3","steelblue4"))
  ggsave(paste0("figures/stability_experiment",exp,".pdf"), height = 4,width = 4)
}

# Now tabulate results
results %>%
  dplyr::select(-SE) %>%
  group_by(Experiment) %>%
  arrange(-`|Bias|`) %>%
  mutate(pct_change = (`|Bias|`/`|Bias|`[Estimator=="no correction"] - 1) * 100) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("Estimator"), names_from = c("Experiment"),values_from = c("pct_change")) -> pct_df

print_df <- pct_df[,c(1,5,4,2,7,3,6)]
colnames(print_df) <- c("Estimator","Experiment 1","Experiment 2","Experiment 3",
                        "Experiment 4", "Experiment 5","Experiment 6")

xtable(print_df, caption = "Percentage decrease in average absolute bias for each estimator and experiment", digits = 1)
