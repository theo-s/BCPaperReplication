library(bartMachine)
library(cfcausal)
library(causalToolbox)
library(Rforestry)
library(grf)
library(dplyr)
library(bartCause)

#### Get parameters
if (!interactive()){
  suppressPackageStartupMessages(library(argparse))

  parser <- ArgumentParser()

  parser$add_argument("--snr", type = "integer", default = 1, help = "Signal to Noise Ratio")
  parser$add_argument("--B", type = "integer", default = 0, help = "Bootstrap samples")
  parser$add_argument("--seed", type = "double", default = 1, help = "rng seed")
  parser$add_argument("--ex", type = "integer", default = 1, help = "Experiment #")
  parser$add_argument("--es", type = "character", default = "slearner_none", help = "Estimator Name")
  parser$add_argument("--f", type = "integer", default = 1, help = "indicator of if one should overwrite old results")

  args <- parser$parse_args()

  n <- args$n
  B <- args$B
  seed <- args$seed
  experiment_num <- args$ex
  estimator <- args$es
  snr <- args$snr
  force <- args$f
} else {

  B <- 1000
  seed <- 1
  snr <- 1
  experiment_num <- 1
  estimator <- "slearner_none"
  force <- 1
}

source("code/define_estimators.R")
source("code/data_inspired_sims/define_experiments.R")

es_names <- names(estimator_list)
data_names <- names(experiment_list)

exp_names <- list("Ex1" = "ACIC 2018 Data", "Ex2" = "GOTV Data", "Ex3" = "Transphobia data")

msg <- paste0("Running experiment: ", exp_names[[data_names[[experiment_num]]]],
              " with B: ", B, " SNR: ", snr, " seed: ", seed," and estimator: ",
              estimator)
print(msg)

# File to save results in
filename <- paste0("code/data_inspired_sims/raw_data/",
                   "exp",experiment_num,"SNR",snr,"seed",seed,"es",estimator,".RDS")

# If we are running force = true or the file doesn't exist yet
if (!file.exists(filename) || (force==1)) {
  print("Running Sim")
  data_func <- experiment_list[[data_names[[experiment_num]]]]
  es_func <- estimator_list[[estimator]]
  generated_data <- data_func(seed = seed,
                              snr = snr)

  train_data <- generated_data$Train %>% dplyr::select(-Tau)
  test_data <- generated_data$Test %>% dplyr::select( -Y, -Tr)

  print(names(train_data))
  print(names(test_data))

  es_cate <- es_func(X = train_data %>% dplyr::select(-Tr,-Y),
                     T = train_data %>% dplyr::select(Tr) %>% .[,1],
                     Y = train_data %>% dplyr::select(Y) %>% .[,1],
                     Xtest = test_data %>% dplyr::select(-Tau),
                     B=B)

  nrow( test_data %>% dplyr::select(-Tau))

  coverage <- ifelse(es_cate[["tau"]]$X5. <= test_data$Tau,
                     ifelse(es_cate[["tau"]]$X95. >= test_data$Tau,
                            1,0)
                     ,0)

  result <- list("Pred" = es_cate[["preds"]],
                 "CI" = es_cate[["tau"]],
                 "Estimator" = estimator,
                 "Experiment" = experiment_num,
                 "cov" = mean(coverage))

  saveRDS(object = result, file = filename)
}


