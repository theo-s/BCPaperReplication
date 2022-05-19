library(tidyverse)
library(dplyr)

file = "Exp1x_rf_n1000_d10_seed1.RData"#"Exp4x_rf_n1000_d100_seed99.RData"

get_exp <- function(filename) {
  if (length(grep("Exp4", file)) == 1) {
    return(4)
  } else if (length(grep("Exp3", file)) == 1) {
    return(3)
  } else if (length(grep("Exp2", file)) == 1) {
    return(2)
  } else if (length(grep("Exp1", file)) == 1) {
    return(1)
  } else {
    return(NA)
  }
}

get_dimension <- function(filename) {
  if (length(grep("d100", file)) == 1) {
    return(100)
  } else if (length(grep("d10", file)) == 1) {
    return(10)
  } else {
    return(NA)
  }
}

results <- data.frame(matrix( nrow = 1, ncol = 5))
colnames(results) <- c("method", "cr","len","exp","d")

for (file in dir("code/simulated_sims/xlearner_results")) {
  print(file)
  load(paste0("code/simulated_sims/xlearner_results/",file))
  print(res$tau)
  experiment = get_exp(filename = file)
  dim = get_dimension(filename = file)
  if (any(is.na(c(experiment, dim)))) {
    next
  }

  results <- rbind(results, c(res$tau$method, res$tau$cr, res$tau$len, experiment, dim ))
}

results %>%
  filter(method == "x_rf") -> xl_results

# xl_results$method <- rep("X-Learner (debiased rf)", nrow(xl_results))

# Save results
save(xl_results, file = "results/simul_synthetic_results_xlearner.RData")










