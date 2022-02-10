library(tidyverse)
library(dplyr)

setwd("~/Dropbox/BCPaperReplication")

# Try making them unique file names ==============================

for (file in dir("results/cov_results/Exp1")) {
  print(file)
  load(paste0("results/cov_results/Exp1/",file))
  res[["exp"]] <- 1
  save(res, file = paste0("results/raw_data_cluster/exp1_",file))
}

for (file in dir("results/cov_results/Exp2")) {
  print(file)
  load(paste0("results/cov_results/Exp2/",file))
  res[["exp"]] <- 2
  save(res, file = paste0("results/raw_data_cluster/exp2_",file))
}

for (file in dir("results/cov_results/Exp3")) {
  print(file)
  load(paste0("results/cov_results/Exp3/",file))
  res[["exp"]] <- 3
  save(res, file = paste0("results/raw_data_cluster/exp3_",file))
}

for (file in dir("results/cov_results/Exp4")) {
  print(file)
  load(paste0("results/cov_results/Exp4/",file))
  res[["exp"]] <- 4
  save(res, file = paste0("results/raw_data_cluster/exp4_",file))
}

# Now move all results to "~/Dropbox/BCPaperReplication/results/raw_data_cluster"

filepath = "~/Dropbox/BCPaperReplication/results/raw_data_cluster"

res_tau <- list()

for (i in 1:length(dir(filepath))) {
  file = dir(filepath)[i]
  print(file)

  if (!(substr(file, 1,3) == "exp")) {
    next
  } else {
    load(paste0("results/raw_data_cluster/",file))
    if (!identical(grep("d100", file),integer(0))) {
      res_tau[[i]] <- cbind(res$tau, res$exp,d = 100)
      print(res$exp)
    } else if (!identical(grep("d10", file),integer(0))) {
      res_tau[[i]] <- cbind(res$tau, res$exp,d = 10)
      print(res$exp)
    }
  }
}

res_tau <- do.call(rbind, res_tau)
names(res_tau) <- c("method","cr","len","exp","d")

res <- list(tau = res_tau)
save(res, file = "results/simul_synthetic_results.RData")
