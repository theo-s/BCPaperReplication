library(tidyverse)
library(dplyr)

setwd("~/Dropbox/BCPaperReplication")


filepath = "~/Dropbox/BCPaperReplication/code/data_inspired_sims/raw_data/raw_data/"

res_tau <- list()

for (i in 1:length(dir(filepath))) {
  file = dir(filepath)[i]
  print(file)

  if ((substr(file,1,4) == "exp3") && ((length(grep("rfnone", file))==1) || (length(grep("rfbc", file)) == 1)
      || (length(grep("causalForest", file)) == 1) || (length(grep("bart", file)) == 1))) {

    res <- readRDS(paste0("code/data_inspired_sims/raw_data/raw_data/",file))

    snr  <- strsplit(strsplit(file, "SNR")[[1]][2], "seed")[[1]][1] %>% as.numeric()

    len = mean(res$CI[,2] - res$CI[,1])
    res_tau[[i]] <- cbind(res$cov, len, res$Experiment, res$Estimator, snr)
    print(res_tau[[i]])
  } else {
    next
  }
}

res_tau <- do.call(rbind, res_tau)
colnames(res_tau) <- c("cr","len","exp","method","snr")
final_res <- data.frame( cov = as.numeric(res_tau[,1]),
                         len = as.numeric(res_tau[,2]),
                         exp = as.numeric(res_tau[,3]),
                         method = (res_tau[,4]),
                         snr = as.numeric(res_tau[,5]))

save(final_res, file = "results/transphobia_coverage.RData")
