source("define_experiments_breiman.R")

set.seed(1)
n <- 1000
for (i in 1:length(names(experiment_list))) {
  name <- names(experiment_list)[i]
  print(name)
  func <- experiment_list[[i]][[1]]
  data <- func(n)
  saveRDS(data, file = paste0("data/",name,".RDS"))
}

