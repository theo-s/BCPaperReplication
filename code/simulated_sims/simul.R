library(dplyr)

#### Get parameters
if (!interactive()){
  suppressPackageStartupMessages(library(argparse))

  parser <- ArgumentParser()

  parser$add_argument("--n", type = "integer", default = 1000, help = "Sample Size")
  parser$add_argument("--d", type = "integer", default = 10, help = "Dimension")
  parser$add_argument("--B", type = "integer", default = 0, help = "Bootstrap samples")
  parser$add_argument("--seed", type = "double", default = 1, help = "rng seed")
  parser$add_argument("--ex", type = "integer", default = 1, help = "Experiment #")
  parser$add_argument("--es", type = "integer", default = 1, help = "Estimator #")

  args <- parser$parse_args()

  n <- args$n
  d <- args$d
  B <- args$B
  seed <- args$seed
  experiment_num <- args$ex
  estimator_num <- args$es
} else {
  n <- 100
  d <- 10
  B <- 50
  seed <- 3425
  experiment_num <- 1
  estimator_num <- 4
}

source("code/simulated_sims/utils.R")
source("code/define_helpers.R")
source("code/simulated_sims/define_experiments.R")
source("code/define_estimators.R")

print(paste0("Running experiment ", experiment_num, " with B = ", B, " D = ", d, "seed = ", seed))

#for (estimator_num in 1:length(estimator_list)) {
 
	ntest <- 10000
	set.seed(seed)
	

	if (experiment_num > length(experiment_list) ||
    		estimator_num > length(estimator_list)) {
  		stop("Estimator num or experiment num is not found")
	}

	# Get the current estimator
	cur_es <- estimator_list[[estimator_num]]
	cur_expr <- experiment_list[[experiment_num]]
	es_name <- names(estimator_list)[estimator_num]
	ex_name <- names(experiment_list)[experiment_num]

	# Read in all the experiment functions
	Xfun <- cur_expr[[1]]
	taufun <- cur_expr[[2]]
	sdfun <- cur_expr[[3]]
	psfun <- cur_expr[[4]]
	errdist <- cur_expr[[5]]

	filename <- paste0("code/simulated_sims/results/",
                   ex_name ,
                   es_name ,
                   "_n", n,
                   "_d", d,
                   "_seed", seed,
                   ".RData")
	res <- list()

	# Run the experiment
	res <- Cf_expr(n = n, d = d, ntest = ntest, Xfun = Xfun, taufun = taufun,
               sdfun = sdfun, psfun = psfun, errdist = errdist, es_name = es_name,
               estimator = cur_es, seed = seed, B = B)

	# Save and print out results
	print(res$tau)
	save(res, file = filename)
#}

