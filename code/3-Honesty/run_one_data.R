

# This file contains examples for comparing the performance of RF
# with default mtry = 1/p and bagging (which is RF with mtry = 1)
# on real data.

# The RealData.RData file contains the real data used in the following
# simulations. Observations with missing values have been removed.
# The last column of each dataframe is the response.

# Source functions in the R file Performance_Comparison_functions.R.
# Load the randomForest, ggplot2 and tidyverse package.

library(Rforestry)
library(ggplot2)
library(tidyverse)

if (!interactive()){
  library(argparse)
  pars <- ArgumentParser()

  pars$add_argument("--data", type = "double", default = 1, help = "Index of data set to use")
  pars$add_argument("--reps", type = "double", default = 1, help = "Number of replications to run each snr level for")
  args <- pars$parse_args()

  datanum <- args$data
  nreps <- args$reps
} else {
  datanum <- 1
  nreps <- 25
}


# The following is a wrapper function for comparing the performance
# of RF and bagging on real datasets.
# Given a dataset, each time we run wrapper, we obtain the K-fold
# CV error of bagging and RF with various levels of noise injected
# into the response.
# This process is repeated 500 times.

wrapper <- function(nsim, dat){

  n <- dim(dat)[1]       # data size
  p <- dim(dat)[2] -1    # feature dimension
  K <- 10                # the number of folds in CV

  # ptg is the proportion alpha in page 15 of the paper.
  # Variance of the noise injected into the reponse is alpha
  # porportion of the sample variance of the original response.
  ptg <- c(0,0.01,0.05,0.1,0.25,0.5,1,2)
  sigma <- (var(dat[,p+1])*ptg)^0.5

  set.seed(nsim)
  cv.sigma <- lapply(sigma,noise_rf,dat=dat,K=K)

  if(nsim==1){
    result <- list(n=n,p=p,K=K,ptg=ptg,sigma=sigma,dat=dat,nsim=nsim,cv.sigma=cv.sigma)
  }else{
    result <- list(cv.sigma=cv.sigma)
  }

  return(result)

}

# The following is an example with the boston housing data.
# Simulations on other datasets can be carried out similarly.
load("RealData.RData")
source("Performance_Comparison_functions.R")



if (datanum == 1) {
  currentdata = abalone
  dataname = "abalone"
} else if (datanum == 2) {
  currentdata = bike
  dataname = "bike"
} else if (datanum == 3) {
  currentdata = boston
  dataname = "boston"
} else if (datanum == 4) {
  currentdata = concrete
  dataname = "concrete"
} else if (datanum == 5) {
  currentdata = cpu
  dataname = "cpu"
} else if (datanum == 6) {
  currentdata = csm
  dataname = "csm"
} else if (datanum == 7) {
  currentdata = fb
  dataname = "fb"
} else if (datanum == 8) {
  currentdata = parkinsons
  dataname = "parkinsons"
} else if (datanum == 9) {
  currentdata = servo
  dataname = "servo"
} else if (datanum == 10) {
  currentdata = solar
  dataname = "solar"
} else {
  stop("Current datanum not valid")
}


# Run wrapper_linear 500 times.
FinalResult <- lapply(1:nreps, wrapper, dat=currentdata)
filename <- paste0("results/",dataname,"-",nreps,"-reps",".RDS")
saveRDS(FinalResult, file = filename)
