library(dplyr)
## Get the coverage and the average length of confidence
## intervals CI
summary_CI <- function(target, CI){
  len <- mean(CI[, 2] - CI[, 1])
  cr <- mean(target >= CI[, 1] & target <= CI[, 2])

  return(list(cr = cr, len = len))
}

## Get the conditional coverage of confidence intervals CI
## with fx being the stratified into nstrata folds based on
## its quantiles
summary_CI_cond <- function(target, CI, fx, nstrata = 10){
  betas <- (0:nstrata) / nstrata
  qt <- quantile(fx, betas)
  qt[1] <- qt[1] - 1e-6
  qt[length(qt)] <- qt[length(qt)] + 1e-6
  strata <- cut(fx, qt)
  levels(strata) <-
    paste0("(",
           round(head(betas, -1) * 100, 0), "%",
           ", ",
           round(tail(betas, -1) * 100, 0), "%",
           "]")
  cover <- (CI[, 1] <= target & CI[, 2] >= target)
  df <- data.frame(strata = strata, cover = cover)
  df %>% group_by(strata) %>%
    summarize(cr = mean(cover)) %>%
    ungroup()
}

## The function to implement one run of the simulation study
## in Section 3.
##
## Inputs:
##   n: sample size
##   d: dimension
##   ntest: number of testing points
##   Xfun: the function to generate X
##   taufun: the function to generate E[Y(1)]
##   sdfun: the function to generate \sigma(x)
##   psfun: the function to generate e(x)
##   estimator: the estimator to use in the simulation
##   errdist: the error distribution
##   seed: the seed to run the sim with
##   B: the number of bootstrap draws for X-learner. Default to be 50 since it is slow
##   strata: variables to be stratified with "tau" for CATE
##           and "std" for \sigma(x)
##
## Outputs:
##   tau: a data.frame with coverage and other information of CATE
##   true_tau: a vector of the true test treatment effects
##   est_tau: a vector of the estimated test treatment effects
Cf_expr <- function(n, d, ntest,
                    Xfun, taufun, sdfun, psfun, errdist,
                    estimator, es_name, seed = 101, B = 50, strata = c("tau")){
  ## Generate data
  set.seed(seed)
  X <- Xfun(n, d)
  Y0 <- rep(0, n)
  tau <- taufun(X)
  std <- sdfun(X)
  Y1 <- tau + std * errdist(n)
  ps <- psfun(X)
  T <- as.numeric(runif(n) < ps)
  Y <- Y0
  Y[T == 1] <- Y1[T == 1]
  Xtest <- Xfun(ntest, d)
  tautest <- taufun(Xtest)
  stdtest <- sdfun(Xtest)
  Y1test <- tautest + stdtest * errdist(n)
  pstest <- psfun(Xtest)

  res_tau <- data.frame()
  res_Y <- data.frame()
  res_cond <- data.frame()

  ## S-learner
  if (B > 0){
    CI <- try(estimator(X, Y, T, Xtest, B))
    if (class(CI) != "try-error"){
      df_tau <- summary_CI(tautest, CI$tau)
      df_tau <- data.frame(method = es_name,
                           cr = df_tau$cr,
                           len = df_tau$len)
      res_tau <- rbind(res_tau, df_tau)
    }
  }

  return(list(tau = res_tau, true_tau = tautest, est_tau = CI$preds))
}
