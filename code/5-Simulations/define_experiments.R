# Define list of different experimental setups.
# Each experiment consists of functions to generate X, tau, SD, propensity score,
# and the error distribution
experiment_list <- list()


## Exp 1 (equation (27) of Wager and Athey (2018)): homoscedastic errors + independent covariates
experiment_list[["Exp1"]] <- list(
  Xfun <- function(n, d){
    matrix(runif(n * d), nrow = n, ncol = d)
  },
  taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
  },
  sdfun <- function(X){
    rep(1, nrow(X))
  },
  psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
  },
  errdist <- rnorm
)

## Exp 2 (equation (27) of Wager and Athey (2018)) heteroscedastic errors + independent covariates
experiment_list[["Exp2"]] <- list(
  Xfun <- function(n, d){
    matrix(runif(n * d), nrow = n, ncol = d)
  },
  taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
  },
  sdfun <- function(X){
    -log(X[, 1] + 1e-9)
  },
  psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
  },
  errdist <- rnorm
)

## Expr 3 (equation (27) of Wager and Athey (2018)), homoscedastic errors + correlated covariates
experiment_list[["Exp3"]] <- list(
  Xfun <- function(n, d){
    rho <- 0.9
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    pnorm(X)
  },
  taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
  },
  sdfun <- function(X){
    rep(1, nrow(X))
  },
  psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
  },
  errdist <- rnorm
)


## Expr 4 (equation (27) of Wager and Athey (2018)) heteroscedastic errors + correlated covariates
experiment_list[["Exp4"]] <- list(
  Xfun <- function(n, d){
    rho <- 0.9
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    pnorm(X)
  },
  taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
  },
  sdfun <- function(X){
    -log(X[, 1] + 1e-9)
  },
  psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
  },
  errdist <- rnorm
)

