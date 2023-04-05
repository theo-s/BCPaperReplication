# Defines a list of experiments from the breiman debiasing paper and a high dimensional
# linear example.
experiment_list <- list()

## Exp 1
experiment_list[["Exp1"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    r <- 3*runif(n,min=0,max=1)
    x <- matrix(rep(0, n * 20), nrow = n, ncol = 20)

    for (i in 1:nrow(x)) {
      x[i,] <- runif(20, min = 0, max = r[i])
    }

    y_true <- 25*exp(-.5*r^2)
    return(list("x" = x, "r" = r, "y_true" = y_true))
  },
  Yfun <- function(data, seed, snr=1){
    r <- data$r
    return(25*exp(-.5*r^2))
  }
)

## Exp 2
experiment_list[["Exp2"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    x <- matrix(runif(10 * n), nrow = n, ncol = 10)
    x <- as.data.frame(x)

    y_true <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5]
    return(list("x" = x, "y_true" = y_true))
  },
  Yfun <- function(data, seed, snr=24) {
    X <- data$x
    set.seed(seed)

    y <- 10*sin(pi*X[,1]*X[,2]) + 20*(X[,3] - .5)^2 + 10*X[,4] + 5*X[,5]

    noise <- rnorm(length(y))
    ratio <- sqrt(var(y)/(snr*var(noise)))

    y <- y + ratio * noise
    return(y)
  }
)

## Expr 3
experiment_list[["Exp3"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    x1 <- runif(n, min = 0, max = 100)
    x2 <- runif(n, min = 40*pi, max = 560*pi)
    x3 <- runif(n, min = 0.01, max = 1)
    x4 <- runif(n, min = 1, max = 11)

    x <- data.frame(x1, x2, x3, x4)

    y_true <- x[, 1] ^ 2 + (x[, 2] * x[, 3] - (1 / (x[, 2] * x[, 3])) ^ 2) ^ (.5)
    return(list("x" = x, "y_true" = y_true))
  },
  Yfun <- function(data, seed,snr=3) {
    X <- data$x
    set.seed(seed)
    y <- X[, 1] ^ 2 + (X[, 2] * X[, 3] - (1 / (X[, 2] * X[, 3])) ^ 2) ^ (.5)

    noise <- rnorm(length(y))
    ratio <- sqrt(var(y)/(snr*var(noise)))
    y <- y + ratio * noise
    return(y)
  }
)

## Expr 4
experiment_list[["Exp4"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    x1 <- runif(n, min = 0, max = 100)
    x2 <- runif(n, min = 40*pi, max = 560*pi)
    x3 <- runif(n, min = 0.01, max = 1)
    x4 <- runif(n, min = 1, max = 11)

    x <- data.frame(x1, x2, x3, x4)

    y_true <- atan( (x[,2]*x[,3] - (1/(x[,2]*x[,4]) )) / x[,1])
    return(list("x" = x,"y_true" = y_true))
  },
  Yfun <- function(data, seed, snr=3) {
    X <- data$x
    set.seed(seed)

    y <- atan( (X[,2]*X[,3] - (1/(X[,2]*X[,4]) )) / X[,1])

    noise <- rnorm(length(y))
    ratio <- sqrt(var(y)/(snr*var(noise)))
    y <- y + ratio * noise
    return(y)
  }
)

## Expr 5
experiment_list[["Exp5"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    x <- matrix(rnorm(n*100), ncol = 100)
    beta <- c(
      c(0.09873424, 0.46945549, 0.28703703, 0.26507412, 0.88071903,
        0.58478414, 0.48429319, 0.07180815, 0.62477804, 0.02620922,
        0.53150494, 0.41295260, 0.08861640, 0.72412665, 0.91862325,
        0.3063318, 0.42222575, 0.14834474, 0.42306006, 0.77476412),
        rep(0,80)
    )

    y_true <- as.vector(as.matrix(x) %*% beta)
    return(list("x" = x, "y_true" = y_true))
  },
  Yfun <- function(data, seed,snr=3) {
    X <- data$x
    set.seed(seed)

    beta <- c(
      c(0.09873424, 0.46945549, 0.28703703, 0.26507412, 0.88071903,
        0.58478414, 0.48429319, 0.07180815, 0.62477804, 0.02620922,
        0.53150494, 0.41295260, 0.08861640, 0.72412665, 0.91862325,
        0.3063318, 0.42222575, 0.14834474, 0.42306006, 0.77476412),
      rep(0,80)
    )

    y <- as.vector(as.matrix(X) %*% beta)

    noise <- rnorm(length(y))
    ratio <- sqrt(var(y)/(snr*var(noise)))
    y <- y + ratio * noise
    return(y)
  }
)

experiment_list[["Exp6"]] <- list(
  Xfun <- function(n, seed){
    set.seed(seed)
    p <- 10
    x_test <- matrix(runif(n*p), ncol = p)
    y_true <- sin(x_test[,1]*2*pi)
    data_test <- data.frame(x_test)

    return(list("x" = data_test, "y_true" = y_true))
  },
  Yfun <- function(data, seed, snr=2) {
    X <- data$x
    y <- sin(X[,1]*2*pi)

    noise <- rnorm(length(y))
    ratio <- sqrt(var(y)/(snr*var(noise)))
    y <- y + ratio * noise
    return(y)
  }
)
