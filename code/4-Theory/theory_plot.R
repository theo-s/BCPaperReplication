library(glmnet)

n <- 100
p <- 10

beta <- matrix(runif(p, -1,1), ncol = 1)

x <- matrix(runif(n*p), ncol = p)
y <- x %*% beta

y_train <- y + rnorm(n,sd = .5*sd(y))

# Simulate the asymptotic behavior of the randomized ensemble method
# using p = 10, S = 5, corresponding to a ridge regression with the
# penalty term lambda = .5
ridge <- glmnet(x, y_train, alpha = 0, lambda = c(.5))
ols <- lm(y_train ~., data = data.frame(x, y_train = y_train))


ols$coefficients
ridge$beta

p_ridge <- predict(ridge, newx = x)
p_ols <- unname(predict(ols, newx = x))

correc <- lm(y_train ~., data = data.frame(x = p_ridge,y_train = y_train))

p_correct <- unname(predict(correc, data.frame(x = p_ridge)))

newx <- matrix(runif(1000*p), ncol = p)
newy <- newx %*% beta

p_ridge <- predict(ridge, newx = newx)
p_correct <-

plot(newy, p_ridge)
plot(newy, p_correct)
