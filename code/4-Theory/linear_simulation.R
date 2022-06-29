library(mvtnorm)
library(ggplot2)
library(dplyr)

ro = .3
beta_true <- matrix(c(1/sqrt(3),-1/sqrt(3),1/sqrt(3)), ncol = 1)
sigma=matrix(c(1,0,0,0,1,ro,0,ro,1), ncol = 3)
x <- rmvnorm(10000, mean = rep(0,3), sigma = sigma)

subset_bias_square <- function(x, beta_true) {
  return((-.3333*x[1]*beta_true[1,1] +
            x[2]*(-.3333*beta_true[2,1] + ro*.5*beta_true[3,1]) +
            x[3]*(-.3333*beta_true[3,1] + ro*.5*beta_true[2,1]) )**2)
}

evaled <- (apply(x, MARGIN = 1, function(k){return(subset_bias_square(k,beta_true))}))
mean(evaled)

reg_func <- function(x, beta_true) {
  return(.6666*(x[1]*beta_true[1,1] +
                x[2]*(beta_true[2,1] + ro*.5*beta_true[3,1]) +
                x[3]*(beta_true[3,1] + ro*.5*beta_true[2,1])))
}

# Now get bias corrected preds
y_reg <- (apply(x, MARGIN = 1, function(k){return(reg_func(k,beta_true))}))
y_true <- (x %*% beta_true)[,1]

print(lm(y_true ~., data.frame(y_true = y_true, x)))

hat_rand <- matrix(.66666*c(beta_true[1,1], beta_true[2,1] + ro*.5*beta_true[3,1], beta_true[3,1] + ro*.5*beta_true[2,1]), ncol = 1)
print(cov(beta_true, hat_rand)/ var(hat_rand))

print(lm(y_true ~., data = data.frame(y_reg, y_true)))
correct_factor <- unname(lm(y_true ~., data = data.frame(y_reg, y_true))$coefficients[2])

cov(y_reg, y_true)/var(y_reg)


print("Ensemble bias")
mean(evaled)
print("Ensemble corrected bias")
print(mean((correct_factor*y_reg - y_true)**2))


data.frame(Y_pred = y_reg, Y_true = y_true) %>%
  ggplot(aes(x = Y_pred, y = Y_true))+
  geom_point()+
  theme_classic()+
  geom_abline(slope = 1, intercept = c(0,0))


# Check that this is the right slope
(t(hat_rand) %*% sigma %*% beta_true) / (t(hat_rand) %*% sigma %*% hat_rand)

