# OLS from the scratch ----------------------------------------------------
wagedata <- wooldridge::wage2

# Standard approach -------------------------------------------------------

model_ols <- lm(wage ~ IQ + educ + exper, data = wagedata)
summary(model_ols)

# OLS manually from scratch -----------------------------------------------

y <- wagedata$wage
N <- nrow(wagedata)
X <- cbind(rep(1,N), wagedata$IQ, wagedata$educ, wagedata$exper)

# Matrix transpose: t()
# Matrix inversion: solve()

beta_hat <- solve( t(X) %*% X) %*% t(X) %*% y 

y_hat <- X %*% beta_hat

u_hat <- y - y_hat
k <- ncol(X) - 1
sigma_sq <- sum(   u_hat^2    ) / (N - k - 1)
vcov <- sigma_sq * solve(t(X) %*% X)
variances <- diag(vcov)
std_error <- sqrt(variances)
t_stat <- beta_hat / std_error

data.frame(beta_hat, std_error, t_stat)
model_ols
