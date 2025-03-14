# R-squared and random regressors -----------------------------------------
library(dplyr)
set.seed(155)
N <- 100
const <- cbind(rep(1, N)) # constant vector
X <- cbind(const) # design matrix containing only 
Y <- rnorm(N, 2, 2) # dependent variable
# initialization
C <- c()
D <- c()
for (i in 1:99) {
  xi <- rnorm(N, 1, 2) # create new regressor
  X <- cbind(X, xi) # add it to design matrix
  k <- ncol(X) - 1 # k...nr. of regressors (without constant)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  Y_hat <- X %*% beta
  SSR <- sum((Y_hat - Y)^2)
  SST <- sum((Y - mean(Y))^2)
  SSE <- SST - SSR
  R_squared <- SSE / SST
  R_adj <- 1 - (((1 - R_squared) * (N - 1)) / (N - k - 1))
  print(i)
  C[i] <- R_squared
  D[i] <- R_adj
  Sys.sleep(0.05)
  plot.ts(C,
          col = "red",
          ylab = "R Squared",
          xlab = NULL,
          xlim = c(0,100),
          ylim = c(0,1),
          main = paste("Number of regressors", i)
  )
  Sys.sleep(0.05)
}





