# https://stats.oarc.ucla.edu/r/dae/tobit-models/

options(scipen=10) 

# libraries
require(VGAM)
library(readr)
library(GGally)
library(ggplot2)

# data
dat <- read.csv("https://stats.idre.ucla.edu/stat/data/tobit.csv")
summary(dat)

# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = apt, fill=prog))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$apt))

p + stat_bin(binwidth = 1) + stat_function(fun = f, size = 1, args = list(var = dat$apt, 
                                                                          bw = 1))

# explore the bivariate relationships in our dataset
cor(dat[, c("read", "math", "apt")])

# plot matrix
ggpairs(dat[, c("read", "math", "apt")])

# run the tobit model, using the vglm function of the VGAM package
m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat)
summary(m)

# 95% confidence intervals for the coefficients
b <- coef(m)
se <- sqrt(diag(vcov(m)))
cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)
confint(m)
