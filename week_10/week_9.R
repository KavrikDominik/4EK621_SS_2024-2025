library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(whitestrap)
library(sandwich)
library(stats)

# read data
data <- read.csv("week_9/HeterWage.csv")

# OLS ---------------------------------------------------------------------

model_ols <- lm(wage ~ educ + IQ + expert, data = data)

summary(model_ols)

# Test for heteroskedasticity ---------------------------------------------
bptest(model_ols)

white_test(model_ols)


# WLS manually ------------------------------------------------------------
weight <-  1/exp(data$educ)

data_w <- data %>% 
  mutate(
    const_w = rep(1, nrow(.)) * sqrt(weight),
    IQ_w = IQ * sqrt(weight),
    educ_w = educ * sqrt(weight),
    wage_w = wage * sqrt(weight),
    expert_w = expert * sqrt(weight),
  )

model_wls_manually <- lm(wage_w ~ 0 + const_w + educ_w + IQ_w + expert_w, data = data_w)
summary(model_wls_manually)

# WLS ---------------------------------------------------------------------

weight <-  1/exp(data$educ)

model_wls <- glm(wage ~ educ + IQ + expert, weights = weight, data = data)
summary(model_wls)

# FGLS --------------------------------------------------------------------

model_step1 <- lm(wage ~ educ + IQ + expert, data = data)
residuals_squared <- (model_step1$residuals)^2

model_aux <- lm(log(residuals_squared) ~ educ + IQ + expert, data=data)

fitted_values_aux <- model_aux$fitted.values

weight_fgls <- 1/exp(fitted_values_aux)

model_fgls <- glm(wage ~ educ + IQ + expert, weights = weight_fgls, data = data)

summary(model_fgls)


# comparison --------------------------------------------------------------

# comparison
stargazer(model_ols,
          model_wls,
          model_fgls,
          model_wls_manually,
          column.labels = c("OLS", "WLS", "FGLS", "Manual WLS"),
          digits=3,
          type="text"
)



# Robust standard errors --------------------------------------------------


vcov_ols <- vcov(model_ols)
vcov_hc1 <- sandwich::vcovHC(model_ols, type="HC1")  


# Extract standard error from VCE
se_ols <- sqrt(diag(vcov_ols))
se_hc1 <- sqrt(diag(vcov_hc1))


# bootstrap SE

n <- nobs(model_ols)
k <- model_ols$rank 
B <- 500 # number of replications

bootstrap_coefficients <- data.frame(
  "(Intercept)" = NA,
  "educ" = NA,
  "IQ" = NA,
  "expert" = NA
  ) # store estimates from the bootstrapping

# bootstrapping
for (b in 1:B) {
  model <- lm(wage ~ educ + IQ + expert, data, 
                 subset = sample(n, n, replace = TRUE)) 
  bootstrap_coefficients[b, ] <- model$coef 
}

# bootstrap VCE 
coeff_cov_bootstrap <- cov(bootstrap_coefficients)
# Extract standard error from VCE
se_bootstrap <- sqrt(diag(coeff_cov_bootstrap))
# Compare standard errors
round(rbind(se_ols, se_hc1, se_bootstrap), digits = 6)

