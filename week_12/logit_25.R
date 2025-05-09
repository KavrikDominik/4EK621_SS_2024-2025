library(dplyr)
library(sandwich)
library(stargazer)
library(tidyr)
library(caret)
library(pROC)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- read.csv("week_12/data/credit_scoring.csv")

data <- data[complete.cases(data),] # removes observation with NAs

data <- data %>% 
  mutate(deliq = ifelse(`NumberOfTime30-59DaysPastDueNotWorse` > 0, 1, 0))

equation <- as.formula(SeriousDlqin2yrs ~ age + MonthlyIncome + deliq)

descriptive_statistics <- summary(data)


# Test train split --------------------------------------------------------

set.seed(123) # For reproducibility
splits <- data %>%
  modelr::resample_partition(c(train = 0.7, test = 0.3))

train_data <- as_tibble(splits$train)
test_data <- as_tibble(splits$test)


# Equation ----------------------------------------------------------------

equation <- as.formula(deliq ~ age + MonthlyIncome)

# Coefficient estimates
OLS <- lm(equation, data = train_data)

# LPM has by definition heteroskedastic errors
OLS_robust_VCE <- vcovHC(OLS, type = "HC1")  # Robust variance matrix
OLS_robust_se <- sqrt(diag(OLS_robust_VCE))  # Robust std. errors

# Let's compare
stargazer(OLS, OLS, 
          column.labels = c("OLS", "OLS rob. SE"), 
          se            = list(NULL, OLS_robust_se), 
          digits        = 4,
          type          = "text"
)


# Logit -------------------------------------------------------------------


logit <- glm(equation, data = train_data, family = binomial(link = 'logit'))
summary(logit)

# Standard CIs
confint.default(logit)

# CI based na "profiled log-likelihood" (computationally intensive, better in small samples)
confint(logit)

# Lets exponentiate the coefficients
exp(logit$coeff)

# Odds ratios and their 95% CIs
exp(cbind(exp.coeff=coef(logit), confint.default(logit)))

# probit ---------------------------------------------------------------------

probit <- glm(equation, data = train_data, family = binomial(link=probit))
summary(probit)

# Compare
stargazer(OLS, OLS, logit, probit,
          column.labels = c("OLS", "OLS robust", "Logit", "Probit"),
          se            = list(NULL, OLS_robust_se, NULL, NULL),
          keep.stat     = c("n"),
          digits        = 4,
          type          = "text"
)




# Logit MLE from the scratch ----------------------------------------------

neg_log_likelihood_logistic <- function(beta, X, y) {
  z <- as.matrix(cbind(1, X)) %*% beta  # Calculate linear predictors including intercept
  log_likelihood <- sum(y * z - log(1 + exp(z)))  # Log-likelihood
  return(-log_likelihood)  # Return negative log-likelihood for minimization
}

# Initial guesses for parameters (intercept and slopes)
initial_values <- c("Intercept"=0,
                    "age"=0,
                    "MonthlyIncome"=0)  # Intercept and two slopes

X = as.matrix(
        data[,c("age", "MonthlyIncome")]
    )


# Minimize the negative log-likelihood using 'optim'
opt_result_logistic <- optim(
  initial_values,
  neg_log_likelihood_logistic,
  X=X, 
  y=data$SeriousDlqin2yrs)

# Display the estimated parameters (log-odds)
opt_result_logistic$par
summary(logit)


# Create predictions ------------------------------------------------------

lpm_prob <- predict(OLS, newdata = test_data)
logit_prob <- predict(logit, newdata = test_data, type = "response")

threshold <- 0.5

# Convert probabilities to binary predictions
lpm_pred <- ifelse(lpm_prob > threshold, 1, 0)
logit_pred <- ifelse(logit_prob > threshold, 1, 0)

# Actual target values
actual_values <- test_data$deliq

# Confusion matrices
lpm_cm <- caret::confusionMatrix(as.factor(lpm_pred), as.factor(actual_values))
logit_cm <- caret::confusionMatrix(as.factor(logit_pred), as.factor(actual_values))

# Model evaluation metrics
lpm_metrics <- lpm_cm$overall
logit_metrics <- logit_cm$overall

# ROC curve ---------------------------------------------------------------
#The Receiver Operating Characteristic (ROC)


lpm_roc <- roc(response = as.numeric(actual_values), predictor = lpm_prob)
logit_roc <- roc(response = as.numeric(actual_values), predictor = logit_prob)

lpm_auc <- auc(lpm_roc)
logit_auc <- auc(logit_roc)


# Plot ROC curves
plot(logit_roc, main = "ROC Curves", col = "blue", lwd = 2)
lines(lpm_roc, col = "red", lwd = 2)
legend("bottomright", legend = c("Logit", "LPM"), col = c("blue", "red"), lwd = 2)

# Print AUC-ROC values
cat("LPM AUC-ROC:", lpm_auc, "\n")
cat("Logit AUC-ROC:", logit_auc, "\n")
# AUC of 0.5 says the model is no better then distinguishing betweeen the classes by chance

