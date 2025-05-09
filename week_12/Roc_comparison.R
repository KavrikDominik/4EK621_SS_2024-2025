# Load required libraries
library(ISLR)
library(caret)
library(pROC)
library(randomForest)
library(xgboost)
library(e1071)

# Load the dataset
data("Default")
str(Default)

# Encode default as a factor
Default$default <- as.factor(Default$default)

# Set seed for reproducibility
set.seed(123)

# Create a train-test split
trainIndex <- createDataPartition(Default$default, p = 0.7, list = FALSE)
trainData <- Default[trainIndex, ]
testData <- Default[-trainIndex, ]

# Prepare data for XGBoost
xgb_train <- xgb.DMatrix(data = model.matrix(~. -1, data = trainData[, -1]), label = as.numeric(trainData$default) - 1)
xgb_test <- xgb.DMatrix(data = model.matrix(~. -1, data = testData[, -1]), label = as.numeric(testData$default) - 1)

# Logistic Regression
logit_model <- glm(default ~ ., data = trainData, family = binomial)
logit_pred <- predict(logit_model, newdata = testData, type = "response")
logit_roc <- roc(testData$default, logit_pred)

# Random Forest
rf_model <- randomForest(default ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, newdata = testData, type = "prob")[, 2]
rf_roc <- roc(testData$default, rf_pred)

# XGBoost
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  nthread = 2
)
xgb_model <- xgboost(data = xgb_train, params = xgb_params, nrounds = 100, verbose = 0)
xgb_pred <- predict(xgb_model, newdata = xgb_test)
xgb_roc <- roc(testData$default, xgb_pred)

# SVM
svm_model <- svm(default ~ ., data = trainData, probability = TRUE)
svm_pred <- predict(svm_model, newdata = testData, probability = TRUE)
svm_probs <- attr(svm_pred, "probabilities")[, "Yes"]
svm_roc <- roc(testData$default, svm_probs)

# Compare ROC Curves
plot(logit_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
plot(rf_roc, col = "red", add = TRUE, lwd = 2)
plot(xgb_roc, col = "green", add = TRUE, lwd = 2)
plot(svm_roc, col = "purple", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "XGBoost", "SVM"), 
       col = c("blue", "red", "green", "purple"), lwd = 2)


