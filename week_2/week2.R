# https://github.com/KavrikDominik/4EK621/blob/develop/week_2/week_2.ipynb

library(dplyr)
library(ggplot2)

data <- wooldridge::wage2
head(data)

data %>%
  ggplot(aes(y = wage, x = IQ)) +
  geom_point(colour = "dodgerblue2")

data %>%
  ggplot(aes(y = wage, x = IQ)) +
  geom_point(colour = "dodgerblue2") +
  stat_smooth(method = "lm", formula = y ~ x)

model.1 <- lm(wage ~ IQ, data)
summary(model.1)

# back to the real dataset
data %>%
  ggplot(aes(y = wage, x = IQ)) +
  geom_point(colour = "dodgerblue2") +
  stat_smooth(method = "lm", formula = y ~ x)

model.1 <- lm(wage ~ IQ, data)
summary(model.1)
fit <- fitted(model.1)

data %>%
  ggplot(aes(y = wage, x = IQ)) +
  geom_point(colour = "dodgerblue2") +
  geom_line(aes(y = fit))

# get closer to the ceteris paribus
model.2 <- lm(
  wage ~ IQ + educ + age + married + black + south + urban,
  data
)
summary(model.2)
# how to interpret all variables? how to interpret dummy variables?
# descriptive vs. causal vs. predictive interpretaion

# predict the wage for yourselve
coef(model.2)
my.wage <- coef(model.2)["(Intercept)"] +
  coef(model.2)["IQ"] * 138 +
  coef(model.2)["educ"] * 24 +
  coef(model.2)["age"] * 33 +
  coef(model.2)["married"] * 0 +
  coef(model.2)["black"] * 0 +
  coef(model.2)["south"] * 0 +
  coef(model.2)["urban"] * 1
unname(my.wage)

# vectorize
sum(coef(model.2) * c(1, 138, 24, 33, 0, 0, 0, 1))

# change the base category
data$single <- abs(data$married - 1)
head(data, 8)

model.4 <- lm(
  wage ~ IQ + educ + age + single + black + south + urban,
  data
)
summary(model.4)
stargazer::stargazer(model.2, model.4, type = "text", style = )

# change units
data$educM <- data$educ * 12
model.5 <- lm(
  wage ~ IQ + educM + age + single + black + south + urban,
  data
)
summary(model.5)
stargazer::stargazer(model.5, model.4, type = "text", style = )

# logaritmic transformation
model.3 <- lm(
  log(wage) ~ IQ + educ + age + married + black + south + urban,
  data
)
summary(model.3)

# approximative interpretation
approx <- coef(model.3)[-1] * 100
approx

# exact interpretation
exact <- (exp(coef(model.3))[-1] - 1) * 100
exact

data.frame(coef(model.3)[-1], approx, exact)
