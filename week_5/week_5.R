library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(gganimate)

# Create the population data ----------------------------------------------
set.seed(27)
N <- 10^4 # N = population size.
pop <- data.frame(
  height = rnorm(N, 175, 10),
  u = rnorm(N, 0, 100)
) %>%
  mutate(wage = 5 + 10 * height + u) %>%
  mutate(irrelevant = rnorm(N, 10, 3))

# single sample
n <- 100

sample1 <- slice_sample(
  pop,
  n = 100,
  replace = TRUE
  ) # Choose 100 observations at random

model1 <- lm(wage ~ height, data = sample1)
summary(model1)

ggplot(aes(y = wage, x = height), data = pop) +
  geom_point(lwd = 0.5, alpha = 0.1) +
  geom_abline(intercept = 5, slope = 10, lwd = 1.5) +
  geom_point(aes(y = wage, x = height), data = sample1, colour = "red", shape = 2, lwd = 2) +
  geom_abline(intercept = coef(model1)[1], slope = coef(model1)[2], colour = "red2", lwd = 1.5)+
  theme_light()

# confidence intervals
confint(model1, level = 0.99) # 99% CI
confint(model1) # 95% CI

# manually
data.frame(
  LB = summary(model1)$coefficients[, 1] - qt(0.975, summary(model1)$df[2]) * summary(model1)$coefficients[, 2],
  UB = summary(model1)$coefficients[, 1] + qt(0.975, summary(model1)$df[2]) * summary(model1)$coefficients[, 2]
)

# approx.
qt(0.975, summary(model1)$df[2])
data.frame(
  LB = summary(model1)$coefficients[, 1] - 2 * summary(model1)$coefficients[, 2],
  UB = summary(model1)$coefficients[, 1] + 2 * summary(model1)$coefficients[, 2]
)


# resampling --------------------------------------------------------------
niter <- 200

lm_formula <- as.formula(wage ~ height)
lm_formula_with_irrelevant <-  as.formula(wage ~ height + irrelevant)
# Create nested tibble with R samples -------------------------------------
samples <- tibble(
  id = 1:niter,
  data = map(
    1:niter,
    ~sample_n(pop, n, replace = TRUE)
  )
)
samples # now we have 1000 different samples saved in one tibble

# Save data and coefficients for wage ~ height ----------------------------
coef_data_1 <- samples %>%
  mutate(
    models = map(data, ~ lm(lm_formula, data = .)),
    coefficients = map(models, broom::tidy)
  ) %>%
  select(id, data, coefficients) %>%
  unnest(coefficients) %>% 
  mutate(true_value = case_when(
    term == "(Intercept)" ~ 5,
    term == "height" ~ 10,
    term == "irrelevant" ~ 0
  )) 

# to see the result of a single regression just `filter()` by id
coef_data_1 %>%
  filter(id == 10)

# add 95% confidence interval:
coef_data_1 %>%
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>%
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>%
  ggplot(aes(x = estimate, y = id, color = term)) +
  geom_point(aes(x = estimate, color = term)) +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  geom_vline(aes(xintercept = true_value), linetype = "dashed") +
  facet_wrap(~term, scales = "free")

coef_data_1 %>%
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>%
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>%
  ggplot(aes(x = estimate, y = id, color = term)) +
  geom_point(aes(x = estimate, color = term)) +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~term, scales = "free")


# Save data with coefficients with irrelevant regressor -------------------
#
# create new object from a new model with irrelevant regressor
#
coef_data_model2 <- samples %>%
  mutate(
    models = map(data, ~ lm(wage ~ height + irrelevant, data = .)),
    coefficients = map(models, broom::tidy)
  ) %>%
  select(id, data, coefficients) %>%
  unnest(coefficients) %>% 
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>%
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>%
  mutate(true_value = case_when(
    term == "(Intercept)" ~ 5,
    term == "height" ~ 10,
    term == "irrelevant" ~ 0
  ))

coef_data_model2

n_viz <- 300

gg_ci <- coef_data_model2 %>%
  ggplot(aes(x = estimate, y = id, color = term)) +
  geom_point(aes(x = estimate, color = term)) +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  geom_vline(aes(xintercept = true_value), linetype = "dashed") +
  facet_wrap(~term, scales = "free") +
  labs(
    title = "Confidence intervals",
    subtitle = paste(n_viz / 3, "samples"),
    caption = "Dashed lines are the true values of population parameters"
  ) +
  theme_light()

gg_ci

# Lets investigate the false positives when testing significance o --------
t_false_positives <- coef_data_model2 %>%
  mutate(incl_zero = ifelse(lower <= 0 & upper >= 0, TRUE, FALSE)) %>%
  group_by(term) %>%
  summarise(
    freq_includes_zero = mean(incl_zero),
    freq_does_not_include_zero = mean(!incl_zero)
    )

t_false_positives

# ggplots of t-distributions ----------------------------------------------
t_dist <- coef_data_model2 %>%
  select(-data) %>%
  filter(!term == "(Intercept)") %>%
  select(id, term, statistic) %>%
  ggplot(aes(statistic, fill = term)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  stat_function(
    fun = dt, args = list(df = 98),
    aes(color = "t_distribution"),
    size = 1.1
  ) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red2", "dodgerblue3")) +
  theme_light() +
  labs(
    title = "Distribution of t-statistic for height and irrelevant",
    subtitle = "Null hypothesis: i-th parameter is insignificant; alpha = 0.05",
    caption = paste("Under the null hypothesis, t-statistic is distributed t [n-k-1]")
  )

t_dist

# Bonus animation ---------------------------------------------------------
#
# coef_data_model2 %>%
#   select(-data) %>%
#   filter(!term == "(Intercept)") %>%
#   select(id, term, statistic) %>% 
#   split(.$id) %>% 
#   accumulate(~bind_rows(.x,.y)) %>% 
#   bind_rows(.id="frame") %>% 
#   ggplot(aes(statistic, fill = term)) +
#   geom_histogram(aes(y = ..density..), color = "white") +
#   stat_function(
#     fun = dt, args = list(df = 98),
#     aes(color = "t_distribution"),
#     size = 1.1
#   ) +
#   scale_color_manual(values = c("black", "black", "black")) +
#   scale_fill_manual(values = c("red2", "dodgerblue3")) +
#   theme_light() +
#   labs(
#     title = "Distribution of t-statistic for height and irrelevant",
#     subtitle = "Null hypothesis: i-th parameter is insignificant; alpha = 0.05",
#     caption = paste("Under the null hypothesis, t-statistic is distributed t [n-k-1]")
#   ) +
#   transition_manual(factor(frame))+
#   ease_aes("linear")



