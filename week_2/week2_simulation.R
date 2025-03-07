library(dplyr)
library(ggplot2)

makeData <- function(N, beta_0, beta_1, seed = 123) {
  set.seed(seed)
  x <- rnorm(N, 13, 3)
  y <- beta_0 + beta_1 * x + rnorm(N, 0, 300)
  DF <- data.frame(y, x)
  return(DF)
}

beta_0 <- 50
beta_1 <- 100
N <- 10000


population <- makeData(N, beta_0, beta_1)

population %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_function(
    fun = function(x) {
      beta_0 + beta_1 * x
    },
    color = "blue",
    linetype = "dashed"
  ) +
  theme_light()

sample <- sample_n(population, 100)

population %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_point(aes(x, y),
    data = sample,
    color = "red",
    shape = 2
  ) +
  theme_light()

lm(y ~ x, data = sample)


tibble(
  sample_id = 1:1000,
  data = purrr::map(sample_id, ~ sample_n(population, 100, replace = F))
) %>%
  mutate(model = purrr::map(data, ~ lm(y ~ x, data = .))) %>%
  mutate(coeffs = purrr::map(model, broom::tidy)) %>%
  select(sample_id, coeffs) %>%
  tidyr::unnest(cols = coeffs) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_histogram(color = "white") +
  facet_wrap(~term, scales = "free") +
  theme_light()
