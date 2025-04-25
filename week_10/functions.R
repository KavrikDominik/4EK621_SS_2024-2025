generateHeteroskedasticPopulation <- function(N) {
  IQ <- rnorm(N, 100, 5)
  expert <- as.numeric(runif(N, min = 0, max = 30))
  educ <- as.numeric(runif(N, min = 9, max = 20))
  sigma <- 2 * (exp(educ))^0.5 # tvar podmíneného sd, rozptyl závisí na jednom z regresoru
  e4 <- rnorm(N, 0, sigma)
  wage <- -50 + 10 * IQ + 50 * educ + 3 * expert + e4
  population <- data.frame(IQ, expert, educ, wage)

  return(population)
}

generateSamplingDistributionsData <- function(population, n_replications) {
  A <- list(tibble::tibble(NA))
  R <- n_replications

  for (i in 1:R) {
    A[[i]] <- dplyr::slice_sample(population, n = R)
  }

  samples <- tibble(vzorek = A)

  sampling_distributions <- samples %>%
    mutate(
      model = purrr::map(vzorek, .f = ~ lm(wage ~ educ + expert + IQ, data = .)),
      coeffs = purrr::map(model, .f = broom::tidy)
    ) %>%
    tidyr::unnest(coeffs) %>%
    mutate(true_val = case_when(
      term == "(Intercept)" ~ -50,
      term == "IQ" ~ 10,
      term == "educ" ~ 50,
      term == "expert" ~ 3
    ))

  return(sampling_distributions)
}


generateSamplingDistributionsData <- function(population, n_replications, sample_size) {
  A <- list(tibble::tibble(NA))
  R <- n_replications

  for (i in 1:R) {
    A[[i]] <- dplyr::slice_sample(population, n = sample_size)
  }

  samples <- tibble(vzorek = A)

  sampling_distributions <- samples %>%
    mutate(
      model = purrr::map(vzorek, .f = ~ lm(wage ~ educ + expert + IQ, data = .)),
      coeffs = purrr::map(model, .f = broom::tidy)
    ) %>%
    tidyr::unnest(coeffs) %>%
    mutate(true_val = case_when(
      term == "(Intercept)" ~ -50,
      term == "IQ" ~ 10,
      term == "educ" ~ 50,
      term == "expert" ~ 3
    ))

  return(sampling_distributions)
}




generateSamplingDistributionsDataWithWLS <- function(population, n_replications, sample_size) {
  A <- list(tibble::tibble(NA))
  R <- n_replications

  for (i in 1:R) {
    A[[i]] <- dplyr::slice_sample(population, n = sample_size)
  }

  samples <- tibble(vzorek = A)

  sampling_distributions_wls <- samples %>%
    mutate(
      model_wls = purrr::map(vzorek, .f = ~ lm(wage ~ educ + expert + IQ, weights = (1 / exp(educ)^0.5), data = .)),
      coeffs_wls = purrr::map(model_wls, .f = broom::tidy)
    ) %>%
    tidyr::unnest(coeffs_wls) %>%
    mutate(true_val = case_when(
      term == "(Intercept)" ~ -50,
      term == "IQ" ~ 10,
      term == "educ" ~ 50,
      term == "expert" ~ 3
    ))

  return(sampling_distributions_wls)
}
