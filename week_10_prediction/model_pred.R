library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(stringr)

source("week_5/functions/data_functions.R")
options(scipen = 999)

data_folder_path <- "week_5/data/skoda_models"

full_data <- read.csv("week_7/full_data.csv")

colnames(full_data)

regressors <- c("age", "km", "model", "fuel", "combi", "gearbox_id")
full_data <- full_data %>% dplyr::select(any_of(regressors))
dependent_variable <- "price"

data <- full_data %>%
  select(any_of(c(dependent_variable, regressors))) %>%
  mutate(
    model = as.factor(model),
    fuel = as.factor(fuel),
    combi = as.factor(combi),
    gearbox_id = as.factor(gearbox_id),
  ) %>%
  na.omit() %>%
  filter(model == "superb") %>%
  mutate(
    diesel = ifelse(fuel == "diesel", 1, 0),
    automatic = ifelse(gearbox_id == 3, 1, 0),
    combi = as.numeric(combi)
  ) %>%
  mutate(
    diesel = ifelse(fuel == "diesel", 1, 0),
    automatic = ifelse(gearbox_id == 3, 1, 0),
    combi = as.numeric(combi)
  )


# filter only Superb model predict its price using age, km, combi, automatic, diesel
# create confidence interval for sum of coefficients
# make price prediction for Superb with 100 000 km, 10 years old, combi and diesel.
# make price prediction using model with log(price)
# - without correction
# - using normality assumption
# - using smearing estimator
