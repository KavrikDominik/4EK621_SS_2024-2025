library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- readr::read_csv("week_1/data/starbucks-menu-nutrition-food.csv",
                        locale = readr::locale(encoding = "UTF-16"))
colnames(data)

data <- data %>%
  rename(Product = ...1)

data %>% 
  head()

num_vars <- data %>%
  dplyr::select(where(is.numeric))

corrplot_input <- cor(num_vars) 

corrplot::corrplot(corrplot_input, method = "number")

data %>% 
  select(Calories, `Protein (g)`) %>% 
  ggplot()+
  geom_point(aes(`Protein (g)`, Calories))

data %>% 
  pivot_longer(names_to = "variable",
               values_to = "value", 3:ncol(.)) %>% 
  ggplot(aes(value, Calories)) +
  geom_point(aes(colour = variable))+
  facet_wrap(~variable, scales = "fixed")