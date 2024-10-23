
library(tidyverse)
library(gtsummary)

# OES Equity Evaluation Series
# Choosing Controls in Regression Analyses Involving Equity
# https://oes.gsa.gov/assets/files/choosing-controls-in-regression-analyses-involving-equity.pdf

# Example 1: Post-attribute bias

# Create the dataset
data1 <- tibble(
  business_id = 1:6,
  women_owned = c(0, 0, 0, 1, 1, 1),
  earnings = c(50, 50, 50, 10, 10, 50),
  funded = c(1, 1, 1, 0, 0, 1)
)

# Full model
full_model1 <- lm(funded ~ women_owned + earnings, data = data1)
summary(full_model1)
tbl_regression(full_model1)

# Bivariate model
bivariate_model1 <- lm(funded ~ women_owned, data = data1)
summary(bivariate_model1)
tbl_regression(bivariate_model1)

# Example 2: Omitted variable bias

# Create the dataset
data2 <- tibble(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

# Full model
full_model2 <- lm(funded ~ black_owned + sole_prop, data = data2)
summary(full_model2)
tbl_regression(full_model2)


# Bivariate model
bivariate_model2 <- lm(funded ~ black_owned, data = data2)
summary(bivariate_model2)

# Example 3: Including uncorrelated variables

# Create the dataset
data3 <- data.frame(
  business_id = 1:8,
  black_owned = c(0, 0, 0, 0, 1, 1, 1, 1),
  sole_prop = c(0, 0, 1, 1, 0, 1, 1, 1),
  priority_zone = c(1, 0, 1, 0, 1, 0, 1, 0),
  funded = c(0, 0, 1, 1, 0, 1, 0, 1)
)

# Model with uncorrelated variable
model_uncorrelated <- lm(funded ~ black_owned + priority_zone, data = data3)
summary(model_uncorrelated)

# Model with all variables
model_all <- lm(funded ~ black_owned + sole_prop + priority_zone, data = data3)
summary(model_all)