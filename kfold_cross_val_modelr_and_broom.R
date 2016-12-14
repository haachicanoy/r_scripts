# K-fold cross validation with modelr and broom
# Source: https://drsimonj.svbtle.com/k-fold-cross-validation-with-modelr-and-broom
# Implemented by: H. Achicanoy, 2016

# R options
options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(modelr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(broom))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))

# Create folds using mtcars data
set.seed(1)  # Run to replicate this post
folds <- crossv_kfold(mtcars, k = 5)
folds

folds$test[[1]]

folds$train[[1]]

# Fitting models to training data
lm(mpg ~ ., data = mtcars)

folds <- folds %>% mutate(model = map(train, ~ lm(mpg ~ ., data = .)))
folds
# folds %>% mutate(model = ...): Add a new column called "model" to the folds tibble
# map(train, ...): Apply a function to each of the cells in "train"
# ~ lm(...): regression model applied to each "train" cell
# data = .: specifies that the data for the regression model will be the data referenced by each "train" object

folds$model[[1]] %>% summary()

# Predicting the test data

folds %>% mutate(predicted = map2(model, test, <FUNCTION_TO_PREDICT_TEST_DATA> ))
folds %>% mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y)))

# Extract relevant information

folds %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>% 
  unnest(predicted)

predicted <- folds %>% unnest(map2(model, test, ~ augment(.x, newdata = .y)))
predicted

# Validating the model

# Compute the residuals
predicted <- predicted %>% 
  mutate(residual = .fitted - mpg)

# Plot actual v residual values
predicted %>%
  ggplot(aes(mpg, residual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  stat_smooth(method = "loess") +
  theme_bw()

rs <- predicted %>%
  group_by(.id) %>% 
  summarise(
    sst = sum((mpg - mean(mpg)) ^ 2), # Sum of Squares Total
    sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
    r.squared = 1 - sse / sst         # Proportion of variance accounted for
  )
rs

rs %>% 
  ggplot(aes(r.squared, fill  = .id)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(r.squared)))  # Overall mean

set.seed(1)
# Select four variables from the mpg data set in ggplot2
ggplot2::mpg %>% select(year, cyl, drv, hwy) %>% 
  # Create 20 folds (5% of the data in each partition)
  crossv_kfold(k = 20) %>%
  # Fit a model to training data
  mutate(model = map(train, ~ lm(hwy ~ ., data = .))) %>%
  # Unnest predicted values on test data
  unnest(map2(model, test, ~ augment(.x, newdata = .y))) %>% 
  # Compute R-squared values for each partition
  group_by(.id) %>%
  summarise(
    sst = sum((hwy - mean(hwy)) ^ 2),
    sse = sum((hwy - .fitted) ^ 2),
    r.squared = 1 - sse / sst
  ) %>% 
  # Plot
  ggplot(aes(r.squared)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(r.squared))) +
  theme_minimal()




