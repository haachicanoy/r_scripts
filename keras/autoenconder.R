# Autoencoder

library(tidyverse)
df <- read_csv("C:/Users/haachicanoy/Downloads/creditcard.csv", col_types = list(Time = col_number()))

library(ggridges)
df %>%
  gather(variable, value, -Class) %>%
  ggplot(aes(y = as.factor(variable), 
             fill = as.factor(Class), 
             x = percent_rank(value))) +
  geom_density_ridges()

df_train <- df %>% filter(row_number(Time) <= 200000) %>% select(-Time)
df_test <- df %>% filter(row_number(Time) > 200000) %>% select(-Time)

library(purrr)

#' Gets descriptive statistics for every variable in the dataset.
get_desc <- function(x) {
  map(x, ~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
} 

#' Given a dataset and normalization constants it will create a min-max normalized
#' version of the dataset.
normalization_minmax <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
}

desc <- df_train %>% 
  select(-Class) %>% 
  get_desc()

x_train <- df_train %>%
  select(-Class) %>%
  normalization_minmax(desc) %>%
  as.matrix()

x_test <- df_test %>%
  select(-Class) %>%
  normalization_minmax(desc) %>%
  as.matrix()

y_train <- df_train$Class
y_test <- df_test$Class

library(keras)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = 15, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))

summary(model)

model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)

checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5", 
  save_best_only = TRUE, 
  period = 1,
  verbose = 1
)

early_stopping <- callback_early_stopping(patience = 5)

model %>% fit(
  x = x_train[y_train == 0,], 
  y = x_train[y_train == 0,], 
  epochs = 100, 
  batch_size = 32,
  validation_data = list(x_test[y_test == 0,], x_test[y_test == 0,]), 
  callbacks = list(checkpoint, early_stopping)
)

loss <- evaluate(model, x = x_test[y_test == 0,], y = x_test[y_test == 0,])
loss

FLAGS <- flags(
  flag_string("normalization", "minmax", "One of minmax, zscore"),
  flag_string("activation", "relu", "One of relu, selu, tanh, sigmoid"),
  flag_numeric("learning_rate", 0.001, "Optimizer Learning Rate"),
  flag_integer("hidden_size", 15, "The hidden layer size")
)

model %>% compile(
  optimizer = optimizer_adam(lr = FLAGS$learning_rate), 
  loss = 'mean_squared_error',
)

library(cloudml)
