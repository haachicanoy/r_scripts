# Deep learning for brand logo detection. Initial steps
# Taken from: http://flovv.github.io/Logo_detection_deep_learning/
# Implemented by: H. Achicanoy
# 2017

# Install dependencies
# devtools::install_github("rstudio/keras")
suppressMessages(library(keras))
# install_tensorflow()

model <- keras_model_sequential()
model %>%  ## we use a smaller filter!
  layer_conv_2d(filter = 16, kernel_size = c(3,3), input_shape = c(img_width, img_height, 3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filter = 64, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_flatten() %>%
  layer_dense(64) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(27) %>%    ## we have 27 brand logo classes!
  layer_activation("softmax")

hist <- model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)
