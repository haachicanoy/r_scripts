# Deep learning for brand logo detection. Processing data
# Taken from: http://flovv.github.io/Logo_detection_deep_learning/
# Implemented by: H. Achicanoy
# 2017

########## data proprocessing!
## download the file set from here: http://image.ntua.gr/iva/datasets/flickr_logos/
## extract image to a folder flickrData
## copy files in the right directories

options(stringsAsFactors = F)
df <- read.csv("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/flickr_logos_27_dataset_query_set_annotation.txt", sep="\t", header = F)
colnames(df) <- c("file", "label")

for( i in unique(df$label)){
  dir.create(paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/train/", i), recursive = T)
  dir.create(paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/test/", i), recursive = T)
}

train <-  sample(df$file, 200)
test <- df[!df$file %in% train,]$file

### copy files

for(i in train){
  label <- df[df$file == i,]$label
  file.copy(paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/flickr_logos_27_dataset_images/", i), paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/train/", label, "/", i))
}

for(i in test){
  label <- df[df$file == i,]$label
  file.copy(paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/flickr_logos_27_dataset_images/", i), paste0("/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/test/", label, "/", i))
}

#### the KERAS part
# devtools::install_github("rstudio/keras")
suppressMessages(library(keras))

# install_tensorflow()

### setting up the model

img_width <- 64
img_height <- 64
batch_size <- 64

train_directory <- "/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/train"
test_directory <- "/Users/haachicanoy/Documents/Datasets/flickr_logos_27_dataset/test"

train_generator <- flow_images_from_directory(train_directory, generator = image_data_generator(),
                                              target_size = c(img_width, img_height), color_mode = "rgb",
                                              class_mode = "categorical", batch_size = batch_size, shuffle = TRUE,
                                              seed = 123)

validation_generator <- flow_images_from_directory(test_directory, generator = image_data_generator(),
                                                   target_size = c(img_width, img_height), color_mode = "rgb", classes = NULL,
                                                   class_mode = "categorical", batch_size = batch_size, shuffle = TRUE,
                                                   seed = 123)

model <- keras_model_sequential()
model %>%
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
  layer_dense(27) %>%    ## we have 27 classes!
  layer_activation("softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

train_samples <- 200
validation_samples <- 70

hist <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = as.integer(train_samples/batch_size), 
  epochs = 200, 
  validation_data = validation_generator,
  validation_steps = as.integer(validation_samples/batch_size),
  verbose=2
)

#############################


##############
## lets create a nice plot!

hist <- data.frame(acc = unlist(hist$history$acc), val_acc=unlist(hist$history$val_acc), val_loss = unlist(hist$history$val_loss),loss = unlist(hist$history$loss))
colnames(hist) <- c("Training Accuracy", "Validation Accuracy" , "Validation Loss", "Training Loss")
hist$Epoch <- 1:200
m <- melt(hist, id.vars = "Epoch")
ggplot(m, aes(epoch, value)) + facet_wrap(~variable, as.table = T, scales = "free" ) + geom_point() + geom_smooth() + ylab("") + theme_light()

########
## evaluation:
evaluate_generator(model,validation_generator, validation_samples)

##################

# extract predictions!  - out of sample - simple example

img_path <- "https://logorealm.com/wp-content/uploads/2017/02/adidas-trefoil-logo.png"
img_path  <- "http://www.logolook.de/wp-content/uploads/bmw-propeller-logo.jpg"
download.file(img_path,'test.jpg', mode = 'wb')

img <- image_load('test.jpg', target_size = c(64,64))
x <- image_to_array(img)
dim(x) <- c(1, dim(x)) 
prediction <- model %>% predict(x)

colnames(prediction) <- unique(df$label)[1:27]
prediction[,which.max(prediction)]

#############