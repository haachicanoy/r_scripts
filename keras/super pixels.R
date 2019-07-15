library(keras)
library(lime)
library(magick)

model <- application_vgg16(
  weights = "imagenet",
  include_top = TRUE
)
model

img <- image_read('https://www.data-imaginist.com/assets/images/kitten.jpg')
img_path <- file.path(tempdir(), 'kitten.jpg')
image_write(img, img_path)
plot(as.raster(img))

image_prep <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}
explainer <- lime(img_path, model, image_prep)

res <- predict(model, image_prep(img_path))
imagenet_decode_predictions(res)

dim(res)

model_labels <- readRDS(system.file('extdata', 'imagenet_labels.rds', package = 'lime'))
explainer <- lime(img_path, as_classifier(model, model_labels), image_prep)

# default
plot_superpixels(img_path)

# Changing some settings
plot_superpixels(img_path, n_superpixels = 200, weight = 40)

explanation <- explain(img_path, explainer, n_labels = 2, n_features = 20)
plot_image_explanation(explanation)

plot_image_explanation(explanation, threshold = 0, show_negative = TRUE, fill_alpha = 0.6)
