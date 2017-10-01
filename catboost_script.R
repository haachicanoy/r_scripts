library(catboost)

# Load the dataset from the CatBoost R package (this dataset is a subset of the Adult Data Set distributed through the UCI Machine Learning Repository)
pool_path <- system.file("extdata", "adult_train.1000", package = "catboost")
cd_path <- system.file("extdata", "adult.cd", package = "catboost")
pool <- catboost.load_pool(pool_path, cd_path)

# Train the model
fit_params <- list(iterations = 100, thread_count = 10, loss_function = 'Logloss')
model <- catboost.train(pool, pool, fit_params)

# Apply the model
prediction <- catboost.predict(model, pool)
head(prediction)
